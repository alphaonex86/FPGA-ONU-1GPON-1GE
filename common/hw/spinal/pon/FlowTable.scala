// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.lib._

/** A flow's key: IPv4 (addressBits 32) 127 bits, IPv6 (128) 319; asBits: dport in bits 15-0 up to tagged last. */
case class FlowKey(addressBits: Int = 32) extends Bundle {
  val dport, sport = UInt(16 bits)
  val dst, src = Bits(addressBits bits)
  val udp = Bool() // else TCP
  val session = UInt(16 bits) // PPPoE session, 0 without PPPoE
  val pppoe = Bool()
  val vid = UInt(12 bits) // outer VID, 0 untagged
  val tagged = Bool()
}

/** NAPT of a flow: its source (else destination) address and port replaced; egress: how it leaves; counter: the
  * FlowClassifier counter it adds to (1-63, 0 none). */
case class FlowAction() extends Bundle {
  val natSrc = Bool()
  val ip = Bits(32 bits)
  val port = UInt(16 bits)
  val egress = UInt(6 bits)
  val counter = UInt(6 bits)
}

case class FlowEntry(addressBits: Int) extends Bundle {
  val valid = Bool()
  val key = FlowKey(addressBits)
  val action = FlowAction()
}

case class FlowWrite(bucketBits: Int, addressBits: Int = 32) extends Bundle {
  val bucket = UInt(bucketBits bits)
  val way = UInt(1 bits)
  val entry = FlowEntry(addressBits)
}

case class FlowResult(bucketBits: Int) extends Bundle {
  val hit = Bool()
  val action = FlowAction()
  val index = UInt(bucketBits + 1 bits) // bucket ## way of the hit
}

object FlowTable {
  /** CRC-16-CCITT (0x1021, init 0xFFFF) of the key padded with zeros to whole bytes, its last bit first: the bucket
    * is its low bits. */
  def hash(key: Bits): Bits = { // affine in the key: each bit the parity of the key bits that flip it (flat XORs)
    val width = key.getWidth
    val zero = model(0, width)
    val flips = (0 until width).map(i => model(BigInt(1) << i, width) ^ zero)
    Vec((0 until 16).map(j => (0 until width).filter(i => (flips(i) >> j & 1) == 1).map(key(_))
      .fold(Bool((zero >> j & 1) == 1))(_ ^ _))).asBits
  }
  def model(key: BigInt, width: Int = 127): Int = (((width + 7) / 8 * 8 - 1) to 0 by -1).foldLeft(0xFFFF) { (c, i) =>
    val bit = (key >> i & 1).toInt ^ (c >> 15)
    ((c << 1) & 0xFFFF) ^ (if (bit == 1) 0x1021 else 0)
  }
}

/** Flow cache of one direction: 2^bucketBits buckets of 2 ways in one block RAM (bucket ## way: the IPv6 entries
  * fill an M20K's width, not its depth), written whole by the CPU. A lookup, one at a time (the next after the
  * answer: frames are >= 64 clocks apart), reads way 0 then way 1 through one comparator and answers 6 clocks later (hash, RAM, compare each registered: 311.04 MHz), a
  * hit when a valid way's key matches. */
case class FlowTable(bucketBits: Int = 10, addressBits: Int = 32) extends Component {
  val io = new Bundle {
    val lookup = slave Flow (FlowKey(addressBits))
    val result = master Flow (FlowResult(bucketBits))
    val write = slave Flow (FlowWrite(bucketBits, addressBits))
  }
  val entries = Mem(FlowEntry(addressBits), 2 << bucketBits) initBigInt (Seq.fill(2 << bucketBits)(BigInt(0)))
  entries.write(io.write.bucket @@ io.write.way, io.write.entry, io.write.valid)

  val first = RegNext(io.lookup.valid) init (False) // way 0 read
  val second = RegNext(first) init (False) // way 1 read
  val key = RegNextWhen(io.lookup.payload, io.lookup.valid)
  val bucket = RegNextWhen(FlowTable.hash(io.lookup.payload.asBits)(bucketBits - 1 downto 0).asUInt, io.lookup.valid)
  val read = RegNext(entries.readSync(bucket @@ second.asUInt, first || second)) // registered RAM output
  val hit = RegNext(read.valid && read.key === key) // way 0 when Delay(first, 3), way 1 a clock later
  val action = RegNext(read.action)
  val hit0 = RegNext(hit)
  val action0 = RegNext(action)
  io.result.valid := RegNext(Delay(second, 3, init = False)) init (False)
  io.result.hit := RegNext(hit0 || hit)
  io.result.action := RegNext(hit0 ? action0 | action)
  io.result.index := RegNext(bucket @@ !hit0)
}
