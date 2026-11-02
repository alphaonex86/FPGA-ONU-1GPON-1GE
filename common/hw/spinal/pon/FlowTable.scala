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

/** NAPT of a flow: its source (else destination) address and port replaced; egress: how it leaves. */
case class FlowAction() extends Bundle {
  val natSrc = Bool()
  val ip = Bits(32 bits)
  val port = UInt(16 bits)
  val egress = UInt(6 bits)
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

/** Flow cache of one direction: 2^bucketBits buckets of 2 ways in block RAM, written whole by the CPU. A lookup
  * answers 5 clocks later (hash, RAM, compare each registered: 311.04 MHz), a hit when a valid way's key matches. */
case class FlowTable(bucketBits: Int = 10, addressBits: Int = 32) extends Component {
  val io = new Bundle {
    val lookup = slave Flow (FlowKey(addressBits))
    val result = master Flow (FlowResult(bucketBits))
    val write = slave Flow (FlowWrite(bucketBits, addressBits))
  }
  val ways = Seq.fill(2)(Mem(FlowEntry(addressBits), 1 << bucketBits))
  for ((w, k) <- ways.zipWithIndex) w.write(io.write.bucket, io.write.entry, io.write.valid && io.write.way === k)
  for (w <- ways) w.initBigInt(Seq.fill(1 << bucketBits)(BigInt(0)))

  val s1 = RegNext(io.lookup) init (io.lookup.getZero)
  val bucket = RegNext(FlowTable.hash(io.lookup.payload.asBits)(bucketBits - 1 downto 0).asUInt)
  val read = ways.map(_.readSync(bucket, s1.valid))
  val s2 = RegNext(s1) init (s1.getZero)
  val entries = read.map(RegNext(_)) // registered RAM outputs
  val s3 = RegNext(s2) init (s2.getZero)
  val hits = entries.map(e => RegNext(e.valid && e.key === s3.payload)) // s4
  val actions = entries.map(e => RegNext(e.action))
  val s4 = RegNext(s3.valid) init (False)
  io.result.valid := RegNext(s4) init (False)
  io.result.hit := RegNext(hits.orR)
  io.result.action := RegNext(hits(0) ? actions(0) | actions(1))
  io.result.index := RegNext(Delay(bucket, 3) @@ !hits(0))
}
