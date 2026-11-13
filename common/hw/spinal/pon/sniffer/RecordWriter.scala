// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.lib._

/** Record kinds, byte 0 of a record is 0xA0 | kind. */
object RecordKind { val ploam = 0; val alloc = 1; val gem = 2; val burst = 3; val reply = 4 }

/** What closes a record: its header fields and the first `extraBytes` payload bytes (line order).
  * Flag bit 7 is the writer's: set when the data FIFO was full and the record lost its tail. */
case class RecordHead(extraBytes: Int) extends Bundle {
  val kind = UInt(3 bits)
  val flags = Bits(8 bits)
  val time = UInt(64 bits)
  val extra = Bits(8 * extraBytes bits)
}

object RecordHead {
  /** The 12 header bytes of a record, in stream order. */
  def bytes(kind: UInt, flags: Bits, length: UInt, time: UInt): Seq[Bits] =
    Seq(B"1010_0" ## kind, flags, length.asBits(7 downto 0), length.asBits(15 downto 8)) ++
      (0 until 8).map(i => time.asBits(8 * i + 7 downto 8 * i))
}

/** A record stream as bytes, zeros after each record to a multiple of 4 bytes (as RecordWords' last word). */
case class RecordPad() extends Component {
  val io = new Bundle {
    val input = slave Stream (Fragment(Bits(8 bits)))
    val output = master Stream (Bits(8 bits))
  }
  val count = Reg(UInt(2 bits)) init (0) // bytes sent, modulo 4
  val padding = Reg(Bool()) init (False)
  io.output.valid := padding || io.input.valid
  io.output.payload := padding ? B(0, 8 bits) | io.input.fragment
  io.input.ready := !padding && io.output.ready
  when(io.output.fire) {
    count := count + 1
    padding := count =/= 3 && (padding || io.input.last)
  }
}

case class RecordEntry(extraBytes: Int) extends Bundle {
  val head = RecordHead(extraBytes)
  val length = UInt(16 bits) // payload bytes, extra included
}

/** One record source. The bytes pushed on `data` since the last `close` (that clock's included) are the payload after
  * `extra`. Out, a byte stream, one fragment per record: 0xA0 | kind, flags, payload length (16 LE), time (64 LE),
  * payload. The header is queued when the record closes, so the data FIFO holds a whole record. A record that starts
  * with fewer than three header entries free is dropped whole (headDepth >= 3); dataDepth + extraBytes < 65536. */
case class RecordWriter(extraBytes: Int, dataDepth: Int, headDepth: Int) extends Component {
  val io = new Bundle {
    val data = slave Flow (Bits(8 bits))
    val close = slave Flow (RecordHead(extraBytes))
    val output = master Stream (Fragment(Bits(8 bits)))
    val drops = out UInt (32 bits)       // records lost whole
    val truncations = out UInt (32 bits) // records cut short (flag bit 7)
  }
  val heads = StreamFifo(RecordEntry(extraBytes), headDepth)
  val datas = StreamFifo(Bits(8 bits), dataDepth)

  // input: one open record, kept or dropped as a whole at its first byte or close
  val idle = RegInit(True)
  val kept = Reg(Bool())
  val truncated = Reg(Bool())
  val count = Reg(UInt(16 bits))
  val starting = idle && (io.data.valid || io.close.valid)
  // the head is pushed a clock after the close (timing); room is a clock old: at most two pushes since, one entry left
  val room = RegNext(heads.io.availability > 2) init (False)
  val keep = starting ? room | kept
  val cutBefore = !starting && truncated
  val push = io.data.valid && keep && !cutBefore
  val cutNow = cutBefore || (push && !datas.io.push.ready)
  val countNow = (starting ? U(0, 16 bits) | count) + (push && datas.io.push.ready).asUInt
  datas.io.push.valid := push && datas.io.push.ready
  datas.io.push.payload := io.data.payload
  val head = Flow(RecordEntry(extraBytes))
  head.valid := io.close.valid && keep
  head.head := io.close.payload
  head.head.flags(7) := cutNow
  head.length := countNow + extraBytes
  val headStaged = head.stage()
  heads.io.push.valid := headStaged.valid
  heads.io.push.payload := headStaged.payload
  when(io.data.valid || io.close.valid) {
    idle := io.close.valid
    kept := keep
    truncated := cutNow
    count := countNow
  }
  val drops = Reg(UInt(32 bits)) init (0)
  val truncations = Reg(UInt(32 bits)) init (0)
  when(io.close.valid && !keep)(drops := drops + 1)
  when(io.close.valid && keep && cutNow)(truncations := truncations + 1)
  io.drops := drops
  io.truncations := truncations

  // output: header bytes, then the record's data bytes
  val headBytes = 12 + extraBytes
  val entry = heads.io.pop.payload
  val h = entry.head
  val header = Vec(RecordHead.bytes(h.kind, h.flags, entry.length, h.time) ++
    (extraBytes - 1 to 0 by -1).map(i => h.extra(8 * i + 7 downto 8 * i)))
  val index = Reg(UInt(16 bits)) init (0)
  val inHeader = index < headBytes
  io.output.valid := heads.io.pop.valid && (inHeader || datas.io.pop.valid)
  io.output.fragment := inHeader ? header(index.resize(log2Up(headBytes) bits)) | datas.io.pop.payload
  val lastIndex = RegNextWhen(entry.length + 11, index === 0) // set before it can match: the head memory is slow to read
  io.output.last := index =/= 0 && index === lastIndex
  datas.io.pop.ready := heads.io.pop.valid && !inHeader && io.output.ready
  heads.io.pop.ready := io.output.fire && io.output.last
  when(io.output.fire)(index := io.output.last ? U(0, 16 bits) | index + 1)
}
