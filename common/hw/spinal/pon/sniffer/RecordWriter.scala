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

/** Up to 4 bytes of a record's payload: `count` of them, the first in bits 31-24. */
case class WideBytes() extends Bundle {
  val bytes = Bits(32 bits)
  val count = UInt(3 bits)
}

/** RecordWriter for a source of up to 4 payload bytes a clock, read as 32-bit words: the same record, first byte in
  * bits 31-24, its last word padded with zeros. The clock after a `close` must carry nothing: the bytes still held
  * leave then. The data FIFO holds words, the payload already where it goes behind the header's last bytes.
  * A source that can wait sends nothing 3 clocks after `room` falls: no record is then cut or lost here. */
case class WideRecordWriter(extraBytes: Int, dataWords: Int, headDepth: Int) extends Component {
  val io = new Bundle {
    val data = slave Flow (WideBytes())
    val close = slave Flow (RecordHead(extraBytes))
    val output = master Stream (Fragment(Bits(32 bits)))
    val room = out Bool ()
    val drops = out UInt (32 bits)       // records lost whole
    val truncations = out UInt (32 bits) // records cut short (flag bit 7)
  }
  val headBytes = 12 + extraBytes
  val offset = headBytes % 4 // header bytes in the first data word
  val heads = StreamFifo(RecordEntry(extraBytes), headDepth)
  val datas = StreamFifo(Bits(32 bits), dataWords)

  // input: one open record, kept or dropped as a whole at its first clock; its bytes packed into words
  val idle = RegInit(True)
  val finishing = RegInit(False) // the clock after the close
  val kept, truncated = Reg(Bool())
  val held = Reg(UInt(2 bits))   // bytes waiting for a full word, in `pending` from bit 23 down
  val pending = Reg(Bits(24 bits))
  val fed, stored = Reg(UInt(16 bits)) // payload bytes taken, and those in the words pushed
  val closing = Reg(RecordHead(extraBytes))
  val starting = idle && (io.data.valid || io.close.valid)
  // the head is pushed two clocks after the close; room is a clock old: at most one push since, one entry left
  val room = RegNext(heads.io.availability > 2) init (False)
  io.room := room && RegNext(datas.io.availability > 8, False)
  val keep = starting ? room | kept
  val heldNow = starting ? U(offset, 2 bits) | held
  val count = io.data.valid ? io.data.count | U(0, 3 bits)
  val masked = io.data.bytes & ~(B(0xFFFFFFFFL, 32 bits) |>> (count << 3))
  val joined = ((starting ? B(0, 24 bits) | pending) ## B(0, 32 bits)) | ((masked ## B(0, 24 bits)) |>> (heldNow << 3))
  val total = heldNow +^ count
  val full = total >= 4
  val fedNow = (starting ? U(0, 16 bits) | fed) + count
  val want = !finishing && (io.data.valid || io.close.valid) && keep && !(!starting && truncated) && full
  val last = finishing && kept && !truncated && fed =/= stored
  datas.io.push.valid := want || last
  datas.io.push.payload := finishing ? (pending ## B(0, 8 bits)) | joined(55 downto 24)
  when(!finishing && (io.data.valid || io.close.valid)) {
    idle := False
    kept := keep
    truncated := (!starting && truncated) || (want && !datas.io.push.ready)
    held := (full ? (total - 4) | total).resized
    pending := full ? joined(23 downto 0) | joined(55 downto 32)
    fed := fedNow
    stored := (want && datas.io.push.ready) ? (fedNow - (total - 4).resized) | (starting ? U(0, 16 bits) | stored)
    closing := io.close.payload
    finishing := io.close.valid
  }
  val cut = truncated || (last && !datas.io.push.ready)
  val head = Flow(RecordEntry(extraBytes))
  head.valid := finishing && kept
  head.head := closing
  head.head.flags(7) := cut
  head.length := ((last && datas.io.push.ready) ? fed | stored) + extraBytes
  val headStaged = head.stage()
  heads.io.push.valid := headStaged.valid
  heads.io.push.payload := headStaged.payload
  val drops = Reg(UInt(32 bits)) init (0)
  val truncations = Reg(UInt(32 bits)) init (0)
  when(finishing) {
    idle := True
    finishing := False
    when(!kept)(drops := drops + 1)
    when(kept && cut)(truncations := truncations + 1)
  }
  io.drops := drops
  io.truncations := truncations

  // output: the header's words, the data's behind (the first of them under the header's last bytes)
  val entry = heads.io.pop.payload
  val h = entry.head
  val header = RecordHead.bytes(h.kind, h.flags, entry.length, h.time) ++
    (extraBytes - 1 to 0 by -1).map(i => h.extra(8 * i + 7 downto 8 * i)) ++ Seq.fill((4 - offset) % 4)(B(0, 8 bits))
  val headWords = Vec(header.grouped(4).map(_.reduce(_ ## _)).toSeq)
  val index = Reg(UInt(15 bits)) init (0)
  // set at index 0, before they can matter: the head memory is slow to read
  val lastIndex = RegNextWhen((((entry.length +^ U(12 + 3)) >> 2) - 1).resize(15 bits), index === 0)
  val payload = RegNextWhen(entry.length =/= extraBytes, index === 0)
  val inHead = index < headWords.length
  val data = index >= headBytes / 4 && (payload || index =/= headBytes / 4)
  io.output.valid := heads.io.pop.valid && (!data || datas.io.pop.valid)
  io.output.fragment := (inHead ? headWords(index.resize(log2Up(headWords.length) bits)) | B(0, 32 bits)) |
    (data ? datas.io.pop.payload | B(0, 32 bits))
  io.output.last := index =/= 0 && index === lastIndex
  datas.io.pop.ready := heads.io.pop.valid && data && io.output.ready
  heads.io.pop.ready := io.output.fire && io.output.last
  when(io.output.fire)(index := io.output.last ? U(0, 15 bits) | index + 1)
}
