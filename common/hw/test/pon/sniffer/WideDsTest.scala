// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.collection.mutable
import pon.gpon._
import pon.gpon.GponCodes._
import pon.gpon.GtcDsTest.{check, GemDriver, PloamDriver, writeBwmap}
import pon.sniffer.SnifferTest.{Record, RecordSink, writeMap}

/** Reference: OLT framer -> byte-wide deframer -> SnifferDs, the line bytes out. Test only. */
case class WideDsReference(fec: Boolean) extends Component {
  val framer = GtcDsFramer()
  val deframer = GtcDsDeframer()
  val sniffer = SnifferDs()
  val io = new Bundle {
    val ploam = slave(cloneOf(framer.io.ploam))
    val bwmapWrite = slave(cloneOf(framer.io.bwmapWrite))
    val bwmapCommit = slave(cloneOf(framer.io.bwmapCommit))
    val gem = slave(cloneOf(framer.io.gem))
    val allocMap = slave Flow (KeepWrite())
    val portMap = slave Flow (KeepWrite())
    val line = out Bits (8 bits)
    val frameStart = out Bool ()
    val output = master Stream (Fragment(Bits(8 bits)))
  }
  framer.io.ploam << io.ploam
  framer.io.bwmapWrite << io.bwmapWrite
  framer.io.bwmapCommit << io.bwmapCommit
  framer.io.gem << io.gem
  framer.io.fec := Bool(fec)
  io.line := framer.io.line
  io.frameStart := framer.io.frameStart
  deframer.io.line := framer.io.line
  sniffer.io.fields := DsFields.of(deframer)
  sniffer.io.time := 0
  sniffer.io.onuId := 5
  sniffer.io.allPloams := False
  sniffer.io.allocMap << io.allocMap
  sniffer.io.portMap << io.portMap
  io.output << sniffer.io.output
}

/** The 4-byte-wide chain. Test only. */
case class WideDsBench() extends Component {
  val deframer = WideDsDeframer()
  val sniffer = WideSnifferDs()
  val io = new Bundle {
    val word = in Bits (32 bits)
    val allocMap = slave Flow (KeepWrite())
    val portMap = slave Flow (KeepWrite())
    val output = master Stream (Fragment(Bits(8 bits)))
    val synced = out Bool ()
    val drops, truncations, hecErrors, plendErrors, syncLosses = out UInt (32 bits)
  }
  deframer.io.word := io.word
  sniffer.io.ploam := deframer.io.ploam
  sniffer.io.alloc := deframer.io.alloc
  sniffer.io.gem := deframer.io.gem
  sniffer.io.time := 0
  sniffer.io.onuId := 5
  sniffer.io.allPloams := False
  sniffer.io.allocMap << io.allocMap
  sniffer.io.portMap << io.portMap
  io.output << sniffer.io.output
  io.synced := deframer.io.synced
  io.drops := sniffer.io.drops
  io.truncations := sniffer.io.truncations
  io.hecErrors := deframer.io.hecErrors
  io.plendErrors := deframer.io.plendErrors
  io.syncLosses := deframer.io.syncLosses
}

/** A downstream line and what the byte-wide chain records from it (not an App: its values are set when used). */
object DsLine {
  val allocIds = Seq(0x105)
  val portIds = Seq(0x100, 0x101)
  val frames = 12
  val psyncBytes = Seq(0xB6, 0xAB, 0x31, 0xE0)

  /** Line bytes of `frames` frames with traffic for several ONUs, less the framer's first frame (it has no FEC yet),
    * and the reference's records. A quarter of the GEM bytes is ours (what the wide sniffer drains), half when `heavy`. */
  def reference(fec: Boolean, heavy: Boolean = false): (Seq[Int], Seq[Record]) = {
    val ports = if (heavy) Seq(0x100, 0x200, 0x101, 0x300) else Seq(0x100, 0x200, 0x300, 0x200, 0x101, 0x300, 0x200, 0x300)
    val line = mutable.ArrayBuffer[Int]()
    var records = Seq[Record]()
    SimConfig.compile(WideDsReference(fec)).doSim(if (fec) 2 else 1) { dut =>
      val cd = dut.clockDomain
      val gem = new GemDriver(dut.io.gem, cd)
      val ploam = new PloamDriver(dut.io.ploam, cd)
      val sink = new RecordSink(dut.io.output, cd, 1)
      dut.io.bwmapWrite.valid #= false
      dut.io.bwmapCommit.valid #= false
      dut.io.allocMap.valid #= false
      dut.io.portMap.valid #= false
      var frameStarts = 0
      cd.onSamplings {
        line += dut.io.line.toInt
        if (dut.io.frameStart.toBoolean) frameStarts += 1
      }
      cd.forkStimulus(10)
      writeMap(dut.io.allocMap, allocIds, cd)
      writeMap(dut.io.portMap, portIds, cd)
      val rand = new scala.util.Random(3)
      cd.waitSamplingWhere(frameStarts == 5) // both deframers long in sync: the same superframe count
      writeBwmap(dut.io.bwmapWrite, dut.io.bwmapCommit,
        Seq((0x105, 0x080, 0x0100, 0x0400), (0x205, 0x000, 0x0500, 0x0900), (0x106, 0x000, 0x0A00, 0x0B00), (0x105, 0x000, 0x0C00, 0x0C10)), cd)
      ploam.queue ++= Seq(5, 7, 0xFF, 5, 9).map(onu => (BigInt(onu) << 88) | BigInt(88, rand))
      gem.queue ++= Seq.tabulate(60)(i => (ports(i % ports.length), Seq.fill(Seq(64, 1518, 5000, 7, 300, 1, 2, 3, 4, 5, 6)(i % 11))(rand.nextInt(256))))
      cd.waitSamplingWhere(frameStarts == frames)
      cd.waitSampling(3000)
      records = sink.records.toSeq
    }
    (line.drop(line.indexOfSlice(psyncBytes) + frameBytes - 8).toSeq, records)
  }

}

/** The wide deframer against the byte-wide one on the same line: the same records, FEC off and on, on several bit
  * alignments; a corrupted GEM header; a full-rate burst (whole fragments dropped, each counted). Throws on failure. */
object WideDsTest extends App {
  import DsLine._

  /** The line, `shift` bits late, through the wide chain; `corrupt`: (byte, mask) flipped on the line. The counters
    * are read where the line ends: the zeros that follow (time to drain) are no frame. */
  def wide(compiled: SimCompiled[WideDsBench], line: Seq[Int], shift: Int, corrupt: Option[(Int, Int)] = None) = {
    val bytes = line.toArray
    corrupt.foreach { case (at, mask) => bytes(at) ^= mask }
    def bit(i: Int): Int = if (i < shift || (i - shift) / 8 >= bytes.length) 0 else (bytes((i - shift) / 8) >> (7 - (i - shift) % 8)) & 1
    var records = Seq[Record]()
    var counters = (0L, 0L, 0L, 0L, 0L)
    compiled.doSim(shift) { dut =>
      val cd = dut.clockDomain
      val sink = new RecordSink(dut.io.output, cd, 2)
      dut.io.allocMap.valid #= false
      dut.io.portMap.valid #= false
      dut.io.word #= 0
      cd.forkStimulus(10)
      writeMap(dut.io.allocMap, allocIds, cd)
      writeMap(dut.io.portMap, portIds, cd)
      for (w <- 0 until bytes.length / 4 + 12000) {
        dut.io.word #= (0 until 32).foldLeft(BigInt(0))((a, k) => (a << 1) | bit(32 * w + k))
        cd.waitSampling()
        if (w == bytes.length / 4 - 8)
          counters = (dut.io.drops.toLong, dut.io.truncations.toLong, dut.io.hecErrors.toLong, dut.io.plendErrors.toLong, dut.io.syncLosses.toLong)
      }
      records = sink.records.toSeq
    }
    (records, counters)
  }

  def body(r: Record) = (r.flags, r.payload)
  def of(records: Seq[Record], kind: Int) = records.filter(_.kind == kind).map(body)
  val kinds = Seq((RecordKind.ploam, "PLOAM"), (RecordKind.alloc, "BWmap"), (RecordKind.gem, "GEM"))
  val compiled = SimConfig.compile(WideDsBench())
  for (fec <- Seq(false, true)) {
    val (line, expect) = reference(fec)
    val name = if (fec) "FEC" else "no FEC"
    check(of(expect, RecordKind.gem).length > 10 && of(expect, RecordKind.alloc).length >= 2, s"$name: the reference has GEM and BWmap records (${expect.length})")
    for (shift <- Seq(0, 1, 13, 31)) {
      val (got, counters) = wide(compiled, line, shift)
      for ((kind, what) <- kinds) {
        val (g, e) = (of(got, kind), of(expect, kind))
        // the two do not reach SYNC on the same frame: one frame's PLOAM more on either side
        check((g.endsWith(e) || e.endsWith(g)) && (g.length - e.length).abs <= 1, s"$name, $shift bits late: $what records as the byte-wide deframer's (${g.length} of ${e.length})")
      }
      check(counters == (0, 0, 0, 0, 0), s"$name, $shift bits late: nothing dropped, no error counted $counters")
    }
    if (!fec) {
      // the third frame is idle GEM headers from byte 30: one bit of the first one
      val (got, counters) = wide(compiled, line, 5, Some((line.indexOfSlice(psyncBytes) + 2 * frameBytes + 31, 0x10)))
      check(counters == (0, 0, 1, 0, 0), s"one HEC error counted for a corrupted idle header $counters")
      check(of(got, RecordKind.gem) == of(expect, RecordKind.gem), "the GEM records of the next frames are whole")
    }
  }
  val (line, expect) = reference(fec = false, heavy = true)
  val (got, counters) = wide(compiled, line, 7)
  val (g, e) = (of(got, RecordKind.gem), of(expect, RecordKind.gem))
  val rest = e.iterator
  check(counters._1 > 0 && e.length - g.length == counters._1 && g.forall(r => rest.contains(r)),
    s"full-rate burst: ${g.length} of ${e.length} GEM records, whole and in order, the ${counters._1} others counted as drops")
  check(of(expect, RecordKind.ploam).endsWith(of(got, RecordKind.ploam)) && of(got, RecordKind.alloc) == of(expect, RecordKind.alloc), "full-rate burst: no PLOAM or BWmap record lost")
  println("PASS WideDsTest")
}
