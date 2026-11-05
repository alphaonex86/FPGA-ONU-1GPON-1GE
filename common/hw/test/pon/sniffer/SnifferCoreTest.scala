// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba3.apb.sim.Apb3Driver
import scala.collection.mutable
import pon.gpon._
import pon.gpon.GponCodes._
import pon.gpon.GtcDsTest.{check, bigToBytes, bytesToBig, PloamDriver}

/** OLT framer -> deframer -> SnifferCore downstream; the upstream line from the test in a half-rate synchronous domain. Test only. */
case class SnifferCoreBench() extends Component {
  val usCd = ClockDomain.external("us")
  usCd.setSynchronousWith(ClockDomain.current)
  val framer = GtcDsFramer()
  val deframer = GtcDsDeframer()
  val core = SnifferCore(usCd, gemDepth = 512, burstDepth = 1024, usWords = 64)
  val io = new Bundle {
    val apb = slave(Apb3(Apb3Config(addressWidth = 8, dataWidth = 32)))
    val ploam = slave(cloneOf(framer.io.ploam))
    val usLine = in Bits (8 bits)
    val irq = out Bool ()
  }
  framer.io.ploam << io.ploam
  framer.io.bwmapWrite.setIdle()
  framer.io.bwmapCommit.setIdle()
  framer.io.gem.setIdle()
  framer.io.fec := False
  core.io.apb <> io.apb
  deframer.io.line := framer.io.line
  core.io.ds := DsFields.of(deframer)
  core.io.usLine := io.usLine
  io.irq := core.io.irq
}

/** Records of both directions read back as words through the registers; nothing recorded before enable. */
object SnifferCoreTest extends App {
  val (ctrl, delimiterReg, level, dsData, usData) = (0x00, 0x04, 0x10, 0x14, 0x18)
  val frameBytes = 38880 // downstream line bytes per 125 us frame
  case class Record(kind: Int, flags: Int, payload: Seq[Int])
  def parse(words: Seq[Long]): Seq[Record] = {
    val bytes = words.flatMap(w => (3 to 0 by -1).map(k => ((w >> (8 * k)) & 0xFF).toInt))
    val records = mutable.ArrayBuffer[Record]()
    var at = 0
    while (at < bytes.length) {
      val length = bytes(at + 2) | (bytes(at + 3) << 8)
      check((bytes(at) & 0xF8) == 0xA0 && at + 12 + length <= bytes.length, s"record framing at byte $at")
      records += Record(bytes(at) & 7, bytes(at + 1), bytes.slice(at + 12, at + 12 + length))
      at += (12 + length + 3) / 4 * 4 // each record padded to a whole word
    }
    records.toSeq
  }

  // upstream bursts at these line bytes; the first comes before enable
  val rnd = new scala.util.Random(5)
  val bursts = Seq(100 -> 0, 900 -> 3, 1300 -> 6).map { case (at, delay) =>
    (at, delay, Seq(0x40, 5, 0x00) ++ Seq.fill(150 + at / 10)(rnd.nextInt(256))) }
  val totalBytes = 1700
  val bits = Array.fill(totalBytes * 8)(0)
  def put(bit: Int, bytes: Seq[Int]): Unit =
    for ((b, i) <- bytes.zipWithIndex; k <- 0 until 8) bits(bit + 8 * i + k) = (b >> (7 - k)) & 1
  def scrambled(clear: Seq[Int]) = clear.zip(Model.keystream(clear.length)).map { case (c, k) => c ^ k }
  for ((at, delay, clear) <- bursts) {
    val first = at * 8 + delay
    put(first - 11 * 8, Seq.fill(8)(0xAA))
    put(first - 3 * 8, bigToBytes(usDelimiter, 3))
    put(first, scrambled(clear))
  }
  // a burst's last zero line bytes look like the squelch (SnifferUs)
  val expectBursts = bursts.drop(1).map { case (_, _, clear) => clear.take(scrambled(clear).lastIndexWhere(_ != 0) + 1) }
  def lineByte(t: Int): Int = if (t >= totalBytes) 0 else (0 until 8).foldLeft(0)((a, k) => (a << 1) | bits(t * 8 + k))

  SimConfig.compile(SnifferCoreBench()).doSim { dut =>
    val cd = dut.clockDomain
    val usCd = dut.usCd
    val apb = Apb3Driver(dut.io.apb, cd)
    val ploam = new PloamDriver(dut.io.ploam, cd)
    var usCycle = 0
    dut.io.usLine #= 0
    usCd.onSamplings {
      dut.io.usLine #= lineByte(usCycle)
      usCycle += 1
    }
    cd.forkStimulus(10)
    usCd.forkStimulus(20)
    cd.waitSampling(10)
    apb.write(delimiterReg, usDelimiter)
    cd.waitSamplingWhere(usCycle > 400) // the first burst has passed
    apb.write(ctrl, 1 | (5 << 8) | (1 << 16))
    // the CPU's part: drain both directions as records come (a noMessage PLOAM is recorded every frame)
    val dsWords, usWords = mutable.ArrayBuffer[Long]()
    def drain(): Unit = {
      val waiting = apb.read(level).toLong
      for (_ <- 0L until (waiting & 0xFF)) dsWords += apb.read(dsData).toLong
      for (_ <- 0L until ((waiting >> 8) & 0xFFFF)) usWords += apb.read(usData).toLong
    }
    def drainFor(cycles: Int): Unit = for (_ <- 0 until cycles / 100) { drain(); cd.waitSampling(100) }
    while (((apb.read(level) >> 24) & 1) == 0) { drain(); cd.waitSampling(100) } // downstream synced
    val sentPloams = Seq(5, 7, 0xFF).map(onu => (BigInt(onu) << 88) | BigInt(88, rnd))
    ploam.queue ++= sentPloams
    drainFor(4 * frameBytes) // one PLOAM per frame
    check(usCycle > totalBytes, "upstream line done")
    apb.write(ctrl, 0)
    drainFor(frameBytes)
    check(!dut.io.irq.toBoolean, "no interrupt once disabled and read")
    check(apb.read(0x1C) == 0 && apb.read(0x20) == 0, "no record lost")
    val ds = parse(dsWords.toSeq)
    val us = parse(usWords.toSeq)
    val ploams = ds.filter(_.kind == RecordKind.ploam).map(r => bytesToBig(r.payload)).filter(_ != noMessage)
    check(ploams == sentPloams.filter(p => (p >> 88) != 7), s"downstream: the PLOAMs to ONU-ID 5 and broadcast (${ploams.length})")
    check(us.map(_.payload) == expectBursts && us.forall(r => r.kind == RecordKind.burst && r.flags == 0),
      s"upstream: the bursts after enable, whole (${us.map(_.payload.length)})")
  }
  println("PASS SnifferCoreTest")
}
