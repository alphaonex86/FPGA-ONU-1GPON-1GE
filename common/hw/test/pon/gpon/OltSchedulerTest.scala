// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.collection.mutable
import GponCodes._
import GtcDsTest.{check, bigToBytes, bytesToBig}

/** Scheduler -> (BWmap, PLOAM to the ONU models; their bursts on the line) -> upstream receiver -> scheduler. Test only. */
case class OltUsLoop(maxOnus: Int) extends Component {
  val scheduler = OltScheduler(maxOnus)
  val receiver = GtcUsDeframer()
  val io = new Bundle {
    val config = in(OltConfig())
    val time = in UInt (24 bits)
    val frameStart = in Bool ()
    val line = in Bits (8 bits)
    val bwmapWrite = master(cloneOf(scheduler.io.bwmapWrite))
    val bwmapCommit = master(cloneOf(scheduler.io.bwmapCommit))
    val ploam = master(cloneOf(scheduler.io.ploam))
    val cpuPloam = slave(cloneOf(scheduler.io.cpuPloam))
    val cpuUsPloam = master(cloneOf(scheduler.io.cpuUsPloam))
    val event = master(cloneOf(scheduler.io.event))
    val release = slave(cloneOf(scheduler.io.release))
    val scheduleError = out Bool ()
    val burst = master(cloneOf(receiver.io.burst))
  }
  scheduler.io.config := io.config
  scheduler.io.time := io.time
  scheduler.io.frameStart := io.frameStart
  io.bwmapWrite << scheduler.io.bwmapWrite
  io.bwmapCommit << scheduler.io.bwmapCommit
  io.ploam << scheduler.io.ploam
  scheduler.io.cpuPloam << io.cpuPloam
  io.cpuUsPloam << scheduler.io.cpuUsPloam
  io.event << scheduler.io.event
  scheduler.io.release << io.release
  io.scheduleError := scheduler.io.scheduleError
  receiver.io.grant << scheduler.io.grant
  receiver.io.line := io.line
  receiver.io.time := io.time
  receiver.io.delimiter := B(usDelimiter, 24 bits)
  receiver.io.delimiterErrors := 1
  scheduler.io.burst := receiver.io.burst
  scheduler.io.usPloam := receiver.io.ploam
  io.burst := receiver.io.burst
}

/** Activation of four ONUs at different distances (one never answers ranging), then static grants; the CPU sends an
  * Assign_Alloc-ID broadcast and silences an ONU that fails ranging. Shared by the scheduler and OLT MAC tests. */
class OltActivation(maxFrames: Int) {
  val bitsPerUs = 1244.16
  val rtdMin = 5443 // 35 us response time, 0 km
  val reach = 31415 // 202 us: 20 km differential reach + 2 us response spread
  val teqd = 37000
  val grantBytes = 63 // PLOAMu (13) + ten idle GEM frames
  val gapBytes = 20
  val preUnits = 5
  val upstreamOverhead = Seq(32, 0, 0, 0xAA, 0xAB, 0x59, 0x83, 0x20 /* e: pre-assigned delay */ , 0, preUnits)
  val line = new UsLine((maxFrames + 4) * usFrameBytes)

  def us(x: Double) = (x * bitsPerUs).round.toInt
  val onus = Seq(
    new OnuModel(line, "A", BigInt("414C434C00000001", 16), us(45) + 3, 1, gasp = true), // 1 km
    new OnuModel(line, "B", BigInt("48575443000000B2", 16), us(85) + 5, 2),              // 5 km
    new OnuModel(line, "C", BigInt("5A544547000000C3", 16), us(135) + 1, 3),             // 10 km
    new OnuModel(line, "D", BigInt("47504F4E000000D4", 16), us(60), 4, mute = true))    // never answers ranging
  val Seq(a, b, c, d) = onus

  def configure(config: OltConfig): Unit = {
    config.enable #= true
    config.upstreamOverhead #= bytesToBig(upstreamOverhead)
    config.teqd #= teqd
    config.rtdMin #= rtdMin
    config.reach #= reach
    config.requestStart #= 20
    config.quietBefore #= 2 // ranged bursts of frames F-2, F-1 land inside the request window of frame F
    config.quietAfter #= 0
    config.snPeriod #= 2
    config.grantBytes #= grantBytes
    config.gapBytes #= gapBytes
  }

  val cpuPloam = BigInt("FF0A", 16) << 80
  val cpuQueue = mutable.Queue(cpuPloam)
  def disable(sn: BigInt) = (BigInt("FF06FF", 16) << 72) | (sn << 8)

  val events = mutable.ArrayBuffer[(String, Int, BigInt, Long)]()
  val cpuUs = mutable.ArrayBuffer[BigInt]()
  val bursts = mutable.ArrayBuffer[(Boolean, Int, Int, Long)]()
  val sentPloams = mutable.ArrayBuffer[BigInt]()
  var normal = Seq[Alloc]() // last BWmap with the three active ONUs
  var frames = 0

  /** Upstream clock domain observations. */
  def sample(event: Flow[OltEvent], cpuUsPloam: Flow[Bits], burst: Flow[UsBurst]): Unit = {
    if (event.valid.toBoolean) {
      events += ((event.kind.toEnum.getName(), event.onuId.toInt, event.sn.toBigInt, event.eqd.toLong))
      if (events.last._1 == "RANGING_FAILED") cpuQueue += disable(events.last._3)
    }
    if (cpuUsPloam.valid.toBoolean) cpuUs += cpuUsPloam.payload.toBigInt
    if (burst.valid.toBoolean)
      bursts += ((burst.found.toBoolean, burst.allocId.toInt, burst.onuId.toInt, burst.delayBits.toLong))
  }

  /** A downstream frame starting at upstream time `t` as the ONUs see it. */
  def frame(t: Int, bwmap: Seq[Alloc], ploam: Option[BigInt], now: Int): Unit = {
    ploam.foreach(sentPloams += _)
    if (bwmap.length == 3) normal = bwmap
    for (o <- onus) o.frame(t, bwmap, ploam.map(bigToBytes(_, 12)), now)
    frames += 1
  }

  def done: Boolean = (Seq(a, b, c).forall(_.state == 5) && events.exists(_._1 == "RANGING_FAILED")) ||
    frames == maxFrames - 20

  def checks(scheduleError: Boolean): Unit = {
    def ranged(o: OnuModel) = events.exists(e => e._1 == "RANGED" && e._3 == o.sn)
    val found = events.filter(_._1 == "SN_FOUND")
    check(Seq(a, b, c).forall(o => found.count(_._3 == o.sn) == 1) && found.exists(_._3 == d.sn) &&
      Seq(a, b, c).map(_.onuId).distinct.length == 3,
      s"each ONU discovered, A, B, C once on distinct ONU-IDs (${found.map(e => f"${e._3}%X -> ${e._2}").mkString(", ")})")
    for (o <- Seq(a, b, c)) {
      val e = events.find(e => e._1 == "RANGED" && e._3 == o.sn).map(_._4)
      check(e.contains(teqd * 8L - o.rtdBits) && o.eqd == teqd * 8L - o.rtdBits,
        s"${o.name}: EqD = Teqd - round trip = ${teqd * 8L - o.rtdBits} bits (event $e, ONU ${o.eqd})")
    }
    check(!ranged(d) && events.exists(e => e._1 == "RANGING_FAILED" && e._3 == d.sn) && d.deactivated > 0 && d.state != 5,
      "D: ranging fails, Deactivate_ONU-ID takes its ONU-ID back before reuse")
    check(d.state == 7 && sentPloams.contains(disable(d.sn)), "D silenced by the CPU's Disable_Serial_Number")
    val ids = Seq(a, b, c).map(_.onuId)
    val last = bursts.filter(x => ids.contains(x._2)).takeRight(9)
    check(last.length == 9 && last.forall(x => x._1 && x._4 == 0 && x._3 == x._2),
      s"in operation every burst of A, B, C arrives on its grant to the bit (last ${last.length} bursts)")
    check(normal.map(_.allocId) == ids.sorted && normal.head.start == gapBytes &&
      normal.zip(normal.tail).forall { case (p, q) => q.start == p.stop + 1 + gapBytes } &&
      normal.forall(x => x.stop - x.start + 1 == grantBytes && x.flags == 0x400),
      s"static BWmap: ${normal.length} grants of $grantBytes bytes with PLOAMu, $gapBytes bytes apart")
    check(cpuUs.exists(m => (m >> 80) == ((BigInt(a.onuId) << 8) | 0x03)) && cpuUs.forall(m => ((m >> 80) & 0xFF) != 0x04),
      s"Dying_Gasp of A reaches the CPU, No_message does not (${cpuUs.length} forwarded)")
    check(sentPloams.contains(cpuPloam) && sentPloams.count(m => (m >> 80) == 0xFF01) >= 4,
      s"downstream PLOAMs: the CPU's goes out (${sentPloams.contains(cpuPloam)}), Upstream_Overhead before each SN " +
        s"request (${sentPloams.count(m => (m >> 80) == 0xFF01)})")
    check(!scheduleError, "no schedule error")
  }
}

object OltSchedulerTest extends App {
  val maxFrames = 140
  val act = new OltActivation(maxFrames)

  SimConfig.compile(OltUsLoop(8)).doSim { dut =>
    val cd = dut.clockDomain
    val io = dut.io
    act.configure(io.config)
    io.frameStart #= false
    io.ploam.ready #= false
    io.release.valid #= false
    io.time #= 0
    io.line #= 0

    val entries = mutable.Map[Int, Alloc]()
    var committed = Seq[Alloc]()
    var cycle = 0
    cd.onSamplings {
      if (io.bwmapWrite.valid.toBoolean) {
        val w = io.bwmapWrite.alloc
        entries(io.bwmapWrite.index.toInt) = Alloc(w.allocId.toInt, w.flags.toInt, w.start.toInt, w.stop.toInt)
      }
      if (io.bwmapCommit.valid.toBoolean) committed = (0 until io.bwmapCommit.payload.toInt).map(entries)
      act.sample(io.event, io.cpuUsPloam, io.burst)
      if (io.frameStart.toBoolean) { // the downstream frame starts at this edge: its BWmap was committed before
        val ploam = if (io.ploam.valid.toBoolean) Some(io.ploam.payload.toBigInt) else None
        if (act.cpuQueue.nonEmpty && ploam.contains(act.cpuQueue.head)) act.cpuQueue.dequeue()
        act.frame(cycle, committed, ploam, cycle)
      }
      cycle += 1
      val start = cycle % usFrameBytes == 0
      io.time #= cycle & 0xFFFFFF
      io.frameStart #= start
      io.ploam.ready #= start
      io.line #= act.line.byte(cycle)
      io.cpuPloam.valid #= act.cpuQueue.nonEmpty
      if (act.cpuQueue.nonEmpty) io.cpuPloam.payload #= act.cpuQueue.head
    }
    cd.forkStimulus(10)
    cd.waitSamplingWhere(act.done)
    val settle = act.frames
    cd.waitSamplingWhere(act.frames == settle + 15)
    act.checks(io.scheduleError.toBoolean)
    println(s"activation done in $settle frames")
  }
  println("PASS OltSchedulerTest")
}
