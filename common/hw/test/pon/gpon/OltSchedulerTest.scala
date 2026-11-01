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

/** Activation of four ONUs at different distances (one never answers ranging), then static grants. */
object OltSchedulerTest extends App {
  val bitsPerUs = 1244.16
  val rtdMin = 5443 // 35 us response time, 0 km
  val reach = 31415 // 202 us: 20 km differential reach + 2 us response spread
  val teqd = 37000
  val xStart = 20
  val grantBytes = 63 // PLOAMu (13) + ten idle GEM frames
  val gapBytes = 20
  val preUnits = 5
  val upstreamOverhead = Seq(32, 0, 0, 0xAA, 0xAB, 0x59, 0x83, 0x20 /* e: pre-assigned delay */ , 0, preUnits)

  // line: laser off (0), each burst ORed in at its bit
  val maxFrames = 140
  val totalBytes = (maxFrames + 4) * usFrameBytes
  val bits = new Array[Boolean](totalBytes * 8)
  def lineByte(t: Int): Int = (0 until 8).foldLeft(0)((a, k) => (a << 1) | (if (bits(t * 8 + k)) 1 else 0))

  case class Alloc(allocId: Int, flags: Int, start: Int, stop: Int)

  class Onu(val name: String, val sn: BigInt, val rtdBits: Int, seed: Int, mute: Boolean = false, gasp: Boolean = false) {
    var state = 2 // O2 standby: downstream in sync
    var onuId = 0xFF
    var preBits = 0
    var eqd = 0L
    var bip = 0
    var gaspPending = gasp
    var deactivated = 0
    val rnd = new scala.util.Random(seed)

    def frame(t: Int, bwmap: Seq[Alloc], ploam: Option[Seq[Int]], now: Int): Unit = {
      for (a <- bwmap) {
        val snMsg = (r: Int) => Seq(onuId, 0x01) ++ bigToBytes(sn, 8) ++ Seq(r >> 4, ((r & 0xF) << 4) | 0x04)
        if (state == 3 && a.allocId == 254) {
          val r = rnd.nextInt(234)
          burst(t, a, preBits + r * 256, snMsg(r), now)
        } else if (state == 4 && a.allocId == onuId && !mute) burst(t, a, preBits, snMsg(0), now)
        else if (state == 5 && a.allocId == onuId) {
          val msg = Seq(onuId, if (gaspPending) 0x03 else 0x04) ++ Seq.fill(10)(0)
          gaspPending = false
          burst(t, a, eqd.toInt, msg, now)
        }
      }
      for (m <- ploam) (m(1), m(0)) match {
        case (0x01, 0xFF) if state == 2 =>
          preBits = if ((m(9) >> 5 & 1) == 1) ((m(10) << 8) | m(11)) * 256 else 0
          state = 3
        case (0x03, 0xFF) if state == 3 && bytesToBig(m.slice(3, 11)) == sn => onuId = m(2); state = 4
        case (0x04, id) if id == onuId && state >= 4 => eqd = bytesToBig(m.slice(3, 7)).toLong; state = 5
        case (0x05, id) if (id == onuId || id == 0xFF) && state != 7 => onuId = 0xFF; state = 2; deactivated += 1
        case (0x06, 0xFF) if m(2) == 0xFF && bytesToBig(m.slice(3, 11)) == sn => state = 7 // emergency stop
        case _ =>
      }
    }

    /** PLOu, PLOAMu, then idle GEM frames to the end of the allocation, `delay` bits after its nominal place. */
    def burst(t: Int, a: Alloc, delay: Int, ploam: Seq[Int], now: Int): Unit = {
      val content = ploam :+ Model.crc8(ploam)
      val fill = (a.stop - a.start + 1 - content.length) / 5
      val clear = Seq(bip, onuId, 0) ++ content ++ Seq.fill(fill)(bigToBytes(gemMask, 5)).flatten
      val bipFirst = (t + a.start - 3) * 8 + rtdBits + delay
      if (bipFirst - 11 * 8 <= now * 8) throw new Exception(s"FAIL: $name: burst placed in the past")
      def put(bit: Int, bytes: Seq[Int]): Unit =
        for ((b, i) <- bytes.zipWithIndex; k <- 0 until 8) bits(bit + 8 * i + k) |= ((b >> (7 - k)) & 1) == 1
      put(bipFirst - 11 * 8, Seq.fill(8)(0xAA))
      put(bipFirst - 3 * 8, bigToBytes(usDelimiter, 3))
      put(bipFirst, clear.zip(Model.keystream(clear.length)).map { case (c, k) => c ^ k })
      bip = clear.drop(1).reduce(_ ^ _)
    }
  }

  def us(x: Double) = (x * bitsPerUs).round.toInt
  val onus = Seq(
    new Onu("A", BigInt("414C434C00000001", 16), us(45) + 3, 1, gasp = true), // 1 km
    new Onu("B", BigInt("48575443000000B2", 16), us(85) + 5, 2),              // 5 km
    new Onu("C", BigInt("5A544547000000C3", 16), us(135) + 1, 3),             // 10 km
    new Onu("D", BigInt("47504F4E000000D4", 16), us(60), 4, mute = true))    // never answers ranging
  val Seq(a, b, c, d) = onus

  SimConfig.compile(OltUsLoop(8)).doSim { dut =>
    val cd = dut.clockDomain
    val io = dut.io
    io.config.enable #= true
    io.config.upstreamOverhead #= bytesToBig(upstreamOverhead)
    io.config.teqd #= teqd
    io.config.rtdMin #= rtdMin
    io.config.reach #= reach
    io.config.requestStart #= xStart
    io.config.quietBefore #= 2 // ranged bursts of frames F-2, F-1 land inside the request window of frame F
    io.config.quietAfter #= 0
    io.config.snPeriod #= 2
    io.config.grantBytes #= grantBytes
    io.config.gapBytes #= gapBytes
    io.frameStart #= false
    io.ploam.ready #= false
    io.release.valid #= false
    io.time #= 0
    io.line #= 0
    // the CPU: an Assign_Alloc-ID broadcast, then Disable_Serial_Number for an ONU that fails ranging
    val cpuPloam = BigInt("FF0A", 16) << 80
    val cpuQueue = mutable.Queue(cpuPloam)
    def disable(sn: BigInt) = (BigInt("FF06FF", 16) << 72) | (sn << 8)

    val entries = mutable.Map[Int, Alloc]()
    var committed = Seq[Alloc]()
    var normal = Seq[Alloc]() // last BWmap with the three active ONUs
    val sentPloams = mutable.ArrayBuffer[BigInt]()
    val events = mutable.ArrayBuffer[(String, Int, BigInt, Long)]()
    val cpuUs = mutable.ArrayBuffer[BigInt]()
    val bursts = mutable.ArrayBuffer[(Boolean, Int, Int, Long)]()
    var frames = 0
    var cycle = 0
    cd.onSamplings {
      if (io.bwmapWrite.valid.toBoolean) {
        val w = io.bwmapWrite.alloc
        entries(io.bwmapWrite.index.toInt) = Alloc(w.allocId.toInt, w.flags.toInt, w.start.toInt, w.stop.toInt)
      }
      if (io.bwmapCommit.valid.toBoolean) {
        committed = (0 until io.bwmapCommit.payload.toInt).map(entries)
        if (committed.length == 3) normal = committed
      }
      if (io.event.valid.toBoolean) {
        events += ((io.event.kind.toEnum.getName(), io.event.onuId.toInt, io.event.sn.toBigInt, io.event.eqd.toLong))
        if (events.last._1 == "RANGING_FAILED") cpuQueue += disable(events.last._3)
      }
      if (io.cpuUsPloam.valid.toBoolean) cpuUs += io.cpuUsPloam.payload.toBigInt
      if (io.burst.valid.toBoolean) bursts += ((io.burst.found.toBoolean, io.burst.allocId.toInt, io.burst.onuId.toInt,
        io.burst.delayBits.toLong))
      if (io.frameStart.toBoolean) { // the downstream frame starts at this edge: its BWmap was committed before
        val ploam = if (io.ploam.valid.toBoolean) Some(io.ploam.payload.toBigInt) else None
        ploam.foreach(sentPloams += _)
        if (cpuQueue.nonEmpty && ploam.contains(cpuQueue.head)) cpuQueue.dequeue()
        for (o <- onus) o.frame(cycle, committed, ploam.map(bigToBytes(_, 12)), cycle)
        frames += 1
      }
      cycle += 1
      val start = cycle % usFrameBytes == 0
      io.time #= cycle & 0xFFFFFF
      io.frameStart #= start
      io.ploam.ready #= start
      io.line #= lineByte(cycle)
      io.cpuPloam.valid #= cpuQueue.nonEmpty
      if (cpuQueue.nonEmpty) io.cpuPloam.payload #= cpuQueue.head
    }
    cd.forkStimulus(10)
    def ranged(o: Onu) = events.exists(e => e._1 == "RANGED" && e._3 == o.sn)
    cd.waitSamplingWhere((Seq(a, b, c).forall(_.state == 5) && events.exists(_._1 == "RANGING_FAILED")) ||
      frames == maxFrames - 20)
    val settle = frames
    cd.waitSamplingWhere(frames == settle + 15)

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
      "downstream PLOAMs: the CPU's goes out, Upstream_Overhead before each SN request")
    check(!io.scheduleError.toBoolean, "no schedule error")
    println(s"activation done in $settle frames")
  }
  println("PASS OltSchedulerTest")
}
