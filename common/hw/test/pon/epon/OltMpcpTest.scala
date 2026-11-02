// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.epon

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.collection.mutable
import pon.gpon.GtcDsTest.check

/** OltMpcp and its MACs, a splitter to ONUs (OnuMpcp and MACs) on fibres of their own (`fibres`: downstream, upstream
  * clocks), the upstream bursts combined (`collision`: two lit at once); `dark` holds an ONU's laser off. */
case class EponPon(fibres: Seq[(Int, Int)]) extends Component {
  val n = fibres.length
  val io = new Bundle {
    val olt = in(OltMpcpConfig())
    val dsData = slave Stream (Fragment(EponTxByte()))
    val usData = master Flow (Fragment(EponRxByte()))
    val usDataGood = out Bool ()
    val deregister = slave Flow (UInt(3 bits))
    val events = master Flow (OltMpcpEventData(8))
    val registered = out Bits (8 bits)
    val onus = in(Vec(OnuMpcpConfig(), n))
    val onuData = Vec(slave(Stream(Fragment(EponUsByte()))), n)
    val reports = in(Vec(UInt(16 bits), n))
    val dark = in Bits (n bits)
    val onuRegistered, bursts = out Bits (n bits)
    val onuRx = Vec(master(Flow(Fragment(EponRxByte()))), n) // the frames each ONU accepts
    val collision, light, rxReset = out Bool ()
  }
  val olt = OltMpcp()
  olt.io.config := io.olt
  olt.io.data << io.dsData
  olt.io.deregister << io.deregister
  io.events << olt.io.events
  io.registered := olt.io.registered
  io.usData << olt.io.dataRx
  io.usDataGood := olt.io.dataRxGood
  val oltTx = EponMacTx()
  oltTx.io.input << olt.io.tx
  val lit = Bits(n bits)
  val codes = Vec(Bits(10 bits), n)
  for (((ds, us), k) <- fibres.zipWithIndex) new Area {
    val rx = EponMacRx()
    rx.io.raw := Delay(oltTx.io.code, ds)
    val mpcp = OnuMpcp()
    mpcp.io.config := io.onus(k)
    mpcp.io.rx << rx.io.frame
    mpcp.io.rxGood := rx.io.good
    mpcp.io.data << io.onuData(k)
    mpcp.io.report := io.reports(k)
    io.onuRx(k).valid := rx.io.frame.valid && mpcp.io.accept
    io.onuRx(k).payload := rx.io.frame.payload
    val tx = EponMacTx()
    tx.io.input << mpcp.io.tx
    val light = RegNext(mpcp.io.burst && !io.dark(k)) init (False)
    lit(k) := Delay(light, us, init = False)
    codes(k) := Delay(tx.io.code, us)
    io.onuRegistered(k) := mpcp.io.registered
    io.bursts(k) := mpcp.io.burst
  }
  io.collision := CountOne(lit) > 1
  io.light := lit.orR
  io.rxReset := olt.io.rxReset
  val oltRx = EponMacRx()
  oltRx.io.raw := (0 until n).foldRight(B(0, 10 bits))((k, rest) => lit(k) ? codes(k) | rest)
  olt.io.rx << oltRx.io.frame
  olt.io.rxGood := oltRx.io.good
}

/** OltMpcp with two ONUs at different distances: both registered through the discovery windows (LLIDs from the base,
  * REGISTERED events), no upstream collision once registered, upstream data of each ONU with its LLID, downstream
  * data to one LLID accepted only by its ONU, grants from the REPORTs (one at maxGrant, the other at minGrant),
  * the optics' receiver reset ending `lead` before each burst, never in one; deregistration by the CPU then
  * registration again, by the ONU (enable low) and by the timeout (fibre cut). */
object OltMpcpTest extends App {
  val rnd = new scala.util.Random(10)
  val macs = Seq(BigInt("001122334401", 16), BigInt("001122334402", 16))
  val (laserOn, laserOff, sync) = (16, 16, 24)
  SimConfig.compile(EponPon(Seq((37, 41), (530, 520)))).doSim { dut =>
    val io = dut.io
    val cd = dut.clockDomain
    cd.forkStimulus(8)
    val o = io.olt
    o.enable #= true; o.mac #= BigInt("0A0B0C0D0E0F", 16); o.llidBase #= 0x10; o.discoveryPeriod #= 2
    o.discoveryLength #= 600; o.maxRtt #= 700; o.syncTime #= sync; o.lead #= 1300; o.horizon #= 4000; o.guard #= 8
    o.overhead #= laserOn + sync + laserOff + Mpcp.cost(Mpcp.pduBytes); o.minGrant #= 160; o.maxGrant #= 1500
    o.timeout #= 0
    val (resetLead, resetWidth) = (4, 8)
    o.rxResetLead #= resetLead; o.rxResetWidth #= resetWidth
    for ((c, k) <- io.onus.zipWithIndex) {
      c.enable #= true; c.mac #= macs(k); c.laserOn #= laserOn; c.laserOff #= laserOff; c.alwaysReport #= true; c.timeout #= 0
    }
    io.reports.foreach(_ #= 0)
    io.dark #= 0
    io.dsData.valid #= false
    io.deregister.valid #= false
    io.onuData.foreach(_.valid #= false)
    val events = mutable.ArrayBuffer[(String, Int, BigInt)]()
    case class Frame(bytes: Seq[Int], llid: Int, good: Boolean)
    val usFrames = mutable.ArrayBuffer[Frame]()
    val onuFrames = Seq.fill(2)(mutable.ArrayBuffer[Seq[Int]]())
    val cur = mutable.ArrayBuffer[Int]()
    val onuCur = Seq.fill(2)(mutable.ArrayBuffer[Int]())
    var collisions = 0
    var (cycle, resetFell, lightWas, resetWas) = (0L, -1L, false, false)
    val leads = mutable.ArrayBuffer[Long]() // clocks from a reset's end to the next light
    var resetInLight = 0
    val burstLengths = Seq.fill(2)(mutable.ArrayBuffer[Int]())
    val burstOn = Array(0, 0)
    cd.onSamplings {
      if (io.events.valid.toBoolean)
        events += ((Seq("REGISTERED", "DEREGISTERED", "DRIFT", "FULL")(io.events.kind.toEnum.position), io.events.onu.toInt, io.events.mac.toBigInt))
      if (io.usData.valid.toBoolean) {
        cur += io.usData.fragment.data.toInt
        if (io.usData.last.toBoolean) { usFrames += Frame(cur.toList, io.usData.fragment.llid.toInt, io.usDataGood.toBoolean); cur.clear() }
      }
      for (k <- 0 until 2) {
        val r = io.onuRx(k)
        if (r.valid.toBoolean) {
          onuCur(k) += r.fragment.data.toInt
          if (r.last.toBoolean) { onuFrames(k) += onuCur(k).toList; onuCur(k).clear() }
        }
        if (((io.bursts.toInt >> k) & 1) == 1) burstOn(k) += 1
        else if (burstOn(k) > 0) { burstLengths(k) += burstOn(k) / 2; burstOn(k) = 0 }
      }
      if (io.collision.toBoolean) collisions += 1
      cycle += 1
      val (light, reset) = (io.light.toBoolean, io.rxReset.toBoolean)
      if (resetWas && !reset) resetFell = cycle
      if (light && !lightWas && resetFell >= 0) { leads += cycle - resetFell; resetFell = -1 }
      if (light && reset) resetInLight += 1
      lightWas = light; resetWas = reset
    }
    def registered = (0 until 2).map(k => ((io.registered.toInt >> k) & 1) == 1 && ((io.onuRegistered.toInt >> k) & 1) == 1)
    def waitFor(what: String, limit: Int)(cond: => Boolean): Unit = {
      var t = 0
      while (!cond && t < limit) { cd.waitSampling(); t += 1 }
      check(cond, s"$what (${t / 2} TQ)")
    }
    waitFor("both ONUs registered", 400000)(registered.forall(identity))
    val reg = events.filter(_._1 == "REGISTERED")
    check(reg.map(e => (e._2, e._3)).toSet == Set((0, macs(0)), (1, macs(1))) || reg.map(_._3).toSet == macs.toSet,
      s"REGISTERED events for both MACs: ${reg.map(e => (e._2, e._3.toString(16)))}")
    val llidOf = reg.map(e => e._3 -> (0x10 + e._2)).toMap
    collisions = 0
    leads.clear(); resetInLight = 0 // the discovery windows: one reset each, REGISTER_REQs at random in them

    // upstream data, reported
    io.reports.foreach(_ #= 600)
    val sent = Seq.fill(2)(Seq.fill(4)(EponFrames.withFcs(Seq.fill(80 + rnd.nextInt(200))(rnd.nextInt(256)))))
    for (k <- 0 until 2) fork {
      for (f <- sent(k)) {
        for ((b, i) <- f.zipWithIndex) {
          io.onuData(k).valid #= true
          io.onuData(k).fragment.data #= b
          io.onuData(k).fragment.length #= f.length
          io.onuData(k).last #= i == f.length - 1
          cd.waitSamplingWhere(io.onuData(k).ready.toBoolean)
        }
        io.onuData(k).valid #= false
      }
    }
    // downstream data to ONU 0
    val ds = Seq.fill(3)(Seq.fill(80 + rnd.nextInt(100))(rnd.nextInt(256)))
    for (f <- ds) {
      for ((b, i) <- f.zipWithIndex) {
        io.dsData.valid #= true
        io.dsData.fragment.data #= b
        io.dsData.fragment.mode #= false
        io.dsData.fragment.fcs #= true
        io.dsData.fragment.llid #= llidOf(macs(0))
        io.dsData.last #= i == f.length - 1
        cd.waitSamplingWhere(io.dsData.ready.toBoolean)
      }
      io.dsData.valid #= false
    }
    cd.waitSampling(60000)
    for (k <- 0 until 2) {
      val got = usFrames.filter(_.llid == llidOf(macs(k)))
      check(got.forall(_.good) && got.map(_.bytes) == sent(k), s"ONU $k upstream: ${got.length} frames with LLID ${llidOf(macs(k))}")
    }
    def dataOf(k: Int) = onuFrames(k).filterNot(f => f(12) == 0x88 && f(13) == 0x08).map(_.dropRight(4)).toSeq
    check(dataOf(0) == ds && dataOf(1).isEmpty, "downstream to ONU 0's LLID: only ONU 0 accepts (MPCPDUs aside)")
    check(collisions == 0, "no upstream collision once registered")
    check(leads.length > 10 && leads.distinct.length == 1 && leads.head > 0 && leads.head <= 2 * resetLead && resetInLight == 0,
      s"receiver reset before each of ${leads.length} bursts, ${leads.head} clocks before the light (lead $resetLead TQ " +
        "less the receive latency in the RTT), never in one")

    // grants from the REPORTs
    io.reports(0) #= 0x0800
    io.reports(1) #= 0
    cd.waitSampling(60000)
    val (big, small) = (burstLengths(0).takeRight(5), burstLengths(1).takeRight(5))
    check(big.forall(_ == 1500 - laserOff) && small.forall(_ == 160 - laserOff),
      s"REPORT 0x800 TQ: bursts of maxGrant ($big), REPORT 0: minGrant ($small)")
    io.reports(0) #= 0

    // deregistrations
    events.clear()
    io.deregister.valid #= true; io.deregister.payload #= llidOf(macs(1)) - 0x10
    cd.waitSampling(); io.deregister.valid #= false
    waitFor("CPU deregisters ONU 1: DEREGISTERED, the ONU unregistered", 20000)(
      events.exists(e => e._1 == "DEREGISTERED" && e._3 == macs(1)) && !registered(1))
    waitFor("ONU 1 registers again at a discovery window", 400000)(registered(1))
    events.clear()
    io.onus(0).enable #= false
    waitFor("ONU 0 disabled: REGISTER_REQ deregister, DEREGISTERED", 40000)(
      events.exists(e => e._1 == "DEREGISTERED" && e._3 == macs(0)) && !registered(0))
    events.clear()
    o.timeout #= 4
    cd.waitSampling(40000)
    check(registered(1) && events.isEmpty, "timeout 4 x 1024 TQ: ONU 1 reporting stays registered")
    io.dark #= 2
    waitFor("ONU 1's fibre cut: DEREGISTERED by the timeout", 40000)(events.exists(e => e._1 == "DEREGISTERED" && e._3 == macs(1)))
    check(collisions == 0, "no collision")
  }
  println("PASS OltMpcpTest")
}
