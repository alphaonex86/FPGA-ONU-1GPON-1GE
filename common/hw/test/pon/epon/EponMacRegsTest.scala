// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.epon

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba3.apb.sim.Apb3Driver
import scala.collection.mutable
import pon.gpon.GtcDsTest.check

/** EponOltMac, a splitter and EponOnuMacs on fibres of their own (downstream, upstream clocks). */
case class EponMacPon(fibres: Seq[(Int, Int)]) extends Component {
  val n = fibres.length
  val apbConfig = Apb3Config(addressWidth = 8, dataWidth = 32)
  val io = new Bundle {
    val oltApb = slave(Apb3(apbConfig))
    val oltIrq = out Bool ()
    val dsData = slave Stream (Fragment(EponTxByte()))
    val usData = master Flow (Fragment(EponRxByte()))
    val usDataGood = out Bool ()
    val onuApb = Vec(slave(Apb3(apbConfig)), n)
    val onuIrq = out Bits (n bits)
    val onuData = Vec(slave(Stream(Fragment(EponUsByte()))), n)
    val onuRx = Vec(master(Flow(Fragment(EponRxByte()))), n)
    val collision = out Bool ()
  }
  val olt = EponOltMac()
  olt.io.apb <> io.oltApb
  io.oltIrq := olt.io.irq
  olt.io.dataTx << io.dsData
  io.usData << olt.io.dataRx
  io.usDataGood := olt.io.dataRxGood
  val lit = Bits(n bits)
  val codes = Vec(Bits(10 bits), n)
  val onus = for (((ds, us), k) <- fibres.zipWithIndex) yield new Area {
    val mac = EponOnuMac()
    mac.io.rxCode := Delay(olt.io.txCode, ds)
    mac.io.apb <> io.onuApb(k)
    io.onuIrq(k) := mac.io.irq
    mac.io.dataTx << io.onuData(k)
    mac.io.report := 600
    io.onuRx(k) << mac.io.dataRx
    lit(k) := Delay(RegNext(mac.io.burst) init (False), us, init = False)
    codes(k) := Delay(mac.io.txCode, us)
  }
  io.collision := CountOne(lit) > 1
  olt.io.rxCode := (0 until n).foldRight(B(0, 10 bits))((k, rest) => lit(k) ? codes(k) | rest)
}

/** The register setup of the EPON test CPUs: LLIDs from 0x10, a discovery window every 2048 TQ, grants of 160 to
  * 1500 TQ, laser on/off 16 TQ, sync 24 TQ; the ONUs always report. Test only. */
object EponCpu {
  val macs = Seq(BigInt("001122334401", 16), BigInt("001122334402", 16))
  val (laserOn, laserOff, sync, llidBase) = (16, 16, 24, 0x10)
  def olt(a: Apb3Driver, r: EponOltRegs): Unit = {
    a.write(r.MAC_HI.addr, 0x0A0B); a.write(r.MAC_LO.addr, 0x0C0D0E0FL)
    a.write(r.LLID.addr, llidBase)
    a.write(r.DISCOVERY.addr, 2 | (600 << 16))
    a.write(r.RANGE.addr, 700 | (sync << 16))
    a.write(r.SCHEDULE.addr, 4000 | (1300 << 16))
    a.write(r.GRANT.addr, 160 | (1500 << 16))
    a.write(r.OVERHEAD.addr, (laserOn + sync + laserOff + Mpcp.cost(Mpcp.pduBytes)) | (8 << 16))
    a.write(r.TIMEOUT.addr, 8)
    a.write(r.IRQ_ENABLE.addr, 3)
    a.write(r.CTRL.addr, 1)
  }
  def onu(a: Apb3Driver, r: EponOnuRegs, mac: BigInt): Unit = {
    a.write(r.MAC_HI.addr, mac >> 32); a.write(r.MAC_LO.addr, mac & 0xFFFFFFFFL)
    a.write(r.LASER.addr, laserOn | (laserOff << 8))
    a.write(r.TIMEOUT.addr, 3052)
    a.write(r.IRQ_ENABLE.addr, 3)
    a.write(r.CTRL.addr, 3) // enable, always report
  }
}

/** The EPON MACs run by their CPUs through the registers only: two ONUs registered (OLT events with their MACs, ONU
  * STATE with the LLID, both interrupts), data each way, frame counters, deregistration of an ONU by the OLT's CPU
  * (DEREGISTER: event, the ONU's EVENTS) and its registration again, no ERRORS bit. */
object EponMacRegsTest extends App {
  import EponCpu.macs
  val rnd = new scala.util.Random(11)
  SimConfig.compile(EponMacPon(Seq((37, 41), (530, 520)))).doSim { dut =>
    val io = dut.io
    val cd = dut.clockDomain
    cd.forkStimulus(8)
    io.dsData.valid #= false
    io.onuData.foreach(_.valid #= false)
    val olt = Apb3Driver(io.oltApb, cd)
    val onu = io.onuApb.map(Apb3Driver(_, cd))
    val (r, ro) = (dut.olt.regs, dut.onus.map(_.mac.regs).head)

    EponCpu.olt(olt, r)
    for ((a, k) <- onu.zipWithIndex) EponCpu.onu(a, ro, macs(k))

    val events = mutable.ArrayBuffer[(Int, Int, BigInt)]()
    def popEvents(): Unit = while (olt.read(r.STATUS.addr).testBit(0)) {
      val e0 = olt.read(r.EVENT_0.addr).toInt
      val mac = (olt.read(r.EVENT_1.addr) << 32) | olt.read(r.EVENT_2.addr)
      events += ((e0 & 3, (e0 >> 8) & 7, mac))
    }
    def polled() = { if (io.oltIrq.toBoolean) popEvents(); events }
    def onuState(k: Int) = onu(k).read(ro.STATE.addr)
    def onuRegistered(k: Int) = onuState(k).testBit(1)
    def waitFor(what: String, limit: Int)(cond: => Boolean): Unit = {
      var (t, done) = (0, cond)
      while (!done && t < limit) { cd.waitSampling(100); t += 100; done = cond }
      check(done, s"$what (${t / 2} TQ)")
    }
    waitFor("OLT interrupt: both REGISTERED events", 400000)(polled().count(_._1 == 0) == 2)
    check(events.map(e => e._3).toSet == macs.toSet, s"events of both MACs: ${events.map(e => (e._1, e._2, e._3.toString(16)))}")
    val entryOf = events.map(e => e._3 -> e._2).toMap
    check(io.onuIrq.toInt == 3, "both ONU interrupts: EVENTS.registered")
    for (k <- 0 until 2) {
      onu(k).write(ro.EVENTS.addr, 1)
      check(onuRegistered(k) && (onuState(k) >> 16) == 0x10 + entryOf(macs(k)), s"ONU $k STATE: registered, LLID ${onuState(k) >> 16}")
    }
    check(olt.read(r.STATE.addr) >> 16 == 3, "OLT STATE: entries 0 and 1 registered")
    check(io.oltIrq.toBoolean == false && io.onuIrq.toInt == 0, "interrupts cleared")

    val upstream = mutable.ArrayBuffer[(Int, Seq[Int], Boolean)]()
    val downstream = Seq.fill(2)(mutable.ArrayBuffer[Seq[Int]]())
    val cur = mutable.ArrayBuffer[Int]()
    val onuCur = Seq.fill(2)(mutable.ArrayBuffer[Int]())
    var collisions = 0
    cd.onSamplings {
      if (io.usData.valid.toBoolean) {
        cur += io.usData.fragment.data.toInt
        if (io.usData.last.toBoolean) { upstream += ((io.usData.fragment.llid.toInt, cur.toList, io.usDataGood.toBoolean)); cur.clear() }
      }
      for (k <- 0 until 2) if (io.onuRx(k).valid.toBoolean) {
        onuCur(k) += io.onuRx(k).fragment.data.toInt
        if (io.onuRx(k).last.toBoolean) { downstream(k) += onuCur(k).toList; onuCur(k).clear() }
      }
      if (io.collision.toBoolean) collisions += 1
    }
    val sent = Seq.fill(2)(Seq.fill(3)(EponFrames.withFcs(Seq.fill(80 + rnd.nextInt(200))(rnd.nextInt(256)))))
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
    val ds = Seq.fill(3)(Seq.fill(80 + rnd.nextInt(100))(rnd.nextInt(256)))
    for (f <- ds) {
      for ((b, i) <- f.zipWithIndex) {
        io.dsData.valid #= true
        io.dsData.fragment.data #= b
        io.dsData.fragment.mode #= false
        io.dsData.fragment.fcs #= true
        io.dsData.fragment.llid #= 0x10 + entryOf(macs(1))
        io.dsData.last #= i == f.length - 1
        cd.waitSamplingWhere(io.dsData.ready.toBoolean)
      }
      io.dsData.valid #= false
    }
    cd.waitSampling(60000)
    for (k <- 0 until 2) {
      val got = upstream.filter(_._1 == 0x10 + entryOf(macs(k)))
      check(got.forall(_._3) && got.map(_._2) == sent(k), s"ONU $k upstream: ${got.length} frames with its LLID")
    }
    check(downstream(1).map(_.dropRight(4)) == ds && downstream(0).isEmpty, "downstream to ONU 1's LLID: only ONU 1, no MAC Control")
    val (oltGood, onuGood) = (olt.read(r.COUNTS(0).addr), onu(1).read(ro.COUNTS(0).addr))
    check(oltGood > 6 && onuGood > 3 && olt.read(r.COUNTS(1).addr) == 0 && onu(1).read(ro.COUNTS(1).addr) == 0 &&
      onu(1).read(ro.COUNTS(2).addr) == 0, s"frame counters: OLT $oltGood good, ONU 1 $onuGood good, none bad")
    check(collisions == 0, "no upstream collision")

    events.clear()
    olt.write(r.DEREGISTER.addr, entryOf(macs(0)))
    waitFor("DEREGISTER: OLT event, ONU 0 interrupt and unregistered", 20000)(
      polled().exists(e => e._1 == 1 && e._3 == macs(0)) && io.onuIrq.toInt == 1 && !onuRegistered(0))
    onu(0).write(ro.EVENTS.addr, 1)
    events.clear()
    waitFor("ONU 0 registered again", 400000)(polled().exists(e => e._1 == 0 && e._3 == macs(0)))
    check(onuRegistered(0) && onuRegistered(1), "both ONUs registered")
    check(olt.read(r.ERRORS.addr) == 0 && onu.forall(_.read(ro.ERRORS.addr) == 0), "no ERRORS bit")
  }
  println("PASS EponMacRegsTest")
}
