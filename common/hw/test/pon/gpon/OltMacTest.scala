// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.apb.sim.Apb3Driver
import scala.collection.mutable
import GponCodes._
import GtcDsTest.bigToBytes

/** OLT MAC with an ONU downstream deframer on its line: what the ONU models read. Test only. */
case class OltMacLoop(maxOnus: Int) extends Component {
  val mac = OltMac(maxOnus)
  val onuRx = GtcDsDeframer()
  val io = new Bundle {
    val usClk = in Bool ()
    val usReset = in Bool ()
    val usLine = in Bits (8 bits)
    val time = out UInt (24 bits)
    val apb = slave(cloneOf(mac.io.apb))
    val irq = out Bool ()
    val dsPloam = master(cloneOf(onuRx.io.ploam))
    val dsAlloc = master(cloneOf(onuRx.io.alloc))
    val dsSynced = out Bool ()
  }
  val usDomain = ClockDomain(io.usClk, io.usReset)
  mac.io.usClk := io.usClk
  mac.io.usReset := io.usReset
  mac.io.gem.valid := False
  mac.io.gem.payload.assignDontCare()
  mac.io.usLine := io.usLine
  io.time := mac.io.time
  mac.io.apb <> io.apb
  io.irq := mac.io.irq
  onuRx.io.line := mac.io.dsLine
  io.dsPloam << onuRx.io.ploam
  io.dsAlloc << onuRx.io.alloc
  io.dsSynced := onuRx.io.synced
  mac.us.receiver.io.burst.simPublic()
  mac.us.scheduler.io.frameStart.simPublic()
}

/** The activation scenario of OltSchedulerTest end to end: a CPU model drives the OLT through its registers (APB,
  * interrupt), BWmap and PLOAMs go through the downstream line and an ONU deframer, across the 311.04 / 155.52 MHz
  * clock domains. */
object OltMacTest extends App {
  val maxFrames = 140
  val act = new OltActivation(maxFrames)

  SimConfig.compile(OltMacLoop(8)).doSim { dut =>
    val ds = dut.clockDomain
    val us = dut.usDomain
    val io = dut.io
    val r = dut.mac.us.regs
    io.usLine #= 0
    io.apb.PSEL #= 0
    io.apb.PENABLE #= false

    // same source: an upstream edge on every other downstream edge
    ds.fallingEdge(); us.fallingEdge()
    ds.assertReset(); us.assertReset()
    fork {
      var n = 0
      while (true) { ds.clockToggle(); if (n % 2 == 0) us.clockToggle(); n += 1; sleep(2) }
    }
    sleep(41)
    ds.deassertReset(); us.deassertReset()

    // ONU side: what the deframer read since the last frame, handed to the models once the PCBd is over
    val allocs = mutable.ArrayBuffer[Alloc]()
    var ploam: Option[BigInt] = None
    ds.onSamplings {
      if (io.dsAlloc.valid.toBoolean && io.dsAlloc.crcOk.toBoolean) {
        val x = io.dsAlloc.value
        allocs += Alloc(x.allocId.toInt, x.flags.toInt, x.start.toInt, x.stop.toInt)
      }
      if (io.dsPloam.valid.toBoolean && io.dsPloam.crcOk.toBoolean) ploam = Some(io.dsPloam.value.toBigInt)
    }
    val handOver = 1000 // upstream bytes after the frame start
    val pending = mutable.Queue[Int]() // frame start times waiting for their PCBd
    var lastBwmap = List[Alloc]()
    us.onSamplings {
      val time = io.time.toInt
      act.onBurst(dut.mac.us.receiver.io.burst)
      if (dut.mac.us.scheduler.io.frameStart.toBoolean) pending += time // the scheduler's frame time reference
      if (pending.nonEmpty && time == pending.head + handOver) {
        lastBwmap = allocs.toList
        act.frame(pending.dequeue(), lastBwmap, ploam, time)
        allocs.clear()
        ploam = None
      }
      io.usLine #= act.line.byte(time + 1)
    }

    // the CPU
    val apb = Apb3Driver(io.apb, us)
    def word(bytes: Seq[Int]): BigInt = bytes.foldLeft(BigInt(0))((a, b) => (a << 8) | b)
    val uo = act.upstreamOverhead
    apb.write(r.UO_0.addr, word(uo.slice(0, 4)))
    apb.write(r.UO_1.addr, word(uo.slice(4, 8)))
    apb.write(r.UO_2.addr, word(uo.slice(8, 10)))
    apb.write(r.TEQD.addr, act.teqd)
    apb.write(r.RTD_MIN.addr, act.rtdMin)
    apb.write(r.RANGING.addr, act.reach | (act.requestStart << 16))
    apb.write(r.WINDOW.addr, act.quietBefore | (act.snPeriod << 8))
    apb.write(r.GRANT.addr, act.grantBytes | (act.gapBytes << 16))
    apb.write(r.IRQ_ENABLE.addr, 3) // events, upstream PLOAMs
    apb.write(r.CTRL.addr, 1)
    var cpuOn = true
    val cpu = fork {
      while (cpuOn) {
        us.waitSamplingWhere(!cpuOn || io.irq.toBoolean || (act.cpuQueue.nonEmpty && io.dsSynced.toBoolean))
        val status = apb.read(r.STATUS.addr)
        if (status.testBit(0)) {
          val e0 = apb.read(r.EVENT_0.addr)
          val sn = (apb.read(r.EVENT_1.addr) << 32) | apb.read(r.EVENT_2.addr)
          val eqd = apb.read(r.EVENT_3.addr).toLong // pops
          act.onEvent(Seq("SN_FOUND", "RANGED", "RANGING_FAILED", "NO_FREE_ID")((e0 & 3).toInt), (e0 >> 2).toInt & 0xFF,
            sn, eqd)
        }
        if (status.testBit(1)) {
          val m = (apb.read(r.PLOAM_RX_0.addr) << 64) | (apb.read(r.PLOAM_RX_1.addr) << 32) | apb.read(r.PLOAM_RX_2.addr)
          act.cpuUs += m
        }
        if (act.cpuQueue.nonEmpty && io.dsSynced.toBoolean && !status.testBit(2)) { // a PLOAM before sync is lost
          val b = bigToBytes(act.cpuQueue.dequeue(), 12)
          apb.write(r.PLOAM_TX_0.addr, word(b.slice(0, 4)))
          apb.write(r.PLOAM_TX_1.addr, word(b.slice(4, 8)))
          apb.write(r.PLOAM_TX_2.addr, word(b.slice(8, 12)))
        }
      }
    }
    us.waitSamplingWhere(act.done)
    val settle = act.frames
    us.waitSamplingWhere(act.frames == settle + 15)
    cpuOn = false
    cpu.join() // the bus is free
    val errors = apb.read(r.ERRORS.addr)
    act.checks(errors.testBit(0))
    GtcDsTest.check(errors == 0, s"no register error ($errors)")

    // a T-CONT through the registers: 100 fixed bytes for A, in its burst
    apb.write(r.TCONT_A.addr, 0 | (300 << 8) | (act.a.onuId << 20))
    apb.write(r.TCONT_B.addr, 100 | (100 << 16))
    apb.write(r.DBA.addr, 1 | (48 << 8))
    apb.write(r.WINDOW.addr, act.quietBefore | (0xFFFF << 8)) // no more discovery windows
    val f = act.frames
    us.waitSamplingWhere(act.frames == f + 6)
    val t = lastBwmap.find(_.allocId == 300)
    val default = lastBwmap.find(_.allocId == act.a.onuId)
    GtcDsTest.check(t.exists(x => x.stop - x.start + 1 == 100 && default.exists(_.stop + 1 == x.start)),
      s"T-CONT written by the CPU granted after A's default allocation ($lastBwmap)")
    println(s"activation done in $settle frames")
  }
  println("PASS OltMacTest")
}
