// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.collection.mutable
import GponCodes._

/** OLT MAC with an ONU downstream deframer on its line: what the ONU models read. Test only. */
case class OltMacLoop(maxOnus: Int) extends Component {
  val mac = OltMac(maxOnus)
  val onuRx = GtcDsDeframer()
  val io = new Bundle {
    val usClk = in Bool ()
    val usReset = in Bool ()
    val usLine = in Bits (8 bits)
    val time = out UInt (24 bits)
    val config = in(OltConfig())
    val cpuPloam = slave(cloneOf(mac.io.cpuPloam))
    val cpuUsPloam = master(cloneOf(mac.io.cpuUsPloam))
    val event = master(cloneOf(mac.io.event))
    val scheduleError = out Bool ()
    val dsPloam = master(cloneOf(onuRx.io.ploam))
    val dsAlloc = master(cloneOf(onuRx.io.alloc))
    val dsSynced = out Bool ()
  }
  val usDomain = ClockDomain(io.usClk, io.usReset)
  mac.io.usClk := io.usClk
  mac.io.usReset := io.usReset
  mac.io.gem.valid := False
  mac.io.gem.payload.assignDontCare()
  mac.io.release.valid := False
  mac.io.release.payload := 0
  mac.io.tcontWrite.valid := False
  mac.io.tcontWrite.payload.assignDontCare()
  mac.io.usLine := io.usLine
  io.time := mac.io.time
  mac.io.config := io.config
  mac.io.cpuPloam << io.cpuPloam
  io.cpuUsPloam << mac.io.cpuUsPloam
  io.event << mac.io.event
  io.scheduleError := mac.io.scheduleError
  onuRx.io.line := mac.io.dsLine
  io.dsPloam << onuRx.io.ploam
  io.dsAlloc << onuRx.io.alloc
  io.dsSynced := onuRx.io.synced
  mac.us.receiver.io.burst.simPublic()
  mac.us.scheduler.io.frameStart.simPublic()
}

/** The activation scenario of OltSchedulerTest end to end: BWmap and PLOAMs go through the downstream line and an
  * ONU deframer, across the 311.04 / 155.52 MHz clock domains. */
object OltMacTest extends App {
  val maxFrames = 140
  val act = new OltActivation(maxFrames)

  SimConfig.compile(OltMacLoop(8)).doSim { dut =>
    val ds = dut.clockDomain
    val us = dut.usDomain
    val io = dut.io
    act.configure(io.config)
    io.usLine #= 0
    io.cpuPloam.valid #= false

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
    us.onSamplings {
      val time = io.time.toInt
      act.sample(io.event, io.cpuUsPloam, dut.mac.us.receiver.io.burst)
      if (io.cpuPloam.valid.toBoolean && io.cpuPloam.ready.toBoolean) act.cpuQueue.dequeue()
      if (dut.mac.us.scheduler.io.frameStart.toBoolean) pending += time // the scheduler's frame time reference
      if (pending.nonEmpty && time == pending.head + handOver) {
        act.frame(pending.dequeue(), allocs.toList, ploam, time)
        allocs.clear()
        ploam = None
      }
      io.usLine #= act.line.byte(time + 1)
      val send = act.cpuQueue.nonEmpty && io.dsSynced.toBoolean // a PLOAM before the ONUs sync would be lost
      io.cpuPloam.valid #= send
      if (send) io.cpuPloam.payload #= act.cpuQueue.head
    }
    us.waitSamplingWhere(act.done)
    val settle = act.frames
    us.waitSamplingWhere(act.frames == settle + 15)
    act.checks(io.scheduleError.toBoolean)
    println(s"activation done in $settle frames")
  }
  println("PASS OltMacTest")
}
