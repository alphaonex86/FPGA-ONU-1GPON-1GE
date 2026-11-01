// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.core.sim._
import pon.gpon.GtcDsTest.check

/** OnuBoardCore management path: OnuMac registers across the clocks or an error while the ONU lane is down, laser
  * held off until both lanes are up, optics pin events, VCXO pin at the center duty. */
object OnuBoardTest extends App {
  val SN_0 = 0x04 // OnuRegs, read-write
  val BOARD = 0x100
  val OPTICS = BOARD + 0x10
  val STATUS = BOARD + 0x14
  val EVENTS = BOARD + 0x18
  val IRQ_ENABLE = BOARD + 0x1C
  val VCXO_CTRL = BOARD + 0x20
  val QUEUES_STATUS = 0x200 + 0x14 // GemCpuQueues STATUS: txFree

  SimConfig.compile(OnuBoardCore()).doSim { dut =>
    val io = dut.io
    val mgmt = dut.clockDomain
    mgmt.forkStimulus(10)
    for (c <- Seq(io.dsClk, io.usClk, io.rxClk, io.helperClk)) c #= false
    io.helperLocked #= true
    fork {
      var n = 0
      while (true) {
        sleep(2); io.dsClk #= !io.dsClk.toBoolean; io.rxClk #= io.dsClk.toBoolean; io.helperClk #= !io.helperClk.toBoolean
        if (n % 2 == 0) io.usClk #= !io.usClk.toBoolean
        n += 1
      }
    }
    for (lane <- Seq(io.onuLane, io.benLane); b <- Seq(lane.txResetAck, lane.rxResetAck, lane.txReady, lane.rxReady,
           lane.txPllLocked, lane.rxLockedToData, lane.rxLockedToRef)) b #= false
    for (b <- Seq(io.pins.rxSd, io.pins.burstCut, io.pins.scl.read, io.pins.sda.read, io.pins.benIo.read)) b #= false
    io.rxWord #= 0
    val hps = new HpsAxi(io.axi, mgmt)
    import hps._
    mgmt.waitSampling(20)
    def pinMean(): Double = {
      var high = 0
      for (_ <- 0 until 16000) { mgmt.waitSampling(); if (io.vcxoPwm.toBoolean) high += 1 }
      high / 16000.0
    }

    check(read(SN_0)._2 == SLVERR && read(QUEUES_STATUS)._2 == SLVERR, "MAC and GEM queues while the ONU lane is down: error answer")
    check(io.pins.laserDisable.toBoolean, "laser off at power-up")
    check(math.abs(pinMean() - 0.5) < 0.01, "VCXO pin at the center duty while the ONU clocks are down")

    io.onuLane.txReady #= true
    io.onuLane.rxReady #= true
    mgmt.waitSampling(20)
    check(write(SN_0, 0x47504F4EL) == 0 && read(SN_0) == (0x47504F4EL, 0), "MAC register across the clocks once up")
    val (queues, queuesResp) = read(QUEUES_STATUS)
    check(queuesResp == 0 && (queues & 0xFFFF) == 512, s"GEM queues once up: TX queue empty (${queues & 0xFFFF} words free)")
    write(OPTICS, 0)
    mgmt.waitSampling(4)
    check(io.pins.laserDisable.toBoolean, "laser still off while the burst enable lane is not ready")
    io.benLane.txReady #= true
    mgmt.waitSampling(6)
    check(!io.pins.laserDisable.toBoolean && !io.pins.benIo.writeEnable.toBoolean, "laser enabled, BEN_IO hi-Z")

    write(EVENTS, 0x7)
    write(IRQ_ENABLE, 0x1)
    io.pins.rxSd #= true
    mgmt.waitSampling(10)
    check((read(STATUS)._1 & 1) == 1 && io.boardIrq.toBoolean, "RX_SD rise: STATUS, EVENTS and the board interrupt")
    write(VCXO_CTRL, 0x40000000L)
    check(math.abs(pinMean() - 0.25) < 0.01, "VCXO pin follows the center register")

    io.onuLane.txReady #= false
    mgmt.waitSampling(20)
    check(read(SN_0)._2 == SLVERR && io.pins.laserDisable.toBoolean, "ONU lane down: error answer, laser off")
  }
  println("PASS OnuBoardTest")
}
