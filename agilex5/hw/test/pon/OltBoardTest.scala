// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.core.sim._
import pon.gpon.GtcDsTest.check

/** OltBoardCore management path: HPS bridge -> board registers and, across to the PON clocks, the OltMac registers;
  * an error answer while the lane is down, pins and pin events. */
object OltBoardTest extends App {
  val BOARD = 0x100
  val GTS_STATUS = BOARD + 4
  val SFP_CTRL = BOARD + 8
  val STATUS = BOARD + 12
  val EVENTS = BOARD + 16
  val IRQ_ENABLE = BOARD + 20
  val UO_0 = 0x04 // OltRegs, read-write

  SimConfig.compile(OltBoardCore()).doSim { dut =>
    val io = dut.io
    val mgmt = dut.clockDomain
    mgmt.forkStimulus(10)
    io.dsClk #= false
    io.usClk #= false
    fork { // 311.04 and 155.52 MHz, same source
      var n = 0
      while (true) { sleep(2); io.dsClk #= !io.dsClk.toBoolean; if (n % 2 == 0) io.usClk #= !io.usClk.toBoolean; n += 1 }
    }
    for (b <- Seq(io.lane.txResetAck, io.lane.rxResetAck, io.lane.txReady, io.lane.rxReady, io.lane.txPllLocked,
           io.lane.rxLockedToData, io.lane.rxLockedToRef, io.sfp.modAbs, io.sfp.los, io.sfp.txFault,
           io.sfp.scl.read, io.sfp.sda.read)) b #= false
    io.rxWord #= 0
    io.refclkFail #= 0
    val axi = io.axi
    axi.aw.valid #= false
    axi.w.valid #= false
    axi.ar.valid #= false
    axi.b.ready #= true
    axi.r.ready #= true
    mgmt.waitSampling(20)

    def write(address: Int, data: Long): Int = {
      axi.aw.addr #= address
      axi.aw.id #= 1
      axi.aw.len #= 0
      axi.aw.size #= 2
      axi.aw.valid #= true
      axi.w.data #= data
      axi.w.strb #= 0xF
      axi.w.last #= true
      axi.w.valid #= true
      var (awDone, wDone) = (false, false)
      while (!(awDone && wDone)) {
        mgmt.waitSampling()
        if (axi.aw.valid.toBoolean && axi.aw.ready.toBoolean) { awDone = true; axi.aw.valid #= false }
        if (axi.w.valid.toBoolean && axi.w.ready.toBoolean) { wDone = true; axi.w.valid #= false }
      }
      mgmt.waitSamplingWhere(1000)(axi.b.valid.toBoolean)
      axi.b.resp.toInt
    }
    def read(address: Int): (Long, Int) = {
      axi.ar.addr #= address
      axi.ar.id #= 2
      axi.ar.len #= 0
      axi.ar.size #= 2
      axi.ar.valid #= true
      mgmt.waitSamplingWhere(1000)(axi.ar.ready.toBoolean)
      axi.ar.valid #= false
      mgmt.waitSamplingWhere(1000)(axi.r.valid.toBoolean)
      (axi.r.data.toLong, axi.r.resp.toInt)
    }
    val SLVERR = 2

    check(read(UO_0)._2 == SLVERR, "MAC register while the lane is down: error answer, no hang")
    check(io.sfp.txDisable.toBoolean, "OLT laser off at power-up")
    check((read(STATUS)._1 & 8) == 0, "STATUS.alive low")

    io.lane.txReady #= true
    mgmt.waitSampling(20)
    check((read(STATUS)._1 & 8) != 0, "STATUS.alive once the lane is ready")
    check((read(GTS_STATUS)._1 & 4) != 0, "GTS_STATUS.txReady")
    check(write(UO_0, 0x12345678L) == 0 && read(UO_0) == (0x12345678L, 0), "MAC register written and read across the clocks")

    check(write(SFP_CTRL, 0x4) == 0, "SFP_CTRL: laser on, RS0 high")
    mgmt.waitSampling(2)
    check(!io.sfp.txDisable.toBoolean && io.sfp.rs0.writeEnable.toBoolean && io.sfp.rs0.write.toBoolean &&
      !io.sfp.rs1.writeEnable.toBoolean, "TX_DIS low, RS0 driven high, RS1 hi-Z")
    write(EVENTS, 0xF)
    write(IRQ_ENABLE, 0x1)
    io.sfp.modAbs #= true
    mgmt.waitSampling(10)
    check((read(EVENTS)._1 & 1) != 0 && io.boardIrq.toBoolean, "module removal: EVENTS.modAbs and the board interrupt")
    write(EVENTS, 0x1)
    mgmt.waitSampling(2)
    check(!io.boardIrq.toBoolean, "EVENTS cleared by writing 1")

    io.lane.txReady #= false
    mgmt.waitSampling(20)
    check(read(UO_0)._2 == SLVERR && io.sfp.txDisable.toBoolean, "lane down again: error answer, laser forced off")
  }
  println("PASS OltBoardTest")
}
