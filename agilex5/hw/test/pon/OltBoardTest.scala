// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.core.sim._
import pon.gpon.GtcDsTest.check

/** OltBoardCore management path: HPS bridge -> board registers and, across to the PON clocks, the OltMac registers;
  * an error answer while the lane is down, pins and pin events; an Ethernet frame through the bridge to the MAC. */
object OltBoardTest extends App {
  val BOARD = 0x100
  val GTS_STATUS = BOARD + 4
  val SFP_CTRL = BOARD + 8
  val STATUS = BOARD + 12
  val EVENTS = BOARD + 16
  val IRQ_ENABLE = BOARD + 20
  val UO_0 = 0x04 // OltRegs, read-write
  val GEMQ_STATUS = 0x214
  val BRIDGE_UNTAGGED = 0x308

  SimConfig.compile { val c = OltBoardCore(); c.pon.mac.io.gem.simPublic(); c }.doSim { dut =>
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
    io.ethDown #= true
    io.eth.tx.ready #= true
    io.eth.rx.valid #= false
    io.eth.rxBad #= false
    io.ethTxClk #= false
    io.ethRxClk #= false
    fork { while (true) { sleep(4); io.ethTxClk #= !io.ethTxClk.toBoolean; io.ethRxClk #= !io.ethRxClk.toBoolean } }
    val hps = new HpsAxi(io.axi, mgmt)
    import hps._
    mgmt.waitSampling(20)

    check(read(UO_0)._2 == SLVERR, "MAC register while the lane is down: error answer, no hang")
    check(read(GEMQ_STATUS)._2 == SLVERR && read(BRIDGE_UNTAGGED)._2 == SLVERR, "GEM queues and bridge while down: errors")
    check(io.sfp.txDisable.toBoolean, "OLT laser off at power-up")
    check((read(STATUS)._1 & 8) == 0, "STATUS.alive low")

    io.lane.txReady #= true
    mgmt.waitSampling(20)
    check((read(STATUS)._1 & 8) != 0, "STATUS.alive once the lane is ready")
    check((read(GTS_STATUS)._1 & 4) != 0, "GTS_STATUS.txReady")
    check(write(UO_0, 0x12345678L) == 0 && read(UO_0) == (0x12345678L, 0), "MAC register written and read across the clocks")
    check(read(GEMQ_STATUS) == (512L, 0), "GEM queues: TX queue empty, 512 words")
    check(read(BRIDGE_UNTAGGED)._2 == SLVERR, "bridge while the Ethernet lane is down: error")
    io.ethDown #= false
    mgmt.waitSampling(20)
    check(write(BRIDGE_UNTAGGED, 0x123 | (1 << 12)) == 0, "bridge: untagged frames to Port-ID 0x123")
    val mac = dut.pon.mac.io.gem
    var gem = Seq[(Int, Int)]() // Port-ID, length of each packet start
    fork { // sampled on the downstream clock's rising edges
      var first = true
      while (true) {
        waitUntil(!io.dsClk.toBoolean); waitUntil(io.dsClk.toBoolean)
        if (mac.valid.toBoolean && mac.ready.toBoolean) {
          if (first) gem :+= ((mac.fragment.portId.toInt, mac.fragment.length.toInt))
          first = mac.last.toBoolean
        }
      }
    }
    for (i <- 0 until 100) {
      waitUntil(io.ethRxClk.toBoolean); waitUntil(!io.ethRxClk.toBoolean)
      io.eth.rx.valid #= true
      io.eth.rx.fragment #= (if (i == 12) 0x08 else i)
      io.eth.rx.last #= i == 99
    }
    waitUntil(io.ethRxClk.toBoolean); waitUntil(!io.ethRxClk.toBoolean)
    io.eth.rx.valid #= false
    mgmt.waitSampling(500)
    check(gem == Seq((0x123, 104)), s"Ethernet frame -> bridge -> OLT MAC: Port-ID 0x123, 100 bytes + FCS ($gem)")

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
