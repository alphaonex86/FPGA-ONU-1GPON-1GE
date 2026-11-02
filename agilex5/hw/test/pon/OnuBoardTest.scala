// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.core.sim._
import pon.gpon.GtcDsTest.check

/** OnuBoardCore management path: OnuMac registers across the clocks or an error while the ONU lane is down, laser
  * held off until both lanes are up, optics pin events, VCXO pin at the center duty; an Ethernet frame through the
  * bridge to the MAC's upstream data stream. */
object OnuBoardTest extends App {
  val SN_0 = 0x04 // OnuRegs, read-write
  val BOARD = 0x100
  val OPTICS = BOARD + 0x10
  val STATUS = BOARD + 0x14
  val EVENTS = BOARD + 0x18
  val IRQ_ENABLE = BOARD + 0x1C
  val VCXO_CTRL = BOARD + 0x20
  val QUEUES_STATUS = 0x200 + 0x14 // GemCpuQueues STATUS: txFree
  val BRIDGE_VLAN_MAP = 0x304
  val BRIDGE_UNTAGGED = 0x308

  SimConfig.compile { val c = OnuBoardCore(); c.pon.mac.io.dataTx.simPublic(); c }.doSim { dut =>
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
    check(read(BRIDGE_UNTAGGED)._2 == SLVERR, "bridge while the Ethernet lane is down: error")
    io.ethDown #= false
    mgmt.waitSampling(20)
    check(write(BRIDGE_UNTAGGED, 0x200 | (1 << 12)) == 0, "bridge: untagged frames to Port-ID 0x200")
    check(write(BRIDGE_VLAN_MAP, 7 | (0x207 << 12) | (1 << 24)) == 0 && write(BRIDGE_VLAN_MAP, 8 | (0x208 << 12)) == 0,
      "bridge table writes back to back once the Ethernet lane is up: answered")
    for (i <- 0 until 100) {
      waitUntil(io.ethRxClk.toBoolean); waitUntil(!io.ethRxClk.toBoolean)
      io.eth.rx.valid #= true
      io.eth.rx.fragment #= (if (i == 12) 0x08 else i)
      io.eth.rx.last #= i == 99
    }
    waitUntil(io.ethRxClk.toBoolean); waitUntil(!io.ethRxClk.toBoolean)
    io.eth.rx.valid #= false
    mgmt.waitSampling(500)
    val data = dut.pon.mac.io.dataTx(0) // no allocation yet: the packet waits at the MAC
    check(data.valid.toBoolean && data.fragment.portId.toInt == 0x200 && data.fragment.length.toInt == 104,
      s"Ethernet frame -> bridge -> ONU MAC data stream: Port-ID 0x200, 100 bytes + FCS")
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
