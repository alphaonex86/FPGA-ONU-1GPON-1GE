// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.epon

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba3.apb.sim.Apb3Driver
import pon.{EthFrames, GemEthBridge}
import pon.gpon.EthSide
import pon.gpon.GtcDsTest.check

/** EponOltMac and an EponOnuMac over a fibre, each with its Ethernet bridge through EponGem (the OLT bridge's Port-ID
  * picks the LLID, the ONU's four queues into its grants). Test only. */
case class EponBridgeLoop(ds: Int, us: Int) extends Component {
  val apbConfig = Apb3Config(addressWidth = 8, dataWidth = 32)
  val io = new Bundle {
    val oltApb, onuApb, oltBridgeApb, onuBridgeApb = slave(Apb3(apbConfig))
    val ethClk = in Bool ()
    val oltEth, onuEth = master(EthFrames())
  }
  val ethDomain = ClockDomain(io.ethClk, ClockDomain.current.readResetWire)
  val pon = ClockDomain.current
  val olt = EponOltMac()
  val onu = EponOnuMac()
  olt.io.apb <> io.oltApb
  onu.io.apb <> io.onuApb
  onu.io.rxCode := Delay(olt.io.txCode, ds)
  val lit = Delay(RegNext(onu.io.burst) init (False), us, init = False)
  olt.io.rxCode := lit ? Delay(onu.io.txCode, us) | B(0, 10 bits)

  val oltBridge = GemEthBridge(pon, pon, pon, ethDomain, ethDomain, defaultAction = 0)
  val (oltRx, oltCut) = EponGem.toGem(olt.io.dataRx, olt.io.dataRxGood)
  oltBridge.io.ponRx << oltRx
  oltBridge.io.ponRxCut := oltCut
  olt.io.dataTx << EponGem.toFrames(oltBridge.io.ponTx(0))
  val onuBridge = GemEthBridge(pon, pon, pon, ethDomain, ethDomain, defaultAction = 0, ponQueues = 4)
  val (onuRx, onuCut) = EponGem.toGem(onu.io.dataRx, onu.io.dataRxGood)
  onuBridge.io.ponRx << onuRx
  onuBridge.io.ponRxCut := onuCut
  val (onuTx, report) = EponGem.toUpstream(onuBridge.io.ponTx, onuBridge.io.ponTxBacklog)
  onu.io.dataTx << onuTx
  onu.io.report := report
  for ((b, apb, eth) <- Seq((oltBridge, io.oltBridgeApb, io.oltEth), (onuBridge, io.onuBridgeApb, io.onuEth))) {
    b.io.cpuPort.setIdle()
    b.io.cpuToLan.setIdle()
    b.io.cpuToWan.setIdle()
    b.io.apb <> apb
    eth <> b.io.eth
  }
}

/** EPON with the Ethernet bridges, MACs and bridges set by their CPUs: once the ONU is registered, untagged frames
  * OLT port -> its LLID -> ONU port and ONU port -> OLT port (queue 0), FCS checked and stripped at each end; VLAN 10
  * frames to the broadcast Port-ID (LLID 0x7FFF, mode 1) reach the ONU port; a VLAN 20 frame of the ONU in queue 2
  * goes up in our grants; no bridge drop. */
object EponBridgeTest extends App {
  SimConfig.compile(EponBridgeLoop(37, 41)).doSim { dut =>
    val io = dut.io
    val cd = dut.clockDomain
    cd.forkStimulus(8)
    val eth = ClockDomain(io.ethClk)
    eth.forkStimulus(8)
    val olt = Apb3Driver(io.oltApb, cd)
    val onu = Apb3Driver(io.onuApb, cd)
    val oltBridge = Apb3Driver(io.oltBridgeApb, cd)
    val onuBridge = Apb3Driver(io.onuBridgeApb, cd)
    val oltEth = new EthSide(io.oltEth, eth)
    val onuEth = new EthSide(io.onuEth, eth)
    val (r, ro, ob, nb) = (dut.olt.regs, dut.onu.regs, dut.oltBridge, dut.onuBridge)
    cd.waitSampling(10)
    EponCpu.olt(olt, r)
    EponCpu.onu(onu, ro, EponCpu.macs(0))
    var t = 0
    while (!onu.read(ro.STATE.addr).testBit(1) && t < 200000) { cd.waitSampling(1000); t += 1000 }
    val llid = (onu.read(ro.STATE.addr) >> 16).toInt
    check(onu.read(ro.STATE.addr).testBit(1) && llid == EponCpu.llidBase, s"ONU registered, LLID $llid (${t / 2} TQ)")

    oltBridge.write(ob.UNTAGGED.addr, llid | (1 << 12))                      // OLT port -> the ONU's LLID
    oltBridge.write(ob.PORT_ACTION.addr, llid | (2 << 12))                   // the ONU's frames -> OLT port
    oltBridge.write(ob.VLAN_MAP.addr, 10 | (EponGem.broadcastPort << 12) | (1 << 24))
    onuBridge.write(nb.UNTAGGED.addr, llid | (1 << 12))
    onuBridge.write(nb.PORT_ACTION.addr, llid | (2 << 12))                   // our LLID -> ONU port
    onuBridge.write(nb.PORT_ACTION.addr, EponGem.broadcastPort | (2 << 12))  // broadcast -> ONU port
    onuBridge.write(nb.VLAN_MAP.addr, 20 | (0x020 << 12) | (1 << 24))
    onuBridge.write(nb.PORT_QUEUE.addr, 0x020 | (2 << 12))

    val down = Seq(List.tabulate(1000)(i => (i * 5 + 1) & 0xFF), List.tabulate(64)(i => (i * 11) & 0xFF))
    val up = Seq(List.tabulate(64)(i => (i * 3 + 7) & 0xFF), List.tabulate(1400)(i => (i * 7 + 2) & 0xFF))
    oltEth.toSend ++= down
    onuEth.toSend ++= up
    cd.waitSampling(40000)
    check(onuEth.received.toList == down.toList && oltEth.received.toList == up.toList,
      s"untagged frames end to end: ${down.map(_.length)} OLT -> ONU, ${up.map(_.length)} ONU -> OLT")
    val vlan10 = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 0x81, 0x00, 0x00, 10) ++ List.tabulate(84)(_ & 0xFF)
    val vlan20 = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 0x81, 0x00, 0x00, 20) ++ List.tabulate(200)(i => (i * 9) & 0xFF)
    oltEth.toSend += vlan10
    onuEth.toSend += vlan20
    cd.waitSampling(40000)
    check(onuEth.received.toList == down.toList :+ vlan10, "VLAN 10 to the broadcast LLID: received by the ONU")
    check(oltEth.received.toList == up.toList :+ vlan20, "VLAN 20 in ONU queue 2: up in our grants")
    check(oltBridge.read(ob.DROPS.addr) == 0 && onuBridge.read(nb.DROPS.addr) == 0, "no bridge drop")
  }
  println("PASS EponBridgeTest")
}
