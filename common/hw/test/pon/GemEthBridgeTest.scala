// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.apb.sim.Apb3Driver
import scala.collection.mutable
import pon.gpon.GtcDsTest.check

/** GemEthBridge, four clocks: PON -> Ethernet by Port-ID action (fragmented frames, bad FCS, CPU, unknown port, cut),
  * Ethernet -> PON by VLAN ID (802.1Q, 802.1ad, untagged, unmapped, bad frame) and PCP mapper, FCS checked, stripped,
  * appended; two queues by Port-ID, a full one dropping while the other flows. */
object GemEthBridgeTest extends App {
  val PORT_ACTION = 0x00
  val VLAN_MAP = 0x04
  val UNTAGGED = 0x08
  val DROPS = 0x0C
  val PCP_MAP = 0x10
  val PORT_QUEUE = 0x14
  case class Bench() extends Component {
    val ponRx, ponTx, ethTx, ethRx = ClockDomain.external("", withReset = false)
    def domain(cd: ClockDomain) = cd.copy(reset = ClockDomain.current.readResetWire, config = ClockDomain.current.config)
    val bridge = GemEthBridge(domain(ponRx), domain(ponRx), domain(ponTx), domain(ethTx), domain(ethRx), defaultAction = 0,
      ponQueues = 2, queueBytes = 4096)
    val io = bridge.io.toIo()
  }
  val rnd = new scala.util.Random(8)
  def frame(n: Int, tag: Option[(Int, Int)] = None): Seq[Int] = {
    val head = Seq.fill(12)(rnd.nextInt(256))
    val vlan = tag.toSeq.flatMap { case (tpid, vid) => Seq(tpid >> 8, tpid & 0xFF, vid >> 8, vid & 0xFF) }
    head ++ vlan ++ Seq(0x08, 0x00) ++ Seq.fill(n - head.length - vlan.length - 2)(rnd.nextInt(256))
  }
  def withFcs(f: Seq[Int]): Seq[Int] = { val c = Crc32.model(f); f ++ (0 until 4).map(i => (c >> (8 * i) & 0xFF).toInt) }

  SimConfig.compile(Bench()).doSim { dut =>
    val io = dut.io
    val mgmt = dut.clockDomain
    val Seq(ponRx, ponTx, ethTx, ethRx) = Seq(dut.ponRx, dut.ponTx, dut.ethTx, dut.ethRx)
    mgmt.forkStimulus(10)
    for ((cd, p) <- Seq(ponRx -> 3, ponTx -> 7, ethTx -> 8, ethRx -> 8)) cd.forkStimulus(p)
    io.ponRx.valid #= false
    io.ponRxCut #= false
    io.cpuPort.valid #= true
    io.cpuPort.payload #= 0x0FE // OMCI: CPU without a table entry
    io.eth.rx.valid #= false
    io.eth.rxBad #= false
    for (q <- io.ponTx) q.ready #= true
    io.eth.tx.ready #= true
    val apb = Apb3Driver(io.apb, mgmt)
    mgmt.waitSampling(20)
    apb.write(PORT_ACTION, 0x100 | (2 << 12))
    apb.write(PORT_ACTION, 0x101 | (1 << 12))
    apb.write(VLAN_MAP, 100 | (0x200 << 12) | (1 << 24))
    apb.write(VLAN_MAP, 7 | (0x202 << 12)) // listed, not valid
    apb.write(UNTAGGED, 0x201 | (1 << 12))
    apb.write(VLAN_MAP, 50 | (3 << 12) | (1 << 24) | (1 << 25)) // mapper 3
    apb.write(PCP_MAP, 0x205 | (5 << 12) | (3 << 15))
    apb.write(PCP_MAP, 0x206 | (0 << 12) | (3 << 15))
    apb.write(PORT_QUEUE, 0x205 | (1 << 12))
    mgmt.waitSampling(50)

    val ethOut = mutable.ArrayBuffer[Seq[Int]]()
    val ponOut = Seq.fill(2)(mutable.ArrayBuffer[(Int, Int, Seq[Int])]()) // per queue
    val cpuOut = mutable.ArrayBuffer[(Int, Int)]()
    var cur = mutable.ArrayBuffer[Int]()
    ethTx.onSamplings {
      if (io.eth.tx.valid.toBoolean && io.eth.tx.ready.toBoolean) {
        cur += io.eth.tx.fragment.toInt
        if (io.eth.tx.last.toBoolean) { ethOut += cur.toSeq; cur = mutable.ArrayBuffer[Int]() }
      }
    }
    val pcur = Seq.fill(2)(mutable.ArrayBuffer[Int]())
    var gaps = 0
    ponTx.onSamplings {
      for ((t, k) <- io.ponTx.zipWithIndex) {
        if (t.valid.toBoolean && t.ready.toBoolean) {
          pcur(k) += t.fragment.data.toInt
          if (t.last.toBoolean) { ponOut(k) += ((t.fragment.portId.toInt, t.fragment.length.toInt, pcur(k).toSeq)); pcur(k).clear() }
        } else if (pcur(k).nonEmpty && t.ready.toBoolean) gaps += 1
      }
    }
    ponRx.onSamplings { if (io.toCpu.valid.toBoolean) cpuOut += ((io.toCpu.gem.portId.toInt, io.toCpu.gem.data.toInt)) }

    // PON -> Ethernet: (Port-ID, GEM fragments), the end-of-frame PTI on the last
    def gem(port: Int, fragments: Seq[Seq[Int]], cutAfter: Boolean = false): Unit = {
      for ((f, k) <- fragments.zipWithIndex) {
        for ((b, i) <- f.zipWithIndex) {
          io.ponRx.valid #= true
          io.ponRx.allocId #= 0
          io.ponRx.gem.portId #= port
          io.ponRx.gem.pti #= (if (k == fragments.length - 1) 1 else 0)
          io.ponRx.gem.data #= b
          io.ponRx.last #= i == f.length - 1 && !(cutAfter && k == fragments.length - 1)
          ponRx.waitSampling()
        }
        io.ponRx.valid #= false
        ponRx.waitSampling(5)
      }
      if (cutAfter) { io.ponRxCut #= true; ponRx.waitSampling(); io.ponRxCut #= false }
      ponRx.waitSampling(5)
    }
    val good1 = withFcs(frame(100))
    val good2 = withFcs(frame(1500, Some((0x8100, 5))))
    val badFcs = withFcs(frame(80)).updated(40, 0)
    val toCpu = withFcs(frame(64))
    gem(0x100, Seq(good1))
    gem(0x100, Seq(good2.take(700), good2.slice(700, 1200), good2.drop(1200)))
    gem(0x100, Seq(badFcs))
    gem(0x101, Seq(toCpu))
    gem(0x0FE, Seq(toCpu))
    gem(0x300, Seq(withFcs(frame(70))))
    gem(0x100, Seq(withFcs(frame(90)).take(50)), cutAfter = true)
    gem(0x100, Seq(withFcs(frame(60)).take(40))) // runt
    mgmt.waitSampling(3000)
    check(ethOut == Seq(good1.dropRight(4), good2.dropRight(4)), s"PON -> Ethernet: ${ethOut.length} frames, fragments joined, FCS stripped")
    check(cpuOut.toSeq == toCpu.map((0x101, _)) ++ toCpu.map((0x0FE, _)), "CPU action and the OMCI Port-ID: to the CPU stream")

    // Ethernet -> PON
    def send(f: Seq[Int], bad: Boolean = false): Unit = {
      for ((b, i) <- f.zipWithIndex) {
        io.eth.rx.valid #= true
        io.eth.rx.fragment #= b
        io.eth.rx.last #= i == f.length - 1
        io.eth.rxBad #= bad && i == f.length - 1
        ethRx.waitSampling()
      }
      io.eth.rx.valid #= false
      io.eth.rxBad #= false
      ethRx.waitSampling(20) // inter-frame gap
    }
    val q = frame(200, Some((0x8100, 100)))
    val ad = frame(64, Some((0x88A8, 100)))
    val untagged = frame(1514)
    val pcp5 = frame(100, Some((0x8100, (5 << 13) | 50)))
    val pcp0 = frame(100, Some((0x8100, 50)))
    send(q); send(ad); send(untagged); send(frame(80, Some((0x8100, 7)))); send(frame(80, Some((0x8100, 9)))); send(frame(90), bad = true)
    send(pcp5); send(pcp0)
    mgmt.waitSampling(3000)
    check(ponOut(0).toSeq == Seq((0x200, 204, withFcs(q)), (0x200, 68, withFcs(ad)), (0x201, 1518, withFcs(untagged)),
      (0x206, 104, withFcs(pcp0))) && gaps == 0, s"Ethernet -> PON: ${ponOut(0).length} packets, Port-ID by VLAN or untagged default, FCS appended, no gap")
    check(ponOut(1).toSeq == Seq((0x205, 104, withFcs(pcp5))), "mapper: Port-ID by PCP, Port-ID 0x205 in queue 1")

    // queue 1 stalled (no grant): it fills and drops, queue 0 still flows
    io.ponTx(1).ready #= false
    val stalled = Seq.fill(3)(frame(1000, Some((0x8100, (5 << 13) | 50))))
    stalled.foreach(send(_))
    val meanwhile = frame(200)
    send(meanwhile)
    mgmt.waitSampling(3000)
    check(ponOut(0).last == ((0x201, 204, withFcs(meanwhile))) && ponOut(1).length == 1, "queue 1 stalled: queue 0 still flows")
    io.ponTx(1).ready #= true
    mgmt.waitSampling(3000)
    check(ponOut(1).drop(1).toSeq == stalled.take(2).map(f => (0x205, 1004, withFcs(f))), "queue 1 released: the 2 frames that fitted")
    val drops = apb.read(DROPS).toLong
    check((drops & 0xFFFF) == 3 && (drops >> 16) == 4, s"drops counted: PON -> Ethernet ${drops & 0xFFFF} (bad FCS, cut, " +
      s"runt; not the drop action), Ethernet -> PON ${drops >> 16} (invalid, unmapped, bad, queue full)")
  }
  println("PASS GemEthBridgeTest")
}
