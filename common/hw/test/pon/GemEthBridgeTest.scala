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
  * appended; two queues by Port-ID, a full one dropping while the other flows; VLAN treatments each way (translate,
  * pop, S-tag push); table writes back to back across the clocks; offloaded flows each way (NAPT, PPPoE added and
  * stripped, VLAN pushed and popped, own Port-ID and queue), a port or a TTL of 1 missing them; in flows mode the
  * misses to the CPU each way, the CPU's own frames out of both sides; a snapshot of a flow's lookup; NAPT, L2 and
  * VLAN bypassed to the CPU; trap actions (L2-bridged ARP to the control queue, to the data queue, dropped). */
object GemEthBridgeTest extends App {
  val PORT_ACTION = 0x00
  val VLAN_MAP = 0x04
  val UNTAGGED = 0x08
  val DROPS = 0x0C
  val PCP_MAP = 0x10
  val PORT_QUEUE = 0x14
  val US_TREATMENT = 0x18
  val DS_VLAN = 0x1C
  val DS_TREATMENT = 0x20
  val FLOW_PORTS = 0x24
  val FLOW_DST = 0x28
  val FLOW_SRC = 0x2C
  val FLOW_L2 = 0x30
  val FLOW_NAT_IP = 0x34
  val FLOW_NAT = 0x38
  val FLOW_WRITE = 0x3C
  val EGRESS_DA = 0x40
  val EGRESS_MAC_HI = 0x44
  val EGRESS_SA = 0x48
  val EGRESS_VLAN = 0x4C
  val EGRESS_PORT = 0x50
  val EGRESS_WRITE = 0x54
  val FLOW_HIT_QUERY = 0x58
  val FLOW_HIT = 0x5C
  val FLOW_DST_1 = 0x60 // _2, _3 follow, then FLOW_SRC_1-3
  val CONTROL_RATE = 0x78
  val CONTROL_DROPS = 0x7C
  val BYPASS = 0x80
  val TRAP_LAN = 0x84
  val TRAP_WAN = 0x88
  val SNAP_ARM = 0x8C
  val SNAP_PORTS = 0x90 // SRC, DST, L2, RESULT, ACTION follow
  import FlowRewriteTest.{be, l4Sum, sum, words}
  /** An IPv4 TCP frame with valid checksums. */
  def ipFrame(macs: (Long, Long), tags: Seq[Int], session: Option[Int], src: Long, dst: Long, sport: Int, dport: Int,
              ttl: Int, payload: Seq[Int]): Seq[Int] = {
    val tcpRaw = be(sport, 2) ++ be(dport, 2) ++ be(0x01020304, 4) ++ be(0, 4) ++ Seq(0x50, 0x10) ++ be(1024, 2) ++
      be(0, 4) ++ payload
    val ipRaw = Seq(0x45, 0) ++ be(20 + tcpRaw.length, 2) ++ be(0x1111, 2) ++ be(0x4000, 2) ++ Seq(ttl, 6) ++ be(0, 2) ++
      be(src, 4) ++ be(dst, 4)
    val ip = ipRaw.patch(10, be(~sum(words(ipRaw)) & 0xFFFF, 2), 2)
    val tcp = tcpRaw.patch(16, be(~l4Sum(ip, tcpRaw) & 0xFFFF, 2), 2)
    be(macs._1, 6) ++ be(macs._2, 6) ++ tags ++ (session match {
      case Some(id) => Seq(0x88, 0x64, 0x11, 0x00) ++ be(id, 2) ++ be(ip.length + tcp.length + 2, 2) ++ Seq(0x00, 0x21)
      case None => Seq(0x08, 0x00)
    }) ++ ip ++ tcp
  }
  def treatment(index: Int, remove: Boolean, add: Boolean, pcpCopy: Boolean, sTag: Boolean, tci: Int): Int =
    index | (remove.compare(false) << 4) | (add.compare(false) << 5) | (pcpCopy.compare(false) << 6) | (sTag.compare(false) << 7) | (tci << 8)
  def rewritten(f: Seq[Int], remove: Boolean, tag: Seq[Int]): Seq[Int] = f.take(12) ++ tag ++ f.drop(if (remove) 16 else 12)
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
    io.cpuToLan.valid #= false
    io.cpuToWan.valid #= false
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
    apb.write(US_TREATMENT, treatment(1, remove = true, add = true, pcpCopy = true, sTag = false, tci = 30)) // translate
    apb.write(US_TREATMENT, treatment(2, remove = true, add = false, pcpCopy = false, sTag = false, tci = 0)) // pop
    apb.write(US_TREATMENT, treatment(3, remove = false, add = true, pcpCopy = false, sTag = true, tci = (6 << 13) | 500))
    for (k <- 1 to 3) apb.write(VLAN_MAP, (19 + k) | ((0x21F + k) << 12) | (1 << 24) | (k << 26))
    apb.write(DS_TREATMENT, treatment(1, remove = true, add = true, pcpCopy = true, sTag = false, tci = 20)) // translate back
    apb.write(DS_TREATMENT, treatment(2, remove = true, add = false, pcpCopy = false, sTag = false, tci = 0)) // pop
    apb.write(DS_VLAN, 30 | (1 << 12))
    apb.write(DS_VLAN, 40 | (2 << 12))
    mgmt.waitSampling(50)

    val ethOut = mutable.ArrayBuffer[Seq[Int]]()
    val ponOut = Seq.fill(2)(mutable.ArrayBuffer[(Int, Int, Seq[Int])]()) // per queue
    val cpuOut = mutable.ArrayBuffer[(Int, Int)]()
    var cur = mutable.ArrayBuffer[Int]()
    var ethGaps = 0
    ethTx.onSamplings {
      if (io.eth.tx.valid.toBoolean && io.eth.tx.ready.toBoolean) {
        cur += io.eth.tx.fragment.toInt
        if (io.eth.tx.last.toBoolean) { ethOut += cur.toSeq; cur = mutable.ArrayBuffer[Int]() }
      } else if (cur.nonEmpty) ethGaps += 1
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
    val ds30 = frame(100, Some((0x8100, (2 << 13) | 30)))
    val ds40 = frame(100, Some((0x8100, 40)))
    gem(0x100, Seq(withFcs(ds30)))
    gem(0x100, Seq(withFcs(ds40)))
    mgmt.waitSampling(3000)
    check(ethOut.take(2) == Seq(good1.dropRight(4), good2.dropRight(4)), s"PON -> Ethernet: ${ethOut.length} frames, fragments joined, FCS stripped")
    check(ethOut.drop(2) == Seq(rewritten(ds30, remove = true, Seq(0x81, 0x00, 2 << 5, 20)), rewritten(ds40, remove = true, Nil)),
      "PON -> Ethernet VLAN treatments by VID: 30 -> 20 PCP kept, 40 popped, others transparent")
    check(ethGaps == 0, "no gap inside a frame to the Ethernet MAC")
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
    val us20 = frame(100, Some((0x8100, (4 << 13) | 20)))
    val us21 = frame(100, Some((0x8100, 21)))
    val us22 = frame(100, Some((0x8100, 22)))
    send(us20); send(us21); send(us22)
    mgmt.waitSampling(3000)
    check(ponOut(0).drop(4).toSeq == Seq((0x220, 104, withFcs(rewritten(us20, remove = true, Seq(0x81, 0x00, 4 << 5, 30)))),
      (0x221, 100, withFcs(rewritten(us21, remove = true, Nil))), (0x222, 108, withFcs(rewritten(us22, remove = false, Seq(0x88, 0xA8, 0xC1, 0xF4))))),
      "Ethernet -> PON VLAN treatments: VID 20 -> 30 PCP kept, 21 popped, 22 S-tag 500 pushed, FCS of the rewritten frame")
    ponOut(0).remove(4, 3)
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

    // offloaded flows: LAN 192.168.1.10:5000 -> 8.8.8.8:443 as WAN 100.64.0.2:40000, PPPoE session 0x1234 in VLAN 35
    def flow(ponToEth: Boolean, k: FlowTableTest.Key, natSrc: Boolean, ip: Long, port: Int, egress: Int): Unit = {
      apb.write(FLOW_PORTS, k.dport | (k.sport.toLong << 16))
      apb.write(FLOW_DST, k.dst)
      apb.write(FLOW_SRC, k.src)
      apb.write(FLOW_L2, k.session | (k.vid << 16) | (k.udp.compare(false) << 28) | (k.pppoe.compare(false) << 29) |
        (k.tagged.compare(false) << 30))
      apb.write(FLOW_NAT_IP, ip)
      apb.write(FLOW_NAT, port | (egress << 16) | (natSrc.compare(false) << 24))
      apb.write(FLOW_WRITE, k.bucket(10) | (1 << 17) | (if (ponToEth) 1L << 31 else 0L))
    }
    def egress(ponToEth: Boolean, index: Int, da: Long, sa: Long, vlan: Int, portId: Int, queue: Int, session: Option[Int]): Unit = {
      apb.write(EGRESS_DA, da & 0xFFFFFFFFL)
      apb.write(EGRESS_MAC_HI, (da >> 32) | ((sa >> 32) << 16))
      apb.write(EGRESS_SA, sa & 0xFFFFFFFFL)
      apb.write(EGRESS_VLAN, vlan)
      apb.write(EGRESS_PORT, portId | (queue << 12) | (session.size << 15) | (session.getOrElse(0).toLong << 16))
      apb.write(EGRESS_WRITE, index | (if (ponToEth) 1L << 31 else 0L))
    }
    val (host, lan, gw, wan) = (0x00AABBCC0010L, 0x020000000001L, 0x00DDEEFF0001L, 0x020000000002L)
    val (hostIp, server, wanIp) = (0xC0A8010AL, 0x08080808L, 0x64400002L)
    val vlan35 = Seq(0x81, 0x00, 0x00, 35)
    egress(ponToEth = false, 3, gw, wan, treatment(0, remove = true, add = true, pcpCopy = false, sTag = false, tci = 35),
      0x230, 1, Some(0x1234))
    flow(ponToEth = false, FlowTableTest.Key(443, 5000, server, hostIp, false, 0, false, 0, false), natSrc = true, wanIp, 40000, 3)
    egress(ponToEth = true, 5, host, lan, treatment(0, remove = true, add = false, pcpCopy = false, sTag = false, tci = 0),
      0, 0, None)
    flow(ponToEth = true, FlowTableTest.Key(40000, 443, wanIp, server, false, 0x1234, true, 35, true), natSrc = false, hostIp, 5000, 5)
    mgmt.waitSampling(50)
    val payload = Seq.fill(30)(rnd.nextInt(256))
    val usIn = ipFrame((lan, host), Nil, None, hostIp, server, 5000, 443, 64, payload)
    val usOut = ipFrame((gw, wan), vlan35, Some(0x1234), wanIp, server, 40000, 443, 63, payload)
    val usMiss = ipFrame((lan, host), Nil, None, hostIp, server, 5001, 443, 64, payload)
    val dsIn = ipFrame((wan, gw), vlan35, Some(0x1234), server, wanIp, 443, 40000, 50, payload)
    val dsOut = ipFrame((host, lan), Nil, None, server, hostIp, 443, 5000, 49, payload)
    val dsTtl1 = ipFrame((wan, gw), vlan35, Some(0x1234), server, wanIp, 443, 40000, 1, payload)
    val (ponBefore, ethBefore) = (ponOut.map(_.length), ethOut.length)
    send(usIn); send(usMiss)
    gem(0x100, Seq(withFcs(dsIn))); gem(0x100, Seq(withFcs(dsTtl1)))
    mgmt.waitSampling(3000)
    check(ponOut(1).drop(ponBefore(1)).toSeq == Seq((0x230, usOut.length + 4, withFcs(usOut))),
      "Ethernet -> PON flow: NAPT source, TTL, PPPoE session and VLAN 35 added, its Port-ID in queue 1, checksums")
    check(ponOut(0).drop(ponBefore(0)).toSeq == Seq((0x201, usMiss.length + 4, withFcs(usMiss))),
      "Ethernet -> PON miss (other port): the L2 rules, unchanged")
    check(ethOut.drop(ethBefore).toSeq == Seq(dsOut, dsTtl1) && ethGaps == 0,
      "PON -> Ethernet flow: NAPT destination, VLAN and PPPoE stripped, no gap; TTL 1 missed: unchanged")

    // flows mode: misses to the CPU
    val lanCpu, wanCpu = mutable.ArrayBuffer[(Int, Seq[Int])]()
    def collect(cd: ClockDomain, f: Flow[Fragment[pon.gpon.UsGemByte]], to: mutable.ArrayBuffer[(Int, Seq[Int])]): Unit = {
      val cur = mutable.ArrayBuffer[Int]()
      cd.onSamplings {
        if (f.valid.toBoolean) {
          cur += f.gem.data.toInt
          if (f.last.toBoolean) { to += ((f.gem.portId.toInt, cur.toList)); cur.clear() }
        }
      }
    }
    collect(ponTx, io.lanToCpu, lanCpu)
    collect(ethTx, io.wanToCpu, wanCpu)
    apb.write(UNTAGGED, 0x201 | (1 << 12) | (1 << 21))
    apb.write(PORT_ACTION, 0x110 | (3 << 12))
    mgmt.waitSampling(50)
    val (ponBefore2, ethBefore2) = (ponOut.map(_.length), ethOut.length)
    send(usIn); send(usMiss)
    gem(0x110, Seq(withFcs(dsIn))); gem(0x110, Seq(withFcs(dsTtl1)))
    mgmt.waitSampling(3000)
    check(ponOut(1).drop(ponBefore2(1)).toSeq == Seq((0x230, usOut.length + 4, withFcs(usOut))) &&
      ponOut(0).length == ponBefore2(0) && lanCpu.toSeq == Seq((0, usMiss)),
      "flows mode, Ethernet -> PON: the hit to its egress, the miss to the CPU")
    check(ethOut.drop(ethBefore2).toSeq == Seq(dsOut) && wanCpu.toSeq == Seq((0x110, dsTtl1)),
      "flows mode, PON -> Ethernet (Port-ID action 3): the hit to Ethernet, the miss to the CPU with its Port-ID")

    // control frames: their own queues, rate-limited
    val lanControl, wanControl = mutable.ArrayBuffer[(Int, Seq[Int])]()
    collect(ponTx, io.lanControlToCpu, lanControl)
    collect(ethTx, io.wanControlToCpu, wanControl)
    apb.write(CONTROL_RATE, 0 | (2 << 8) | (1 << 16) | (32 << 24)) // LAN: no refill, burst 2
    ponTx.waitSampling(70000) // a refill tick: the LAN bucket clamped to 2
    val arps = Seq.fill(3)(be(0xFFFFFFFFFFFFL, 6) ++ be(host, 6) ++ Seq(0x08, 0x06) ++ Seq.fill(46)(rnd.nextInt(256)))
    val lcp = be(wan, 6) ++ be(gw, 6) ++ vlan35 ++ Seq(0x88, 0x64, 0x11, 0x00, 0x12, 0x34, 0x00, 0x0A, 0xC0, 0x21) ++
      Seq(0x09, 0x01, 0x00, 0x08) ++ Seq.fill(40)(rnd.nextInt(256)) // LCP echo request
    arps.foreach(send(_))
    gem(0x110, Seq(withFcs(lcp)))
    mgmt.waitSampling(3000)
    val controlDrops = apb.read(CONTROL_DROPS).toLong
    check(lanControl.toSeq == arps.take(2).map((0, _)) && wanControl.toSeq == Seq((0x110, lcp)) &&
      lanCpu.length == 1 && wanCpu.length == 1 && controlDrops == 1,
      s"control frames (ARP, LCP) to their own queues, the LAN bucket of 2 dropping the third ARP (drops $controlDrops)")

    // the CPU's own frames
    def inject(cd: ClockDomain, port: Stream[Fragment[pon.gpon.GemTxByte]], portId: Int, bytes: Seq[Int]): Unit = {
      for ((b, i) <- bytes.zipWithIndex) {
        port.valid #= true
        port.fragment.portId #= portId
        port.fragment.length #= bytes.length
        port.fragment.data #= b
        port.last #= i == bytes.length - 1
        cd.waitSamplingWhere(port.ready.toBoolean)
      }
      port.valid #= false
    }
    val lanFrame = frame(80)
    val wanPacket = withFcs(frame(90))
    inject(ethTx, io.cpuToLan, 0, lanFrame)
    inject(ponTx, io.cpuToWan, 0x205, wanPacket)
    mgmt.waitSampling(500)
    check(ethOut.last == lanFrame && ponOut(1).last == ((0x205, wanPacket.length, wanPacket)),
      "CPU frames: to Ethernet, and to the PON in the queue of their Port-ID (0x205: queue 1)")

    def hitOf(ponToEth: Boolean, k: FlowTableTest.Key, way: Int): Boolean = {
      apb.write(FLOW_HIT_QUERY, k.bucket(10) | (way << 16) | (if (ponToEth) 1L << 31 else 0L))
      mgmt.waitSampling(30)
      val r = apb.read(FLOW_HIT).toLong
      assert((r >> 31) == 1, "FLOW_HIT not done")
      (r & 1) == 1
    }
    val usKey = FlowTableTest.Key(443, 5000, server, hostIp, false, 0, false, 0, false)
    val dsKey = FlowTableTest.Key(40000, 443, wanIp, server, false, 0x1234, true, 35, true)
    val answers = Seq(hitOf(false, usKey, 0), hitOf(false, usKey, 0), hitOf(true, dsKey, 0), hitOf(true, dsKey, 0),
      hitOf(true, dsKey, 1))
    check(answers == Seq(true, false, true, false, false), s"flow hit bits: set by the traffic, cleared by a query $answers")

    // IPv6, routed: LAN 2001:db8:1::10 port 5000 -> 2001:4860::8888 port 443 into PPPoE session 0x1234 in VLAN 35
    val tcp = be(5000, 2) ++ be(443, 2) ++ Seq.fill(9)(rnd.nextInt(256)) ++ Seq(0x10) ++ Seq.fill(6)(rnd.nextInt(256)) ++
      payload // ACK; unchanged: no NAT
    def ip6Frame(macs: (Long, Long), tags: Seq[Int], session: Option[Int], src: BigInt, dst: BigInt, hop: Int): Seq[Int] = {
      val ip = Seq(0x60, 0, 0, 0) ++ be(tcp.length, 2) ++ Seq(6, hop) ++ (0 until 32).map(k =>
        ((if (k < 16) src else dst) >> (8 * (15 - k % 16)) & 0xFF).toInt)
      be(macs._1, 6) ++ be(macs._2, 6) ++ tags ++ (session match {
        case Some(id) => Seq(0x88, 0x64, 0x11, 0x00) ++ be(id, 2) ++ be(ip.length + tcp.length + 2, 2) ++ Seq(0x00, 0x57)
        case None => Seq(0x86, 0xDD)
      }) ++ ip ++ tcp
    }
    val (host6, server6) = (BigInt("20010db8000100000000000000000010", 16), BigInt("20014860000000000000000000008888", 16))
    val key6 = FlowTableTest.Key(443, 5000, server6, host6, false, 0, false, 0, false, addressBits = 128)
    for ((reg, v) <- Seq(FLOW_PORTS -> (443 | (5000L << 16)), FLOW_DST -> (server6 & 0xFFFFFFFFL).toLong,
           FLOW_SRC -> (host6 & 0xFFFFFFFFL).toLong, FLOW_L2 -> 0L, FLOW_NAT_IP -> 0L, FLOW_NAT -> (3L << 16)) ++
         (1 to 3).map(k => (FLOW_DST_1 + 4 * (k - 1)) -> (server6 >> (32 * k) & 0xFFFFFFFFL).toLong) ++
         (1 to 3).map(k => (FLOW_DST_1 + 12 + 4 * (k - 1)) -> (host6 >> (32 * k) & 0xFFFFFFFFL).toLong))
      apb.write(reg, v)
    apb.write(FLOW_WRITE, key6.bucket(8) | (1 << 17) | (1L << 30))
    mgmt.waitSampling(50)
    val us6 = ip6Frame((lan, host), Nil, None, host6, server6, 64)
    val us6Out = ip6Frame((gw, wan), vlan35, Some(0x1234), host6, server6, 63)
    val ponBefore3 = ponOut(1).length
    send(us6)
    mgmt.waitSampling(2000)
    check(ponOut(1).drop(ponBefore3).toSeq == Seq((0x230, us6Out.length + 4, withFcs(us6Out))),
      "IPv6 flow, Ethernet -> PON: routed (hop limit), PPPoE (0x0057) and VLAN 35 added, its Port-ID")
    def hit6(): Long = { apb.write(FLOW_HIT_QUERY, key6.bucket(8) | (1L << 30)); mgmt.waitSampling(30); apb.read(FLOW_HIT).toLong }
    check(Seq(hit6(), hit6()) == Seq(0x80000001L, 0x80000000L), "IPv6 flow hit bit: set, then cleared")

    // snapshot of the next Ethernet -> PON frame: the IPv4 flow found in its bucket
    def snapshot(ponToEth: Boolean)(send: => Unit): Seq[Long] = {
      apb.write(SNAP_ARM, if (ponToEth) 1L << 31 else 0L)
      mgmt.waitSampling(30)
      send
      mgmt.waitSampling(2000)
      (0 until 6).map(k => apb.read(SNAP_PORTS + 4 * k).toLong)
    }
    val snap = snapshot(ponToEth = false)(send(usIn))
    val result = snap(4)
    check(snap.take(3) == Seq(443 | (5000L << 16), hostIp, server) && (result & 0xFFFF) == usKey.bucket(10) &&
      (result >> 16 & 0xFF) == 0xCE && (result >> 24 & 0xF) == 0 && (result >> 31) == 1 &&
      snap(5) == (40000 | (3L << 16) | (64L << 24)), s"snapshot: ports, addresses, bucket, way 0 found and taken, " +
      s"egress 3, port, TTL ${snap.map(_.toHexString)}")
    ponOut(1).clear()

    // bypassed layers: to the CPU instead (the untagged entry in flows mode, VLAN 100 not)
    val (lanCpuBefore, wanCpuBefore) = (lanCpu.length, wanCpu.length)
    apb.write(BYPASS, 1 << 2) // NAPT
    val natSnap = snapshot(ponToEth = false)(send(usIn))
    apb.write(BYPASS, 1 << 4) // VLAN rewrites
    send(us20); send(q)
    apb.write(BYPASS, 1 << 5) // L2
    send(q); gem(0x100, Seq(withFcs(ds40)))
    apb.write(BYPASS, 0)
    mgmt.waitSampling(3000)
    check((natSnap(4) >> 16 & 0x1F) == 0x06 && lanCpu.drop(lanCpuBefore).toSeq == Seq((0, usIn), (0, us20), (0, q)) &&
      ponOut(1).isEmpty && ponOut(0).last == ((0x200, 204, withFcs(q))) && wanCpu.drop(wanCpuBefore).toSeq == Seq((0x100, ds40)),
      "bypass: the NAPT flow found but missed, the VLAN translation, the L2-bridged frames each way to the CPU")

    // traps: an ARP bridged from the PON to the CPU's control queue; from Ethernet to its data queue, then dropped
    val arp = be(0xFFFFFFFFFFFFL, 6) ++ be(gw, 6) ++ Seq(0x08, 0x06) ++ Seq.fill(46)(rnd.nextInt(256))
    val arpCode = TrapReason("arp")
    val (wanControlBefore, lanCpuBefore2, ethBefore4) = (wanControl.length, lanCpu.length, ethOut.length)
    apb.write(TRAP_WAN, 1L << (2 * arpCode - 2))
    apb.write(TRAP_LAN, 2L << (2 * arpCode - 2))
    gem(0x100, Seq(withFcs(arp)))
    send(arp)
    mgmt.waitSampling(2000)
    val dropsBefore = apb.read(DROPS).toLong >> 16
    apb.write(TRAP_LAN, 3L << (2 * arpCode - 2))
    send(arp)
    mgmt.waitSampling(2000)
    val dropsAfter = apb.read(DROPS).toLong >> 16
    check(wanControl.drop(wanControlBefore).toSeq == Seq((0x100, arp)) && ethOut.length == ethBefore4 &&
      lanCpu.drop(lanCpuBefore2).toSeq == Seq((0, arp)) && dropsAfter == dropsBefore + 1,
      s"traps: ARP from the PON to the control queue, from Ethernet to the data queue, then dropped and counted")
  }
  println("PASS GemEthBridgeTest")
}
