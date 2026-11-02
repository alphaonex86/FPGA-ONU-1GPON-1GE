// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.collection.mutable
import pon.gpon.GtcDsTest.check

/** FlowRewrite then VlanRewrite(reserve = 12), as at the bridge's output. */
case class FlowRewriteChain() extends Component {
  val io = new Bundle {
    val input = slave Stream (Fragment(Bits(8 bits)))
    val job = in(RewriteJob())
    val output = master Stream (Fragment(Bits(8 bits)))
  }
  val flow = FlowRewrite()
  val vlan = VlanRewrite(appendFcs = false, reserve = 12)
  flow.io.input << io.input
  flow.io.job := io.job
  vlan.io.input << flow.io.output
  vlan.io.treatment.assignFromBits(B(0, widthOf(VlanTreatment()) bits))
  io.output << vlan.io.output
}

/** FlowRewrite against a model: IPv4 TCP/UDP frames with valid checksums, 0-2 VLAN tags, PPPoE or not, Ethernet
  * padding, IPv6 (hop limit only); jobs with PPPoE kept, added or stripped, source or destination NAPT, UDP without checksum, disabled jobs;
  * the IP and L4 checksums of every output frame verified from scratch; no gap in an output frame. */
object FlowRewriteTest extends App {
  val rnd = new scala.util.Random(6)
  def be(v: Long, n: Int): Seq[Int] = (n - 1 to 0 by -1).map(k => ((v >> (8 * k)) & 0xFF).toInt)
  def words(b: Seq[Int]): Seq[Int] = b.padTo(b.length + b.length % 2, 0).grouped(2).map(p => p(0) << 8 | p(1)).toSeq
  def sum(ws: Seq[Int]): Int = ws.foldLeft(0)(Checksum.addModel)
  def l4Sum(ip: Seq[Int], l4: Seq[Int]): Int = sum(words(ip.slice(12, 20)) ++ Seq(ip(9), l4.length) ++ words(l4))
  case class Job(enable: Boolean, da: Long, sa: Long, l2End: Int, strip: Boolean, add: Boolean, session: Int,
                 pppLength: Int, l3Offset: Int, natSrc: Boolean, ip: Long, port: Int, udp: Boolean, ipChecksum: Int,
                 l4Checksum: Int, ipv6: Boolean)

  /** A frame, its job and the expected output. */
  def make(): (Seq[Int], Job, Seq[Int]) = {
    val udp = rnd.nextBoolean()
    val (src, dst) = (rnd.nextLong() & 0xFFFFFFFFL, rnd.nextLong() & 0xFFFFFFFFL)
    val (sport, dport) = (rnd.nextInt(65536), rnd.nextInt(65536))
    val payload = Seq.fill(rnd.nextInt(60))(rnd.nextInt(256))
    val l4raw = be(sport, 2) ++ be(dport, 2) ++ (if (udp) be(8 + payload.length, 2) ++ be(0, 2)
      else Seq.fill(9)(rnd.nextInt(256)) ++ Seq(0x10) ++ be(rnd.nextInt(65536), 2) ++ be(0, 2) ++ be(0, 2)) ++ payload
    val ipraw = Seq(0x45, 0) ++ be(20 + l4raw.length, 2) ++ be(rnd.nextInt(65536), 2) ++ be(0x4000, 2) ++
      Seq(2 + rnd.nextInt(254), if (udp) 17 else 6) ++ be(0, 2) ++ be(src, 4) ++ be(dst, 4)
    val ip = ipraw.patch(10, be(~sum(words(ipraw)) & 0xFFFF, 2), 2)
    val noUdpChecksum = udp && rnd.nextInt(4) == 0
    val c4 = if (noUdpChecksum) 0 else { val c = ~l4Sum(ip, l4raw) & 0xFFFF; if (udp && c == 0) 0xFFFF else c }
    val l4 = l4raw.patch(if (udp) 6 else 16, be(c4, 2), 2)
    val v6 = rnd.nextInt(3) == 0 // routed: only the hop limit changes
    val ip6 = Seq(0x60, 0, 0, 0) ++ be(l4.length, 2) ++ Seq(if (udp) 17 else 6, 2 + rnd.nextInt(254)) ++
      Seq.fill(32)(rnd.nextInt(256))
    val ipIn = if (v6) ip6 else ip
    val etherType = if (v6) Seq(0x86, 0xDD) else Seq(0x08, 0x00)
    val vlans = Seq.fill(rnd.nextInt(3))(be(0x81000000L | rnd.nextInt(65536), 4)).flatten
    val l2End = 12 + vlans.length
    val pppoeIn = rnd.nextBoolean()
    val session = rnd.nextInt(65536)
    def pppoe(s: Int) = Seq(0x88, 0x64, 0x11, 0x00) ++ be(s, 2) ++ be(ipIn.length + l4.length + 2, 2) ++
      Seq(0x00, if (v6) 0x57 else 0x21)
    val l2 = Seq.fill(12)(rnd.nextInt(256)) ++ vlans
    val frame = l2 ++ (if (pppoeIn) pppoe(rnd.nextInt(65536)) else etherType) ++ ipIn ++ l4 ++
      Seq.fill(rnd.nextInt(4))(rnd.nextInt(256)) // Ethernet padding
    val pppoeOut = rnd.nextInt(3) match { case 0 => pppoeIn; case 1 => true; case _ => false }
    val natSrc = rnd.nextBoolean()
    val (newIp, newPort) = (rnd.nextLong() & 0xFFFFFFFFL, rnd.nextInt(65536))
    val (oldIp, oldPort) = if (natSrc) (src, sport) else (dst, dport)
    val ttlProto = ip(8) << 8 | ip(9)
    val ipC = Checksum.updateModel(ip(10) << 8 | ip(11), Seq(ttlProto, (oldIp >> 16).toInt, (oldIp & 0xFFFF).toInt),
      Seq(ttlProto - 0x100, (newIp >> 16).toInt, (newIp & 0xFFFF).toInt))
    val l4C = if (noUdpChecksum) 0 else {
      val c = Checksum.updateModel(c4, Seq((oldIp >> 16).toInt, (oldIp & 0xFFFF).toInt, oldPort),
        Seq((newIp >> 16).toInt, (newIp & 0xFFFF).toInt, newPort))
      if (udp && c == 0) 0xFFFF else c
    }
    val (da, sa) = (rnd.nextLong() & 0xFFFFFFFFFFFFL, rnd.nextLong() & 0xFFFFFFFFFFFFL)
    val enable = rnd.nextInt(6) != 0
    val job = Job(enable, da, sa, l2End, pppoeIn && !pppoeOut, !pppoeIn && pppoeOut, session,
      ipIn.length + l4.length + 2, l2End + (if (pppoeIn) 10 else 2), natSrc, newIp, newPort, udp, ipC, l4C, v6)
    val ipOut = if (v6) ip6.patch(7, Seq(ip6(7) - 1), 1)
                else ip.patch(8, Seq(ip(8) - 1), 1).patch(10, be(ipC, 2), 2).patch(if (natSrc) 12 else 16, be(newIp, 4), 4)
    val l4Out = if (v6) l4 else l4.patch(if (natSrc) 0 else 2, be(newPort, 2), 2).patch(if (udp) 6 else 16, be(l4C, 2), 2)
    val expected = if (!enable) frame else be(da, 6) ++ be(sa, 6) ++ vlans ++
      (if (pppoeOut) pppoe(if (pppoeIn && !job.strip && !job.add) frame.slice(l2End + 4, l2End + 6).reduce(_ << 8 | _) else session)
       else etherType) ++ ipOut ++ l4Out ++ frame.drop(job.l3Offset + ipIn.length + l4.length)
    if (enable && !v6) { // the incremental checksums, verified from scratch
      assert(sum(words(ipOut)) == 0xFFFF, "IP checksum")
      assert((noUdpChecksum && l4C == 0) || l4Sum(ipOut, l4Out) == 0xFFFF, "L4 checksum")
    }
    (frame, job, expected)
  }

  for (stress <- Seq(true, false)) SimConfig.compile(FlowRewriteChain()).doSim { dut =>
    val io = dut.io
    val cd = dut.clockDomain
    cd.forkStimulus(10)
    io.input.valid #= false
    io.output.ready #= false
    cd.waitSampling(3)
    val jobs = mutable.Queue[(Seq[Int], Job, Seq[Int])]()
    val received = mutable.ArrayBuffer[Seq[Int]]()
    val current = mutable.ArrayBuffer[Int]()
    var gaps = 0
    cd.onSamplings {
      if (io.output.valid.toBoolean && io.output.ready.toBoolean) {
        current += io.output.fragment.toInt
        if (io.output.last.toBoolean) { received += current.toList; current.clear() }
      } else if (current.nonEmpty) gaps += 1
      io.output.ready #= !stress || rnd.nextInt(4) != 0
    }
    val all = Seq.fill(150)(make())
    for ((frame, j, _) <- all) {
      val p = io.job
      p.enable #= j.enable; p.da #= j.da; p.sa #= j.sa; p.l2End #= j.l2End; p.strip #= j.strip; p.add #= j.add
      p.session #= j.session; p.pppLength #= j.pppLength; p.l3Offset #= j.l3Offset; p.natSrc #= j.natSrc
      p.ip #= j.ip; p.port #= j.port; p.udp #= j.udp; p.ipChecksum #= j.ipChecksum; p.l4Checksum #= j.l4Checksum
      p.ipv6 #= j.ipv6
      for ((b, i) <- frame.zipWithIndex) {
        io.input.valid #= true
        io.input.fragment #= b
        io.input.last #= i == frame.length - 1
        cd.waitSamplingWhere(io.input.ready.toBoolean)
        if (stress && rnd.nextInt(5) == 0) { io.input.valid #= false; cd.waitSampling(rnd.nextInt(3)) }
      }
      io.input.valid #= false
      cd.waitSampling(if (stress) rnd.nextInt(30) else 1) // the next job set once this frame's job was read
    }
    cd.waitSampling(200)
    val wrong = all.map(_._3).zip(received).zipWithIndex.collect { case ((e, g), k) if e != g => s"frame $k: expected $e got $g" }
    check(received.length == all.length && wrong.isEmpty && (stress || gaps == 0),
      s"${if (stress) "stress" else "continuous"}: ${received.length} frames rewritten as the model, checksums valid, $gaps gaps ${wrong.take(1)}")
  }
  println("PASS FlowRewriteTest")
}
