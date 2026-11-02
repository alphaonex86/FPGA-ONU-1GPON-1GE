// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.core.sim._
import scala.collection.mutable
import pon.gpon.GtcDsTest.check

/** FrameParser against frames built here: 0-2 VLAN tags, PPPoE session or not, IPv4 TCP / UDP / ICMP (options,
  * fragments, TCP SYN/FIN/RST), IPv6, ARP; every field and the offload verdict as expected. */
object FrameParserTest extends App {
  case class Expect(vlans: Seq[Int], session: Option[Int], ipv4: Boolean, ipv6: Boolean, l3Offset: Int, ttl: Int,
                    proto: Int, src: Long, dst: Long, sport: Int, dport: Int, offloadable: Boolean, da: Long)
  val rnd = new scala.util.Random(4)
  def be(v: Long, n: Int): Seq[Int] = (n - 1 to 0 by -1).map(k => ((v >> (8 * k)) & 0xFF).toInt)
  def frame(): (Seq[Int], Expect) = {
    val da = rnd.nextLong() & 0xFFFFFFFFFFFFL
    val vlans = Seq.fill(rnd.nextInt(3))(rnd.nextInt(4096))
    val session = if (rnd.nextBoolean()) Some(rnd.nextInt(65536)) else None
    val kind = rnd.nextInt(8) // 0-3 IPv4 TCP/UDP, 4 ICMP, 5 options, 6 IPv6, 7 ARP
    val proto = Seq(6, 17, 6, 17, 1, 6, 0, 0)(kind)
    val fragment = kind < 4 && rnd.nextInt(5) == 0
    val flags = if (proto == 6 && rnd.nextInt(3) == 0) Seq(0x02, 0x01, 0x04)(rnd.nextInt(3)) else 0x10
    val (ttl, src, dst, sport, dport) = (rnd.nextInt(256), rnd.nextLong() & 0xFFFFFFFFL, rnd.nextLong() & 0xFFFFFFFFL,
      rnd.nextInt(65536), rnd.nextInt(65536))
    val ipv4 = kind <= 5
    val ether = if (kind == 6) 0x86DD else if (kind == 7) 0x0806 else 0x0800
    val l2 = be(da, 6) ++ Seq.fill(6)(rnd.nextInt(256)) ++
      vlans.flatMap(v => Seq(0x81, 0x00) ++ be((rnd.nextInt(8) << 13) | v, 2)) ++
      (session match {
        case Some(s) => Seq(0x88, 0x64, 0x11, 0x00) ++ be(s, 2) ++ be(100, 2) ++ be(if (ether == 0x86DD) 0x0057 else if (ipv4) 0x0021 else 0xC021, 2)
        case None => be(ether, 2)
      })
    val l3 = if (ipv4) {
      val ihl = if (kind == 5) 6 else 5
      Seq(0x40 | ihl, 0) ++ be(100, 2) ++ be(rnd.nextInt(65536), 2) ++ be(if (fragment) 0x2000 else 0x4000, 2) ++
        Seq(ttl, proto) ++ be(0, 2) ++ be(src, 4) ++ be(dst, 4) ++ Seq.fill(4 * (ihl - 5))(0)
    } else if (kind == 6) Seq(0x60) ++ Seq.fill(39)(rnd.nextInt(256)) else Seq.fill(28)(rnd.nextInt(256))
    val l4 = if (proto == 6) be(sport, 2) ++ be(dport, 2) ++ Seq.fill(9)(0) ++ Seq(flags) ++ Seq.fill(6)(0)
             else if (proto == 17) be(sport, 2) ++ be(dport, 2) ++ Seq.fill(4)(0) else Seq.fill(8)(0)
    val bytes = l2 ++ l3 ++ l4 ++ Seq.fill(10 + rnd.nextInt(30))(rnd.nextInt(256))
    val v4 = ipv4 && kind != 5 && !fragment
    (bytes, Expect(vlans, session, v4, kind == 6, l2.length, ttl, proto, src, dst, sport, dport,
      v4 && proto != 1 && (proto == 17 || (flags & 0x07) == 0), da))
  }

  SimConfig.compile(FrameParser()).doSim { dut =>
    val io = dut.io
    val cd = dut.clockDomain
    cd.forkStimulus(10)
    io.input.valid #= false
    cd.waitSampling(3)
    var wrong = mutable.ArrayBuffer[String]()
    val n = 400
    for (k <- 0 until n) {
      val (bytes, e) = frame()
      for ((b, i) <- bytes.zipWithIndex) {
        io.input.valid #= true
        io.input.fragment #= b
        io.input.last #= i == bytes.length - 1
        cd.waitSampling()
      }
      io.input.valid #= false
      cd.waitSampling()
      val h = io.header
      val got = Seq(h.vlans.toInt == e.vlans.length, e.vlans.headOption.forall(_ == h.outerVid.toInt),
        e.vlans.drop(1).headOption.forall(_ == h.innerVid.toInt), h.pppoe.toBoolean == e.session.isDefined,
        e.session.forall(_ == h.session.toInt), h.ipv4.toBoolean == e.ipv4, h.ipv6.toBoolean == e.ipv6,
        h.da.toLong == e.da, h.offloadable.toBoolean == e.offloadable) ++
        (if (e.ipv4) Seq(h.l3Offset.toInt == e.l3Offset, h.ttl.toInt == e.ttl, h.proto.toInt == e.proto,
          h.src.toLong == e.src, h.dst.toLong == e.dst) else Nil) ++
        (if (e.offloadable) Seq(h.sport.toInt == e.sport, h.dport.toInt == e.dport) else Nil)
      if (got.contains(false)) wrong += s"frame $k: $e checks $got"
      cd.waitSampling(rnd.nextInt(3))
    }
    check(wrong.isEmpty, s"$n frames: VLANs, PPPoE session, IPv4 fields, ports, offload verdict ${wrong.take(2)}")
  }
  println("PASS FrameParserTest")
}
