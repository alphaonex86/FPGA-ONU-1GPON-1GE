// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.epon

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.collection.mutable
import pon.gpon.GtcDsTest.check

/** OLT MAC -> fibre (dsDelay clocks) -> ONU MAC + OnuMpcp -> laser (dark: zeros) -> fibre (usDelay) -> OLT MAC. */
case class EponOnuLoop(dsDelay: Int, usDelay: Int) extends Component {
  val io = new Bundle {
    val dsFrames = slave Stream (Fragment(EponTxByte()))
    val usFrames = master Flow (Fragment(EponRxByte()))
    val usGood = out Bool ()
    val config = in(OnuMpcpConfig())
    val data = slave Stream (Fragment(EponUsByte()))
    val report = in UInt (16 bits)
    val burst, registered, drift, late, timedOut = out Bool ()
    val llid = out UInt (15 bits)
    val time = out UInt (32 bits)
  }
  val oltTx = EponMacTx()
  oltTx.io.input << io.dsFrames
  val onuRx = EponMacRx()
  onuRx.io.raw := Delay(oltTx.io.code, dsDelay)
  val mpcp = OnuMpcp()
  mpcp.io.config := io.config
  mpcp.io.rx << onuRx.io.frame
  mpcp.io.rxGood := onuRx.io.good
  mpcp.io.data << io.data
  mpcp.io.report := io.report
  val onuTx = EponMacTx()
  onuTx.io.input << mpcp.io.tx
  val light = RegNext(mpcp.io.burst) init (False) // with the encoder's registered code
  val oltRx = EponMacRx()
  oltRx.io.raw := Delay(light ? onuTx.io.code | B(0, 10 bits), usDelay)
  io.usFrames << oltRx.io.frame
  io.usGood := oltRx.io.good
  io.burst := mpcp.io.burst
  io.registered := mpcp.io.registered
  io.drift := mpcp.io.drift
  io.late := mpcp.io.late
  io.timedOut := mpcp.io.timedOut
  io.llid := mpcp.io.llid
  io.time := mpcp.io.time
}

/** OnuMpcp against an OLT model over EPON MACs and a fibre: discovery (REGISTER_REQ inside the window, at a random
  * offset), REGISTER, REGISTER_ACK in the first grant, REPORT then the data frames that fit, bursts from the grant
  * start to its end less laserOff in the ONU's time, a constant round trip, a late grant dropped, a timestamp drift,
  * deregistration by the OLT, by the MPCP timeout and by the ONU (REGISTER_REQ deregister). */
object OnuMpcpTest extends App {
  val rnd = new scala.util.Random(9)
  val mac = BigInt("001122334455", 16)
  val oltMac = BigInt("0A0B0C0D0E0F", 16)
  val (laserOn, laserOff) = (16, 16)
  SimConfig.compile(EponOnuLoop(dsDelay = 37, usDelay = 41)).doSim { dut =>
    val io = dut.io
    val cd = dut.clockDomain
    cd.forkStimulus(8)
    io.dsFrames.valid #= false
    io.data.valid #= false
    io.report #= 0x1234
    val c = io.config
    c.enable #= true; c.mac #= mac; c.laserOn #= laserOn; c.laserOff #= laserOff; c.alwaysReport #= true; c.timeout #= 0
    var cycle = 0L
    def oltTime: Long = cycle / 2
    case class Us(at: Long, bytes: Seq[Int], llid: Int, good: Boolean) {
      def opcode = bytes(15)
      def field(k: Int, n: Int = 1): Long = (0 until n).foldLeft(0L)((v, i) => v << 8 | bytes(20 + k + i))
      def stamp: Long = (16 until 20).foldLeft(0L)((v, i) => v << 8 | bytes(i))
    }
    val us = mutable.ArrayBuffer[Us]()
    val current = mutable.ArrayBuffer[Int]()
    var firstAt = 0L
    val bursts = mutable.ArrayBuffer[(Long, Long)]() // ONU time of laser on, off
    var burstOn = 0L
    var drifts, lates = 0
    cd.onSamplings {
      cycle += 1
      if (io.usFrames.valid.toBoolean) {
        if (current.isEmpty) firstAt = cycle
        current += io.usFrames.fragment.data.toInt
        if (io.usFrames.last.toBoolean) {
          us += Us(firstAt, current.toList, io.usFrames.fragment.llid.toInt, io.usGood.toBoolean)
          current.clear()
        }
      }
      val burst = io.burst.toBoolean
      if (burst && burstOn < 0) burstOn = io.time.toLong
      if (!burst && burstOn >= 0) { if (cycle > 2) bursts += ((burstOn, io.time.toLong)); burstOn = -1 }
      if (io.drift.toBoolean) drifts += 1
      if (io.late.toBoolean) lates += 1
    }
    burstOn = -1
    cd.waitSampling(100)

    def be(v: Long, n: Int): Seq[Int] = (n - 1 to 0 by -1).map(k => ((v >> (8 * k)) & 0xFF).toInt)
    var stampSkew = 0L
    /** An MPCPDU from the OLT, its timestamp the OLT time of its first byte. */
    def mpcp(da: BigInt, mode: Boolean, llid: Int, opcode: Int, fields: Seq[Int]): Unit = {
      io.dsFrames.fragment.mode #= mode
      io.dsFrames.fragment.llid #= llid
      io.dsFrames.fragment.fcs #= true
      var stamp = 0L
      for (i <- 0 until 60) {
        val b = i match {
          case _ if i < 6 => ((da >> (8 * (5 - i))) & 0xFF).toInt
          case _ if i < 12 => ((oltMac >> (8 * (11 - i))) & 0xFF).toInt
          case 12 => 0x88; case 13 => 0x08; case 14 => 0; case 15 => opcode
          case _ if i < 20 => be(stamp + stampSkew, 4)(i - 16)
          case _ => fields.lift(i - 20).getOrElse(0)
        }
        io.dsFrames.valid #= true
        io.dsFrames.fragment.data #= b
        io.dsFrames.last #= i == 59
        cd.waitSamplingWhere(io.dsFrames.ready.toBoolean)
        if (i == 0) stamp = oltTime
      }
      io.dsFrames.valid #= false
    }
    def gate(llid: Int, grants: Seq[(Long, Int)], discovery: Boolean = false, force: Int = 0, sync: Int = 0): Unit =
      mpcp(Mpcp.multicast, discovery, llid, Mpcp.gate, Seq(force << 4 | (if (discovery) 8 else 0) | grants.length) ++
        grants.flatMap(g => be(g._1, 4) ++ be(g._2, 2)) ++ (if (discovery) be(sync, 2) else Nil))
    def waitUs(tq: Long): Seq[Us] = { val u = us.length; cd.waitSampling(2 * tq.toInt); us.drop(u).toSeq }
    def grantAt(delay: Long) = io.time.toLong + delay
    def discover(): Seq[Us] = { gate(Epon.broadcastLlid, Seq((grantAt(2000), 1000)), discovery = true, sync = 32); waitUs(3200) }
    def register(llid: Int): Seq[Us] = {
      mpcp(mac, true, Epon.broadcastLlid, Mpcp.register, be(llid, 2) ++ Seq(Mpcp.regAck) ++ be(16, 2) ++ Seq(8))
      gate(llid, Seq((grantAt(1500), 300)))
      waitUs(1800)
    }

    // discovery
    val discoveryStart = grantAt(2000)
    gate(Epon.broadcastLlid, Seq((discoveryStart, 1000)), discovery = true, sync = 32)
    var got = waitUs(3200)
    val req = got.headOption
    check(got.length == 1 && req.get.good && req.get.llid == Epon.broadcastLlid && req.get.opcode == Mpcp.registerReq &&
      req.get.field(0) == Mpcp.regRegister && req.get.field(1) == 8 &&
      req.get.bytes.slice(6, 12) == be(mac.toLong, 6), "discovery GATE: one REGISTER_REQ (register, 8 pending grants)")
    val rtt = req.get.at / 2 - req.get.stamp
    val (on, off) = bursts.last
    val reqBurst = laserOn + 32 + Mpcp.cost(Mpcp.pduBytes)
    check(bursts.length == 1 && on >= discoveryStart && on + reqBurst + laserOff <= discoveryStart + 1000 && off - on == reqBurst,
      s"its burst inside the discovery window (offset ${on - discoveryStart} TQ), laser on ${off - on} TQ; round trip $rtt TQ")

    // registration
    mpcp(mac, true, Epon.broadcastLlid, Mpcp.register, be(0x123, 2) ++ Seq(Mpcp.regAck) ++ be(16, 2) ++ Seq(8))
    cd.waitSampling(100) // the fibre and the MAC
    check(!io.registered.toBoolean && io.llid.toInt == 0x123, "REGISTER ack: LLID 0x123, waiting for a grant")
    var start = grantAt(1500)
    gate(0x123, Seq((start, 300)))
    got = waitUs(1800)
    check(got.map(_.opcode) == Seq(Mpcp.registerAck, Mpcp.report) && got.forall(u => u.good && u.llid == 0x123) &&
      got(0).field(0) == 1 && got(0).field(1, 2) == 0x123 && got(0).field(3, 2) == 16,
      "first grant: REGISTER_ACK (echoed LLID and sync time), then a REPORT")
    check(io.registered.toBoolean && got(1).field(0) == 1 && got(1).field(1) == 1 && got(1).field(2, 2) == 0x1234,
      "registered; REPORT: one queue set, queue 0 = 0x1234 TQ")
    check(bursts.last == ((start, start + 300 - laserOff)), s"burst from the grant start to its end less laserOff ${bursts.last}")
    check(got.forall(u => (u.at / 2 - u.stamp - rtt).abs <= 1), "round trip constant (+-1 TQ)")

    // data frames: those that fit, in order
    val frames = Seq.fill(6)(EponFrames.withFcs(Seq.fill(100 + rnd.nextInt(100))(rnd.nextInt(256))))
    fork {
      for (f <- frames) {
        for ((b, i) <- f.zipWithIndex) {
          io.data.valid #= true
          io.data.fragment.data #= b
          io.data.fragment.length #= f.length
          io.data.last #= i == f.length - 1
          cd.waitSamplingWhere(io.data.ready.toBoolean)
        }
        io.data.valid #= false
      }
    }
    start = grantAt(1500)
    gate(0x123, Seq((start, 300), (start + 400, 600)), force = 1)
    got = waitUs(2500)
    val datas = got.filter(_.bytes.length > 64)
    check(got.count(_.opcode == Mpcp.report) == 2 && got.forall(_.good) && datas.length >= 3 &&
      datas.map(_.bytes) == frames.take(datas.length), s"two grants: a REPORT each, then ${datas.length} data frames in order")
    check(bursts.takeRight(2) == Seq((start, start + 300 - laserOff), (start + 400, start + 1000 - laserOff)), "two bursts")

    // a late grant, a drift
    gate(0x123, Seq((grantAt(-10), 100)))
    cd.waitSampling(300)
    check(lates == 1, "a grant already started: dropped (late)")
    stampSkew = 20
    gate(0x123, Seq((grantAt(1500), 200)))
    cd.waitSampling(3400)
    check(drifts == 1 && io.registered.toBoolean, "a timestamp 20 TQ off: drift, still registered")
    stampSkew = 0

    // the OLT deregisters, the ONU registers again
    mpcp(mac, true, Epon.broadcastLlid, Mpcp.register, be(0x123, 2) ++ Seq(Mpcp.regDeregisterOnu) ++ be(16, 2) ++ Seq(8))
    cd.waitSampling(100) // the fibre and the MAC
    check(!io.registered.toBoolean && io.llid.toInt == Epon.broadcastLlid, "REGISTER deregister: unregistered")
    discover()
    got = register(0x124)
    check(io.registered.toBoolean && io.llid.toInt == 0x124 && got.exists(_.opcode == Mpcp.registerAck), "registered again, LLID 0x124")

    // the ONU deregisters itself
    c.enable #= false
    gate(0x124, Seq((grantAt(1500), 300)))
    got = waitUs(1800)
    check(got.headOption.exists(u => u.opcode == Mpcp.registerReq && u.field(0) == Mpcp.regDeregister) && !io.registered.toBoolean,
      "enable low: REGISTER_REQ deregister in the next grant, unregistered")
    check(discover().isEmpty, "disabled: no answer to discovery")

    // MPCP timeout
    c.enable #= true
    discover()
    register(0x125)
    gate(0x125, Seq((grantAt(5000), 100)))
    c.timeout #= 1
    check(io.registered.toBoolean, "registered, a 1024 TQ MPCP timeout")
    cd.waitSampling(2 * 1100)
    check(!io.registered.toBoolean, "no GATE for 1024 TQ: unregistered")
  }
  println("PASS OnuMpcpTest")
}
