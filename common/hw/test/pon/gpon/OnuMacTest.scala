// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba3.apb.sim.Apb3Driver
import pon.{Crc32, EthFrames, GemEthBridge}
import scala.collection.mutable
import GponCodes._
import GtcDsTest.{bigToBytes, bytesToBig, check}

/** Our OLT and ONU MACs, each with its Ethernet bridge (ONU data GEM upstream from it); the test carries their lines
  * (fibre delay, cut). Test only. */
case class PonLoop() extends Component {
  val olt = OltMac(8, 8)
  val onu = OnuMac()
  val io = new Bundle {
    val usClk = in Bool ()
    val usReset = in Bool ()
    val oltDs = out Bits (8 bits)
    val oltUs = in Bits (8 bits)
    val onuDs = in Bits (8 bits)
    val onuUs = out Bits (8 bits)
    val onuBen = out Bits (8 bits)
    val oltApb = slave(cloneOf(olt.io.apb))
    val onuApb = slave(cloneOf(onu.io.apb))
    val oltIrq = out Bool ()
    val onuIrq = out Bool ()
    val oltGem = slave(cloneOf(olt.io.gem))      // downstream GEM, OLT side
    val oltUsGem = master(cloneOf(olt.io.usGem))
    val onuGemTx = slave(cloneOf(onu.io.gemTx))  // upstream GEM, ONU side
    val onuGemRx = master(cloneOf(onu.io.gemRx))
    val onuGemBacklog = in UInt (16 bits)
    val ethClk = in Bool ()
    val oltEth, onuEth = master(EthFrames())
    val oltBridgeApb, onuBridgeApb = slave(Apb3(Apb3Config(addressWidth = 8, dataWidth = 32)))
  }
  val usDomain = ClockDomain(io.usClk, io.usReset)
  val ethDomain = ClockDomain(io.ethClk, io.usReset)
  olt.io.usClk := io.usClk
  olt.io.usReset := io.usReset
  onu.io.usClk := io.usClk
  onu.io.usReset := io.usReset
  val oltBridge = GemEthBridge(usDomain, usDomain, ClockDomain.current, ethDomain, ethDomain, defaultAction = 1)
  oltBridge.io.ponRx << olt.io.usGem
  oltBridge.io.ponRxCut := olt.io.usGemCut
  oltBridge.io.cpuPort.setIdle()
  io.oltUsGem << oltBridge.io.toCpu
  olt.io.gem << StreamArbiterFactory.lowerFirst.fragmentLock.onArgs(io.oltGem, oltBridge.io.ponTx)
  io.oltEth <> oltBridge.io.eth
  oltBridge.io.apb <> io.oltBridgeApb
  val onuBridge = GemEthBridge(ClockDomain.current, ClockDomain.current, usDomain, ethDomain, ethDomain, defaultAction = 0)
  onuBridge.io.ponRx << onu.io.gemRx
  onuBridge.io.ponRxCut := onu.io.gemRxCut
  onuBridge.io.cpuPort << onu.io.omciPort
  io.onuGemRx << onuBridge.io.toCpu
  onu.io.gemTx << io.onuGemTx
  onu.io.gemTxBacklog := io.onuGemBacklog
  onu.io.dataTx << onuBridge.io.ponTx
  onu.io.dataTxBacklog := onuBridge.io.ponTxBacklog
  io.onuEth <> onuBridge.io.eth
  onuBridge.io.apb <> io.onuBridgeApb
  io.oltDs := olt.io.dsLine
  olt.io.usLine := io.oltUs
  onu.io.dsLine := io.onuDs
  io.onuUs := onu.io.usLine
  io.onuBen := onu.io.usBen
  olt.io.apb <> io.oltApb
  onu.io.apb <> io.onuApb
  io.oltIrq := olt.io.irq
  io.onuIrq := onu.io.irq
  olt.framer.io.frameStart.simPublic()
  olt.us.receiver.io.burst.simPublic()
  olt.us.receiver.io.dbru.simPublic()
  onu.upstream.ploam.io.state.simPublic()
  onu.upstream.ploam.io.onuId.simPublic()
  onu.upstream.ploam.io.eqd.simPublic()
  onu.upstream.ploam.io.omciValid.simPublic()
  onu.upstream.ploam.io.omciPort.simPublic()
  onu.deframer.io.synced.simPublic()
}

/** An Ethernet port on a bridge: frames sent with a 12-clock gap, frames received collected. Test only. */
class EthSide(f: EthFrames, cd: ClockDomain) {
  val toSend = mutable.Queue[List[Int]]()
  val received = mutable.ArrayBuffer[List[Int]]()
  private val current = mutable.ArrayBuffer[Int]()
  private var index, gap = 0
  f.rx.valid #= false
  f.rxBad #= false
  f.tx.ready #= true
  cd.onSamplings {
    if (f.tx.valid.toBoolean) {
      current += f.tx.fragment.toInt
      if (f.tx.last.toBoolean) { received += current.toList; current.clear() }
    }
    f.rx.valid #= false
    if (gap > 0) gap -= 1
    else for (frame <- toSend.headOption) {
      f.rx.valid #= true
      f.rx.fragment #= frame(index)
      f.rx.last #= index == frame.length - 1
      index += 1
      if (index == frame.length) { toSend.dequeue(); index = 0; gap = 12 }
    }
  }
}

/** A CPU on one MAC's registers: queued register writes and PLOAMs, interrupts served. Test only. */
class Cpu(apb: Apb3Driver, cd: ClockDomain, irq: Bool) {
  val writes = mutable.Queue[(BigInt, BigInt)]()
  val reads = mutable.Queue[(BigInt, BigInt => Unit)]()
  def word(m: BigInt, k: Int): BigInt = (m >> (64 - 32 * k)) & 0xFFFFFFFFL
  def send(m: BigInt, tx0: BigInt): Unit = for (k <- 0 until 3) writes += ((tx0 + 4 * k, word(m, k)))
  def busy = writes.nonEmpty || reads.nonEmpty
  /** `serve` reads what the interrupt is about; returns when nothing is left. */
  def run(serve: Apb3Driver => Unit): Unit = fork {
    while (true) {
      cd.waitSamplingWhere(irq.toBoolean || busy)
      while (writes.nonEmpty) { val (a, v) = writes.dequeue(); apb.write(a, v) }
      while (reads.nonEmpty) { val (a, f) = reads.dequeue(); f(apb.read(a)) }
      if (irq.toBoolean) serve(apb)
    }
  }
}

/** Closed loop OLT MAC <-> ONU MAC over a simulated fibre, both driven by CPU models through their registers:
  * activation to O5 and the ONU response time, Assign_Alloc-ID / Configure_Port-ID / BER_Interval acknowledged,
  * Request_Password answered 3 times, a CPU PLOAM each way, a DBA T-CONT in the ONU's burst with its DBRu,
  * OMCI over GEM both ways (upstream fragmented in the default allocation, Ethernet frames at once in the T-CONT with
  * their DBRu), Ethernet frames end to end both ways through the bridges, a fibre cut
  * (O6) restored with a downstream shift then a directed POPUP keeping the EqD to the bit, Disable_Serial_Number
  * (O7, laser dark), re-activation, a cut longer than TO2 (O1). */
object OnuMacTest extends App {
  val act = new OltActivation(1) // the OLT settings of the activation tests
  val sn = BigInt("414C434C12345678", 16)
  val password = BigInt("30313233343536373839", 16)
  val oneWay = 6223 // upstream bits, 5 us: 1 km
  val responseBits = 43546 // 35 us
  val snPeriod = 8 // frames between SN windows: normal frames in between
  def ploam(bytes: Int*): BigInt = bytesToBig(bytes.padTo(12, 0))

  SimConfig.compile(PonLoop()).doSim { dut =>
    val ds = dut.clockDomain
    val us = dut.usDomain
    val io = dut.io
    val onuP = dut.onu.upstream.ploam.io
    for (apb <- Seq(io.oltApb, io.onuApb)) { apb.PSEL #= 0; apb.PENABLE #= false }
    io.oltUs #= 0
    val oltGem = new GtcDsTest.GemDriver(io.oltGem, ds)
    val onuGem = new GtcDsTest.GemDriver(io.onuGemTx, us)
    us.onSamplings(io.onuGemBacklog #= onuGem.queue.map(_._2.length).sum) // whole packets until sent
    val oltEth = new EthSide(io.oltEth, dut.ethDomain)
    val onuEth = new EthSide(io.onuEth, dut.ethDomain)
    for (apb <- Seq(io.oltBridgeApb, io.onuBridgeApb)) { apb.PSEL #= 0; apb.PENABLE #= false }
    io.ethClk #= false
    fork { while (true) { sleep(5); io.ethClk #= !io.ethClk.toBoolean } }
    io.onuDs #= 0
    ds.fallingEdge(); us.fallingEdge()
    ds.assertReset(); us.assertReset()
    fork {
      var n = 0
      while (true) { ds.clockToggle(); if (n % 2 == 0) us.clockToggle(); n += 1; sleep(2) }
    }
    sleep(41)
    ds.deassertReset(); us.deassertReset()

    // fibre: downstream delay in downstream bits, upstream in upstream bits; a cut feeds zeros
    val ring = 1 << 20
    val dsBits, usBits = new Array[Boolean](ring)
    var dsShift = 0
    var dsCut = false
    var dsCycle, usCycle = 0L
    var frames = 0
    val psyncs = mutable.ArrayBuffer[Long]() // each frame's PSync at the ONU input, downstream bits
    def byteAt(bits: Array[Boolean], start: Long): Int =
      (0 until 8).foldLeft(0)((a, k) => (a << 1) | (if (start + k >= 0 && bits(((start + k) & (ring - 1)).toInt)) 1 else 0))
    def put(bits: Array[Boolean], start: Long, v: Int): Unit =
      for (k <- 0 until 8) bits(((start + k) & (ring - 1)).toInt) = ((v >> (7 - k)) & 1) == 1
    ds.onSamplings {
      put(dsBits, dsCycle * 8, io.oltDs.toInt)
      if (dut.olt.framer.io.frameStart.toBoolean) { psyncs += dsCycle * 8 + 2 * oneWay + dsShift; frames += 1 }
      dsCycle += 1
      io.onuDs #= (if (dsCut) 0 else byteAt(dsBits, dsCycle * 8 - 2 * oneWay - dsShift))
    }
    val benRises = mutable.ArrayBuffer[Long]() // upstream bit of each burst's first laser-on bit
    var benOn = false
    var leak = 0 // line bits set with the laser off
    val bursts = mutable.ArrayBuffer[(Int, Boolean, Int, Int, Long)]() // frame, found, allocId, onuId, delayBits
    val dbrus = mutable.ArrayBuffer[(Int, Boolean, Int)]() // allocId, CRC good, mode 0 report
    us.onSamplings {
      val line = io.onuUs.toInt
      val ben = io.onuBen.toInt
      for (k <- 0 until 8) {
        val on = ((ben >> (7 - k)) & 1) == 1
        if (((line >> (7 - k)) & 1) == 1 && !on) leak += 1
        if (on && !benOn) benRises += usCycle * 8 + k
        benOn = on
      }
      put(usBits, usCycle * 8, line)
      val b = dut.olt.us.receiver.io.burst
      if (b.valid.toBoolean) bursts += ((frames, b.found.toBoolean, b.allocId.toInt, b.onuId.toInt, b.delayBits.toLong))
      val d = dut.olt.us.receiver.io.dbru
      if (d.valid.toBoolean) dbrus += ((d.allocId.toInt, d.crcOk.toBoolean, d.report.toInt & 0xFF))
      usCycle += 1
      io.oltUs #= byteAt(usBits, usCycle * 8 - oneWay)
    }
    val onuRx = mutable.ArrayBuffer[BigInt]()
    def waitFrames(n: Int): Unit = { val f = frames; us.waitSamplingWhere(frames >= f + n) }
    def state = onuP.state.toEnum.position + 1 // O1 = 1
    def waitState(s: Int, within: Int): Unit = {
      val f = frames
      us.waitSamplingWhere(state == s || frames > f + within)
      check(state == s, s"ONU reaches O$s within $within frames (O$state)")
    }

    // OLT CPU
    val r = dut.olt.us.regs
    val olt = new Cpu(Apb3Driver(io.oltApb, us), us, io.oltIrq)
    val events = mutable.ArrayBuffer[(String, Int, Long)]()
    val oltRx = mutable.ArrayBuffer[BigInt]()
    olt.run { apb =>
      val status = apb.read(r.STATUS.addr)
      if (status.testBit(0)) {
        val e0 = apb.read(r.EVENT_0.addr)
        val eqd = apb.read(r.EVENT_3.addr).toLong
        events += ((Seq("SN_FOUND", "RANGED", "RANGING_FAILED", "NO_FREE_ID")((e0 & 3).toInt), (e0 >> 2).toInt & 0xFF, eqd))
      }
      if (status.testBit(1))
        oltRx += (apb.read(r.PLOAM_RX_0.addr) << 64) | (apb.read(r.PLOAM_RX_1.addr) << 32) | apb.read(r.PLOAM_RX_2.addr)
    }
    def oltSend(m: BigInt): Unit = olt.send(m, r.PLOAM_TX_0.addr)
    def word(bytes: Seq[Int]): BigInt = bytesToBig(bytes)
    val uo = act.upstreamOverhead
    olt.writes ++= Seq(r.UO_0.addr -> word(uo.slice(0, 4)), r.UO_1.addr -> word(uo.slice(4, 8)), r.UO_2.addr -> word(uo.slice(8, 10)),
      r.TEQD.addr -> BigInt(act.teqd), r.RTD_MIN.addr -> BigInt(act.rtdMin), r.RANGING.addr -> BigInt(act.reach | (act.requestStart << 16)),
      r.WINDOW.addr -> BigInt(act.quietBefore | (snPeriod << 8)), r.GRANT.addr -> BigInt(act.grantBytes | (act.gapBytes << 16)),
      r.IRQ_ENABLE.addr -> BigInt(3), r.CTRL.addr -> BigInt(3)) // activation, DS FEC on (stock OLTs' default)

    // ONU CPU
    val q = dut.onu.upstream.regs
    val onu = new Cpu(Apb3Driver(io.onuApb, us), us, io.onuIrq)
    val states = mutable.ArrayBuffer[Int]()
    onu.run { apb =>
      if (apb.read(q.EVENTS.addr).testBit(0)) {
        apb.write(q.EVENTS.addr, 1)
        states += (apb.read(q.STATE.addr) & 7).toInt + 1
      }
      if (apb.read(q.STATUS.addr).testBit(0))
        onuRx += (apb.read(q.PLOAM_RX_0.addr) << 64) | (apb.read(q.PLOAM_RX_1.addr) << 32) | apb.read(q.PLOAM_RX_2.addr)
    }
    onu.writes ++= Seq(q.SN_0.addr -> (sn >> 32), q.SN_1.addr -> (sn & 0xFFFFFFFFL), q.PASSWORD_0.addr -> (password >> 48),
      q.PASSWORD_1.addr -> ((password >> 16) & 0xFFFFFFFFL), q.PASSWORD_2.addr -> (password & 0xFFFF),
      q.TIMERS.addr -> (BigInt(80000) | (BigInt(200) << 20)), q.IRQ_ENABLE.addr -> BigInt(3), q.CTRL.addr -> BigInt(1))

    // activation
    waitState(5, 60)
    val ranged = events.filter(_._1 == "RANGED")
    val onuId = onuP.onuId.toInt
    check(ranged.length == 1 && ranged.head._2 == onuId && onuP.eqd.toLong == ranged.head._3,
      s"ranged: ONU-ID $onuId, ONU EqD ${onuP.eqd.toLong} = OLT's ($ranged)")
    waitFrames(20) // normal bursts only in the last 4
    val eqd1 = onuP.eqd.toLong
    // response: from the frame's PSync at the ONU input to its upstream frame start (EqD and StartTime removed)
    val payloadAfterBen = 96 - 32 + 24 // overhead (guard 32 laser off) + PLOu
    val responses = benRises.takeRight(4).map { rise =>
      psyncs.map(p => rise + payloadAfterBen - p / 2 - eqd1 - act.gapBytes * 8).filter(x => x > responseBits / 2 && x < responseBits * 2).min
    }
    check(responses.forall(x => (x - responseBits).abs <= 2), s"ONU response time 35 us (${responses.head} bits; G.984.3: +- 1 us)")
    val ranged1 = bursts.filter(x => x._1 > frames - 4 && x._4 == onuId)
    check(ranged1.nonEmpty && ranged1.forall(x => x._2 && x._5 == 0), s"ONU bursts at the OLT to the bit ($ranged1)")

    // PLOAMs: acknowledged, password, the CPUs' own
    oltSend(ploam(onuId, 0x0A, 0x12, 0xC0, 0x01))       // Assign_Alloc-ID 300, GEM
    oltSend(ploam(onuId, 0x0E, 0x01, 0x12, 0x30))       // Configure_Port-ID 0x123
    oltSend(ploam(onuId, 0x12, 0x00, 0x00, 0x1F, 0x40)) // BER_Interval 8000 frames
    oltSend(ploam(onuId, 0x09))                         // Request_Password
    onu.send(ploam(onuId, 0x03), q.PLOAM_TX_0.addr)     // Dying_Gasp
    val f0 = frames
    us.waitSamplingWhere(oltRx.length >= 7 || frames > f0 + 30) // one PLOAMu per frame
    def acked(m: BigInt) = { val d = bigToBytes(m, 12); ploam(Seq(onuId, 0x09, d(1), d(0), d(1)) ++ d.slice(2, 9): _*) }
    for (m <- Seq(ploam(onuId, 0x0A, 0x12, 0xC0, 0x01), ploam(onuId, 0x0E, 0x01, 0x12, 0x30), ploam(onuId, 0x12, 0x00, 0x00, 0x1F, 0x40)))
      check(oltRx.count(_ == acked(m)) == 1 && onuRx.contains(m), f"PLOAM 0x${bigToBytes(m, 12)(1)}%02X: to the ONU CPU, acknowledged once")
    check(oltRx.count(_ == ((BigInt(onuId) << 88) | (BigInt(2) << 80) | password)) == 3, "Request_Password: Password 3 times")
    check(oltRx.contains(ploam(onuId, 0x03)), "the ONU CPU's Dying_Gasp reaches the OLT CPU")
    check(dut.onu.upstream.ploam.io.omciValid.toBoolean && dut.onu.upstream.ploam.io.omciPort.toInt == 0x123, "OMCI Port-ID 0x123")

    // DBA: the CPU grants T-CONT 300 behind the default allocation, with a DBRu
    olt.writes ++= Seq(r.TCONT_A.addr -> BigInt(0 | (300 << 8) | (onuId << 20) | (1 << 28)), r.TCONT_B.addr -> BigInt(100 | (100 << 16)),
      r.DBA.addr -> BigInt(1 | (48 << 8)), r.WINDOW.addr -> BigInt(act.quietBefore | (0xFFFF << 8)))
    waitFrames(12) // an SN window already begun takes up to 4 of them
    val dbru300 = dbrus.filter(_._1 == 300)
    check(dbru300.length >= 4 && dbru300.forall(_._2), s"T-CONT 300 granted in the ONU's burst, ${dbru300.length} DBRu with good CRC")
    check(bursts.filter(x => x._1 > frames - 4).forall(x => x._2 && x._5 == 0), "two-allocation bursts to the bit")

    // OMCI over GEM: the ONU CPU's packets upstream in its default allocation, fragmented to fit (50 bytes after the
    // PLOAMu), Ethernet frames through the ONU bridge at the same time in T-CONT 300; downstream only the OMCI Port-ID
    // reaches the ONU CPU (the bridge drops other Port-IDs by default)
    val usFragments = mutable.ArrayBuffer[(Int, Int, Boolean, List[Int])]() // allocId, Port-ID, end, bytes
    val open = mutable.ArrayBuffer[Int]()
    us.onSamplings {
      val g = io.oltUsGem
      if (g.valid.toBoolean) {
        open += g.gem.data.toInt
        if (g.last.toBoolean) {
          usFragments += ((g.allocId.toInt, g.gem.portId.toInt, g.gem.pti.toInt % 2 == 1, open.toList))
          open.clear()
        }
      }
    }
    val dsGem = mutable.ArrayBuffer[(Int, Int)]() // Port-ID, byte
    ds.onSamplings { if (io.onuGemRx.valid.toBoolean) dsGem += ((io.onuGemRx.gem.portId.toInt, io.onuGemRx.gem.data.toInt)) }
    val omci = Seq.tabulate(2)(k => List.tabulate(48)(i => (i * 7 + k * 101) & 0xFF))
    val data = Seq.tabulate(2)(k => List.tabulate(150)(i => (i * 13 + k * 31) & 0xFF)) // untagged frames
    def withFcs(frame: List[Int]): List[Int] = { val fcs = Crc32.model(frame); frame ++ (0 until 4).map(k => (fcs >> (8 * k)).toInt & 0xFF) }
    val oltBridge = Apb3Driver(io.oltBridgeApb, ds)
    val onuBridge = Apb3Driver(io.onuBridgeApb, ds)
    val br = dut.onuBridge
    onuBridge.write(br.UNTAGGED.addr, 0x200 | (1 << 12))
    onu.writes += ((q.DATA_ALLOC.addr, BigInt(300 | (1 << 12))))
    us.waitSamplingWhere(!onu.busy)
    onuGem.queue ++= omci.map(0x123 -> _)
    onuEth.toSend ++= data
    val dbruBefore = dbrus.length
    val f2 = frames
    us.waitSamplingWhere(usFragments.count(_._3) == 4 || frames > f2 + 10)
    def packets(allocId: Int) = usFragments.filter(_._1 == allocId).foldLeft((List[List[Int]](), List[Int]())) {
      case ((done, cur), f) => if (f._3) (done :+ (cur ++ f._4), Nil) else (done, cur ++ f._4) }._1
    val omciFragments = usFragments.filter(_._1 == onuId)
    check(packets(onuId) == omci && omciFragments.forall(_._2 == 0x123) && omciFragments.length > 2,
      s"ONU OMCI upstream: 2 packets in ${omciFragments.length} fragments of the default allocation (${omciFragments.map(_._4.length)})")
    val dataFragments = usFragments.filter(_._1 == 300)
    check(packets(300) == data.map(withFcs) && dataFragments.forall(_._2 == 0x200) && dataFragments.length > 2,
      s"ONU Ethernet frames upstream meanwhile, FCS appended: ${dataFragments.length} fragments of T-CONT 300 (${dataFragments.map(_._4.length)})")
    waitFrames(2)
    val reports = dbrus.drop(dbruBefore).filter(_._1 == 300).map(_._3)
    check(reports.exists(_ > 0) && reports.forall(_ <= (308 + 47) / 48) && reports.last == 0,
      s"T-CONT 300 DBRu follows the backlog, 48-byte blocks (${reports.mkString(" ")})")
    val omciDown = List.tabulate(48)(i => 255 - i)
    oltGem.queue ++= Seq(0x456 -> List.fill(20)(0x5A), 0x123 -> omciDown)
    waitFrames(3)
    check(dsGem.toList == omciDown.map(0x123 -> _), "downstream: only the OMCI Port-ID to the ONU CPU")

    // Ethernet end to end: OLT port -> Port-ID 0x456 -> ONU port, ONU port -> Port-ID 0x200 -> OLT port
    oltBridge.write(dut.oltBridge.UNTAGGED.addr, 0x456 | (1 << 12))
    oltBridge.write(dut.oltBridge.PORT_ACTION.addr, 0x200 | (2 << 12))
    onuBridge.write(br.PORT_ACTION.addr, 0x456 | (2 << 12))
    val down = List.tabulate(1000)(i => (i * 5 + 1) & 0xFF)
    val up = List.tabulate(64)(i => (i * 3 + 7) & 0xFF)
    oltEth.toSend += down
    onuEth.toSend += up
    waitFrames(3)
    check(onuEth.received.toList == List(down) && oltEth.received.toList == List(up),
      "Ethernet frames end to end: 1000 bytes OLT -> ONU, 64 bytes ONU -> OLT, FCS checked and stripped")
    check(oltBridge.read(dut.oltBridge.DROPS.addr) == 0 && onuBridge.read(br.DROPS.addr) == 0, "no bridge drop")

    var fec = Seq[BigInt]()
    for (reg <- Seq(q.FEC(0), q.FEC(3)) /* codewords, uncorrectable */) onu.reads += ((reg.addr, x => fec :+= x))
    us.waitSamplingWhere(fec.length == 2)
    check(fec(0) > 20 * 153 && fec(1) == 0, s"DS FEC since activation: ${fec(0)} codewords decoded, ${fec(1)} uncorrectable")

    // fibre cut: O6; back with the downstream 11 bits later (5.5 upstream bits), directed POPUP: same EqD
    dsCut = true
    waitState(6, 8)
    waitFrames(4)
    dsShift = 11
    dsCut = false
    us.waitSamplingWhere(dut.onu.deframer.io.synced.toBoolean)
    check(state == 6, "LOS cleared: still O6 until POPUP")
    oltSend(ploam(onuId, 0x0C))
    waitState(5, 4)
    waitFrames(4)
    val back = bursts.filter(x => x._1 > frames - 3 && x._4 == onuId)
    check(back.nonEmpty && back.forall(x => x._2 && (x._5 == 5 || x._5 == 6)) && onuP.eqd.toLong == eqd1,
      s"after POPUP the EqD is kept and bursts follow the fibre: 5.5 bits later (${back.map(_._5).mkString(" ")})")

    // Disable_Serial_Number: O7, laser dark; enable all: activation again, EqD 5.5 bits shorter
    oltSend(ploam(0xFF, 0x06, 0xFF) | (sn << 8))
    waitState(7, 4)
    val dark = benRises.length
    waitFrames(6)
    check(benRises.length == dark, "O7: no burst")
    olt.writes += ((r.RELEASE.addr, BigInt(onuId)))
    olt.writes += ((r.WINDOW.addr, BigInt(act.quietBefore | (snPeriod << 8))))
    oltSend(ploam(0xFF, 0x06, 0x0F))
    waitState(5, 60)
    val eqd2 = onuP.eqd.toLong
    check(events.count(_._1 == "RANGED") == 2 && (eqd1 - eqd2 == 5 || eqd1 - eqd2 == 6), s"re-activated, EqD $eqd2 = $eqd1 - 5.5")
    waitFrames(4)

    // a cut longer than TO2 (10 frames here): O6, then O1
    onu.writes += ((q.TIMERS.addr, BigInt(80000) | (BigInt(10) << 20)))
    dsCut = true
    waitState(6, 8)
    val lost = frames
    waitState(1, 14)
    check(frames - lost >= 10, s"TO2: O6 -> O1 after ${frames - lost} frames")
    check(leak == 0, "no line bit with the laser off")
    var errors = Seq[BigInt]()
    olt.reads += ((r.ERRORS.addr, x => errors :+= x))
    onu.reads += ((q.ERRORS.addr, x => errors :+= x))
    us.waitSamplingWhere(errors.length == 2)
    check(errors.forall(_ == 0), s"no OLT or ONU register error (${errors.mkString(" ")})")
    val f1 = frames
    us.waitSamplingWhere(states.last == 1 || frames > f1 + 2) // the CPU serves the last state event
    check(states.takeRight(2) == Seq(6, 1) && states.contains(7), s"ONU state events (${states.mkString(" ")})")
  }
  println("PASS OnuMacTest")
}
