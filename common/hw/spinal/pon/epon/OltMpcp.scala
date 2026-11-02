// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.epon

import spinal.core._
import spinal.lib._

case class OltMpcpConfig() extends Bundle {
  val enable = Bool()
  val mac = Bits(48 bits)
  val llidBase = UInt(15 bits)    // ONU k gets LLID llidBase + k
  val discoveryPeriod = UInt(16 bits) // 1024 TQ (16.4 us), 0: no discovery
  val discoveryLength, maxRtt, syncTime, horizon = UInt(16 bits) // TQ
  val lead = UInt(16 bits)        // TQ from a GATE's choice to its grant: a data frame in progress, the GATE, the ONU
  val guard = UInt(8 bits)        // TQ between bursts
  val overhead, minGrant, maxGrant = UInt(16 bits) // TQ: added to a REPORT; the grant's bounds
  val timeout = UInt(16 bits)     // no MPCPDU from a registered ONU for this many 1024 TQ: deregistered, 0: never
  val rxResetLead, rxResetWidth = UInt(8 bits) // TQ: the optics' receiver reset ends lead before a burst, 0 width: off
}

object OltMpcpEvent extends SpinalEnum { val REGISTERED, DEREGISTERED, DRIFT, FULL = newElement() }

case class OltMpcpEventData(onus: Int) extends Bundle {
  val kind = OltMpcpEvent()
  val onu = UInt(log2Up(onus) bits)
  val mac = Bits(48 bits)
}

/** OLT MPCP, one byte per clock: local time (TQ), periodic discovery windows (the upstream kept free for their length
  * plus maxRtt), automatic registration of up to `onus` ONUs (REGISTER_REQ -> REGISTER -> GATE -> REGISTER_ACK, a
  * re-registering MAC keeps its entry), round trip of every MPCPDU (rx time of the first byte - timestamp; a change
  * past 8 TQ: DRIFT), one grant at a time per ONU (IPACT): length = its last REPORT (sum of the first queue set) +
  * overhead within [minGrant, maxGrant], force report set, bursts back to back with `guard`, planned at most `horizon`
  * ahead; deregistration by the CPU (`deregister`), by the ONU (REGISTER_REQ deregister) or by `timeout`. `rxReset`:
  * the burst-mode receiver reset of the optics, before each planned burst and each discovery window.
  * Downstream: its MPCPDUs between the data frames (`data`, LLID and mode with the first byte); upstream: non-MPCP
  * frames of registered ONUs on `dataRx` (14 clocks later). */
case class OltMpcp(onus: Int = 8) extends Component {
  import Mpcp._
  val io = new Bundle {
    val config = in(OltMpcpConfig())
    val rx = slave Flow (Fragment(EponRxByte()))
    val rxGood = in Bool ()
    val dataRx = master Flow (Fragment(EponRxByte()))
    val dataRxGood = out Bool ()
    val tx = master Stream (Fragment(EponTxByte()))
    val data = slave Stream (Fragment(EponTxByte()))
    val deregister = slave Flow (UInt(log2Up(onus) bits))
    val events = master Flow (OltMpcpEventData(onus))
    val registered = out Bits (onus bits)
    val time = out UInt (32 bits)
    val rxReset, resetLost = out Bool ()
  }
  val indexBits = log2Up(onus)
  val time = Reg(UInt(32 bits)) init (0)
  val half = RegInit(False)
  half := !half
  when(half)(time := time + 1)
  io.time := time
  def reached(t: UInt): Bool = !(time - t).msb
  def be(bytes: scala.collection.Seq[Bits]): UInt = bytes.reduce(_ ## _).asUInt

  object Onu extends SpinalEnum { val FREE, OFFERED, REGISTERING, REGISTERED = newElement() }
  case class Entry() extends Bundle {
    val state = Onu()
    val mac = Bits(48 bits)
    val rtt, report = UInt(16 bits)
    val pending = Bits(8 bits) // the ONU's pending grants, echoed
    val outstanding, toDeregister = Bool()
    val grantEnd = UInt(32 bits)
    val silent = UInt(16 bits) // 1024 TQ without an MPCPDU
  }
  val entries = Vec(Reg(Entry()), onus)
  for (e <- entries) { e.state init (Onu.FREE); e.outstanding init (False); e.toDeregister init (False) }
  io.registered := Vec(entries.map(_.state === Onu.REGISTERED)).asBits
  io.events.valid := False
  io.events.payload.assignDontCare()
  val rxEvent = CombInit(False)
  def event(kind: OltMpcpEvent.E, onu: UInt, mac: Bits): Unit = {
    io.events.valid := True; io.events.kind := kind; io.events.onu := onu; io.events.mac := mac
  }
  def rxEventOf(kind: OltMpcpEvent.E, onu: UInt, mac: Bits): Unit = { rxEvent := True; event(kind, onu, mac) }
  val tick = CounterFreeRun(2048).willOverflow // 1024 TQ
  for (e <- entries) {
    when(e.outstanding && reached(e.grantEnd))(e.outstanding := False)
    when(tick && e.state =/= Onu.FREE && e.silent =/= 0xFFFF)(e.silent := e.silent + 1)
    when(e.state === Onu.REGISTERED && io.config.timeout =/= 0 && e.silent >= io.config.timeout)(e.toDeregister := True)
  }
  when(io.deregister.valid)(entries(io.deregister.payload).toDeregister := entries(io.deregister.payload).state =/= Onu.FREE)

  // upstream MPCPDUs, acted on at a good end
  val rx = io.rx.payload.fragment
  val position = Reg(UInt(11 bits)) init (0)
  val header = Vec(Reg(Bits(8 bits)), 20)
  val fieldCount = 18
  val fields = Vec(Reg(Bits(8 bits)), fieldCount)
  val firstAt = Reg(UInt(32 bits))
  when(io.rx.valid) {
    position := io.rx.last ? U(0) | position + (position =/= 2047).asUInt
    when(position === 0)(firstAt := time)
    for (k <- 0 until 20) when(position === k)(header(k) := rx.data)
    for (k <- 0 until fieldCount) when(position === 20 + k)(fields(k) := rx.data)
  }
  val sa = header.slice(6, 12).reduce(_ ## _)
  val isMpcp = be(header.take(6)) === multicast && header(12) === 0x88 && header(13) === 0x08 && header(14) === 0
  val opcode = header(15).asUInt
  val ended = io.rx.valid && io.rx.last && io.rxGood && isMpcp && position >= 63 && io.config.enable
  val rtt = (firstAt - be(header.slice(16, 20))).resize(16)
  val offset = rx.llid - io.config.llidBase
  val fromOnu = !rx.mode && offset < onus
  val index = offset.resize(indexBits)
  val known = entries(index)
  val macMatch = Vec(entries.map(e => e.state =/= Onu.FREE && e.mac === sa))
  val free = Vec(entries.map(_.state === Onu.FREE))
  when(ended && opcode === registerReq && rx.llid === Epon.broadcastLlid && fields(0).asUInt === regRegister) {
    val slot = macMatch.orR ? OHToUInt(OHMasking.first(macMatch)) | OHToUInt(OHMasking.first(free))
    when(macMatch.orR || free.orR) {
      val e = entries(slot)
      e.state := Onu.OFFERED // REGISTER to send
      e.mac := sa
      e.rtt := rtt
      e.pending := fields(1)
      e.report := U(0, 16 bits)
      e.silent := U(0, 16 bits)
    } otherwise (rxEventOf(OltMpcpEvent.FULL, U(0), sa))
  }
  when(ended && fromOnu && known.state =/= Onu.FREE && known.state =/= Onu.OFFERED) {
    known.silent := U(0, 16 bits)
    val change = rtt - known.rtt
    when(change > 8 && change < 0xFFF8)(rxEventOf(OltMpcpEvent.DRIFT, index, known.mac))
    known.rtt := rtt
    switch(opcode) {
      is(registerAck) {
        when(known.state === Onu.REGISTERING && fields(0) === 1) {
          known.state := Onu.REGISTERED
          rxEventOf(OltMpcpEvent.REGISTERED, index, known.mac)
        }
      }
      is(report) {
        val count = CountOne(fields(1))
        val queues = (0 until 8).map(q => (count > q) ? be(Seq(fields(2 + 2 * q), fields(3 + 2 * q))).resize(19) | U(0, 19 bits))
        val sum = queues.reduce(_ + _)
        known.report := (sum > 0xFFFF) ? U(0xFFFF, 16 bits) | sum.resize(16)
      }
      is(registerReq)(when(fields(0).asUInt === regDeregister)(known.toDeregister := True))
    }
  }

  // upstream data: frames of registered ONUs that are not MPCPDUs, once the EtherType told
  val passing = Reg(Bool()) init (False)
  when(io.rx.valid && position === 13) {
    passing := !(header(12) === 0x88 && rx.data === 0x08) && fromOnu && known.state === Onu.REGISTERED
  }
  when(io.rx.valid && io.rx.last && position < 13)(passing := False)
  val delayed = Delay(io.rx, 14, init = io.rx.getZero)
  io.dataRx.valid := delayed.valid && passing
  io.dataRx.payload := delayed.payload
  io.dataRxGood := Delay(io.rxGood, 14)

  // downstream MPCPDUs: one at a time, chosen when the generator is free
  object Pdu extends SpinalEnum { val GATE, DISCOVERY, REGISTER, DEREGISTER = newElement() }
  val sending = RegInit(False)
  val pdu = Reg(Pdu())
  val target = Reg(UInt(indexBits bits))
  val grantStart = Reg(UInt(32 bits))
  val grantLength = Reg(UInt(16 bits))
  val cursor = Reg(UInt(32 bits)) init (0) // the OLT time the upstream is free from
  val discoveryDue = RegInit(False)
  val sinceDiscovery = Reg(UInt(16 bits)) init (0)
  when(tick)(sinceDiscovery := sinceDiscovery + 1)
  when(io.config.discoveryPeriod =/= 0 && sinceDiscovery >= io.config.discoveryPeriod) { discoveryDue := True; sinceDiscovery := 0 }
  val pointer = Reg(UInt(indexBits bits)) init (0)
  val offered = Vec(entries.map(_.state === Onu.OFFERED))
  val leaving = Vec(entries.map(_.toDeregister))
  val ready = Vec(entries.map(e => (e.state === Onu.REGISTERING || e.state === Onu.REGISTERED) && !e.outstanding && !e.toDeregister))
  val ahead = cursor - time
  val roomAhead = ahead.msb || ahead < io.config.horizon
  val next = entries(pointer)
  val wanted = (next.report +^ io.config.overhead).resize(17)
  val length = (wanted < io.config.minGrant) ? io.config.minGrant | ((wanted > io.config.maxGrant) ? io.config.maxGrant | wanted.resize(16))
  val earliest = RegNext(time + io.config.lead) // half a TQ stale: within the lead
  val from = (cursor - earliest).msb ? earliest | cursor // the OLT time a burst may arrive from
  val gateArrival = (from - (earliest + next.rtt)).msb ? (earliest + next.rtt) | from
  val arrivals = StreamFifo(UInt(32 bits), 16) // planned, in order (after the cursor): one per ONU, discovery windows
  arrivals.io.push.valid := False
  arrivals.io.push.payload := gateArrival
  val inData = RegInit(False)
  val dataFrame = Bool() // the frame on io.tx is a data frame
  val sent = io.tx.fire && io.tx.last && !dataFrame
  when(!sending && io.config.enable) {
    when(leaving.orR) {
      target := OHToUInt(OHMasking.first(leaving)); pdu := Pdu.DEREGISTER; sending := True
    } elsewhen (offered.orR) {
      target := OHToUInt(OHMasking.first(offered)); pdu := Pdu.REGISTER; sending := True
    } elsewhen (discoveryDue && roomAhead) {
      discoveryDue := False
      pdu := Pdu.DISCOVERY; sending := True
      arrivals.io.push.valid := True
      arrivals.io.push.payload := from
      grantStart := from
      grantLength := io.config.discoveryLength
      cursor := from + io.config.discoveryLength + io.config.maxRtt + io.config.guard
    } elsewhen (roomAhead) {
      pointer := pointer + 1
      when(ready(pointer)) {
        target := pointer; pdu := Pdu.GATE; sending := True
        arrivals.io.push.valid := True
        grantStart := gateArrival - next.rtt
        grantLength := length
        cursor := gateArrival + length + io.config.guard
        next.outstanding := True
        next.grantEnd := gateArrival + length
      }
    }
  }
  io.resetLost := arrivals.io.push.valid && !arrivals.io.push.ready
  val coming = arrivals.io.pop
  val resetEnd = coming.payload - io.config.rxResetLead
  val resetBegin = resetEnd - io.config.rxResetWidth
  coming.ready := reached(coming.payload)
  io.rxReset := RegNext(coming.valid && reached(resetBegin) && !reached(resetEnd) && io.config.rxResetWidth =/= 0) init (False)
  val chosen = entries(target)
  val left = Reg(Flow(OltMpcpEventData(onus))) // DEREGISTERED, out when no upstream event takes the clock
  left.valid init (False)
  when(left.valid && !rxEvent) { event(OltMpcpEvent.DEREGISTERED, left.onu, left.mac); left.valid := False }
  when(sent) {
    sending := False
    when(pdu === Pdu.REGISTER)(chosen.state := Onu.REGISTERING)
    when(pdu === Pdu.DEREGISTER) {
      chosen.state := Onu.FREE
      chosen.toDeregister := False
      chosen.outstanding := False
      left.valid := True
      left.onu := target
      left.mac := chosen.mac
      left.kind := OltMpcpEvent.DEREGISTERED
    }
  }

  // the MPCPDU bytes
  def bytesOf(v: UInt, n: Int): Seq[Bits] = (n - 1 to 0 by -1).map(k => v.asBits(8 * k + 7 downto 8 * k))
  val pduIndex = Reg(UInt(6 bits)) init (0)
  val stampOut = Reg(UInt(32 bits))
  val stampNow = (pduIndex === 0) ? time | stampOut
  val unicast = pdu === Pdu.GATE
  val fieldsOut = Vec(Bits(8 bits), 9)
  fieldsOut.foreach(_ := 0)
  val grantBytes = bytesOf(grantStart, 4) ++ bytesOf(grantLength, 2)
  switch(pdu) {
    is(Pdu.GATE) { fieldsOut(0) := 0x11; for (k <- 0 until 6) fieldsOut(1 + k) := grantBytes(k) } // 1 grant, force report
    is(Pdu.DISCOVERY) {
      fieldsOut(0) := 0x09 // 1 grant, discovery
      for (k <- 0 until 6) fieldsOut(1 + k) := grantBytes(k)
      for ((b, k) <- bytesOf(io.config.syncTime, 2).zipWithIndex) fieldsOut(7 + k) := b
    }
    is(Pdu.REGISTER, Pdu.DEREGISTER) {
      val fs = bytesOf((io.config.llidBase + target).resize(16), 2) ++
        Seq((pdu === Pdu.REGISTER) ? B(regAck, 8 bits) | B(regDeregisterOnu, 8 bits)) ++ bytesOf(io.config.syncTime, 2) :+ chosen.pending
      for ((b, k) <- fs.zipWithIndex) fieldsOut(k) := b
    }
  }
  val toOnu = pdu === Pdu.REGISTER || pdu === Pdu.DEREGISTER
  val pduBytesOut = Vec((toOnu ? chosen.mac | B(multicast, 48 bits)).subdivideIn(8 bits).reverse ++
    io.config.mac.subdivideIn(8 bits).reverse ++ Seq(B(0x88, 8 bits), B(0x08, 8 bits), B(0, 8 bits),
    pdu.mux(Pdu.GATE -> B(gate, 8 bits), Pdu.DISCOVERY -> B(gate, 8 bits), Pdu.REGISTER -> B(register, 8 bits),
      Pdu.DEREGISTER -> B(register, 8 bits))) ++ bytesOf(stampNow, 4) ++ fieldsOut ++ Seq.fill(pduBytes - 29)(B(0, 8 bits)))

  // frame boundaries: an MPCPDU waits for the data frame in progress (`lead` covers it), a data frame starts only
  // when none is waiting
  dataFrame := inData || !sending
  when(io.tx.fire)(inData := dataFrame && !io.tx.last)
  io.tx.valid := dataFrame ? io.data.valid | True
  io.tx.last := dataFrame ? io.data.last | pduIndex === pduBytes - 1
  io.tx.fragment.data := dataFrame ? io.data.fragment.data | pduBytesOut(pduIndex)
  io.tx.fragment.mode := dataFrame ? io.data.fragment.mode | !unicast
  io.tx.fragment.fcs := dataFrame ? io.data.fragment.fcs | True
  io.tx.fragment.llid := dataFrame ? io.data.fragment.llid | (unicast ? (io.config.llidBase + target) | U(Epon.broadcastLlid, 15 bits))
  io.data.ready := dataFrame && io.tx.ready
  when(io.tx.fire && !dataFrame) {
    pduIndex := io.tx.last ? U(0) | pduIndex + 1
    when(pduIndex === 0)(stampOut := time)
  }
}
