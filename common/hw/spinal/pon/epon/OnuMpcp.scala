// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.epon

import spinal.core._
import spinal.lib._

/** MPCP (IEEE 802.3 clause 64) frame layout: MAC Control frames of 64 bytes, DA 01-80-C2-00-00-01 (REGISTER: the
  * ONU's MAC), EtherType 0x8808, opcode at byte 14, timestamp (TQ) at 16, fields from 20. */
object Mpcp {
  val (gate, report, registerReq, register, registerAck) = (2, 3, 4, 5, 6)
  val multicast = BigInt("0180C2000001", 16)
  val fieldBytes = 27 // the longest: a GATE of 4 grants and a sync time
  val pduBytes = 60   // without the FCS
  def cost(bytes: Int): Int = (bytes + 8 + 4 + Epon.ipg + 2 + 1) / 2 // TQ on the line: preamble, FCS, IPG, alignment
  val (regRegister, regDeregister) = (1, 3)                      // REGISTER_REQ flags
  val (regReregister, regDeregisterOnu, regAck, regNack) = (1, 2, 3, 4) // REGISTER flags
}

case class OnuMpcpConfig() extends Bundle {
  val enable = Bool()            // low: deregistered (REGISTER_REQ deregister in the next grant if registered)
  val mac = Bits(48 bits)
  val laserOn, laserOff = UInt(8 bits) // TQ
  val alwaysReport = Bool()      // a REPORT in every grant, not only when forced
  val timeout = UInt(16 bits)    // no GATE for this many 1024 TQ (16.4 us) while registered: deregistered, 0: never
}

case class EponUsByte() extends Bundle {
  val data = Bits(8 bits)
  val length = UInt(11 bits) // frame bytes without the FCS, with the first byte
}

/** ONU MPCP, one byte per clock: local time (TQ, every 2 clocks) set by each MPCPDU's timestamp, discovery
  * (REGISTER_REQ at a random offset of the discovery window), REGISTER, REGISTER_ACK in the first grant, GATE grants
  * (a FIFO of 8; a grant already started is dropped, `late`), bursts: laser on (`burst`) from the grant start to its
  * end less laserOff, idle for laserOn + syncTime, then REGISTER_ACK, deregistration, REPORT (forced or always: one
  * queue set, queue 0 = `report` TQ), data frames while they fit, in order. `accept`: a downstream frame is ours
  * (unicast LLID, or broadcast but not our own LLID; unregistered: broadcast). `drift`: a timestamp more than 12 TQ
  * off while registered (guardThresholdONU). */
case class OnuMpcp() extends Component {
  import Mpcp._
  val io = new Bundle {
    val config = in(OnuMpcpConfig())
    val rx = slave Flow (Fragment(EponRxByte()))
    val rxGood = in Bool () // with rx.last
    val accept = out Bool () // with each rx byte
    val tx = master Stream (Fragment(EponTxByte()))
    val data = slave Stream (Fragment(EponUsByte()))
    val report = in UInt (16 bits)
    val burst = out Bool ()
    val registered = out Bool ()
    val llid = out UInt (15 bits)
    val time = out UInt (32 bits)
    val drift, late, grantLost, timedOut = out Bool ()
  }
  object Registration extends SpinalEnum { val UNREGISTERED, REQUESTED, ACKING, REGISTERED = newElement() }
  import Registration._
  val state = RegInit(UNREGISTERED)
  val llid = Reg(UInt(15 bits)) init (Epon.broadcastLlid)
  val syncTime = Reg(UInt(16 bits)) init (0)
  val discoverySync = Reg(UInt(16 bits)) init (0)
  val needAck, needDeregister, reported, requested = Reg(Bool()) init (False)
  io.registered := state === REGISTERED
  io.llid := llid

  val time = Reg(UInt(32 bits)) init (0)
  val half = RegInit(False)
  half := !half
  when(half)(time := time + 1)
  io.time := time

  // downstream: our frames, MPCPDU fields by position, acted on at a good end
  val rx = io.rx.payload.fragment
  val unicast = !rx.mode && rx.llid === llid && state =/= UNREGISTERED && state =/= REQUESTED
  io.accept := unicast || rx.mode && (rx.llid =/= llid || llid === Epon.broadcastLlid)
  val position = Reg(UInt(11 bits)) init (0)
  val header = Vec(Reg(Bits(8 bits)), 20)
  val fields = Vec(Reg(Bits(8 bits)), fieldBytes)
  when(io.rx.valid) {
    position := io.rx.last ? U(0) | position + (position =/= 2047).asUInt
    for (k <- 0 until 20) when(position === k)(header(k) := rx.data)
    for (k <- 0 until fieldBytes) when(position === 20 + k)(fields(k) := rx.data)
  }
  def be(bytes: scala.collection.Seq[Bits]): UInt = bytes.reduce(_ ## _).asUInt
  val da = be(header.take(6))
  val opcode = header(15).asUInt
  val stamp = be(header.slice(16, 20))
  val isMpcp = header(12) === 0x88 && header(13) === 0x08 && header(14) === 0 && (da === multicast || da === io.config.mac.asUInt)
  val ended = io.rx.valid && io.rx.last && io.rxGood && io.accept && isMpcp && position >= 63
  val now = stamp + ((position + 2) >> 1).resized // the timestamp is of the frame's first byte
  val offset = now - time
  io.drift := ended && state === REGISTERED && offset > 12 && offset < U(32 bits, default -> True) - 11
  when(ended) { time := now; half := False }

  // grants
  case class Grant() extends Bundle {
    val start = UInt(32 bits)
    val length = UInt(16 bits)
    val force, discovery = Bool()
  }
  val grants = StreamFifo(Grant(), 8)
  grants.io.flush := False
  val gateCount = Reg(UInt(3 bits)) init (0) // grants of the last GATE left to push
  val gateIndex = Reg(UInt(2 bits))
  val gateDiscovery = Reg(Bool())
  val gateForce = Reg(Bits(4 bits))
  val gateFields = Reg(Bits(8 * fieldBytes bits))
  def gateField(k: UInt, at: Int, bytes: Int): UInt = {
    val all = Vec((0 until 4).map(g => be((0 until bytes).map(b => gateFields(8 * (fieldBytes - 1 - (1 + 6 * g + at + b)), 8 bits)))))
    all(k)
  }
  grants.io.push.valid := gateCount =/= 0
  grants.io.push.start := gateField(gateIndex, 0, 4)
  grants.io.push.length := gateField(gateIndex, 4, 2).resized
  grants.io.push.force := gateForce(gateIndex)
  grants.io.push.discovery := gateDiscovery
  io.grantLost := grants.io.push.valid && !grants.io.push.ready
  when(gateCount =/= 0) { gateCount := gateCount - 1; gateIndex := gateIndex + 1 }

  val sinceGate = Reg(UInt(26 bits)) init (0) // TQ
  when(half)(sinceGate := sinceGate + 1)
  when(state =/= REGISTERED)(sinceGate := 0)
  val flags = fields(0)
  val toDeregister = CombInit(False)
  when(ended && opcode === gate) {
    val count = flags(2 downto 0).asUInt
    val discovery = flags(3)
    val valid = count =/= 0 && count <= 4
    when(discovery && rx.mode && (state === UNREGISTERED || state === REQUESTED) && valid && io.config.enable) {
      gateCount := 1
      discoverySync := be(Seq(fields(7), fields(8)))
    }
    when(!discovery && unicast && valid) {
      gateCount := count
      sinceGate := 0
    }
    gateIndex := 0
    gateDiscovery := discovery
    gateForce := flags(7 downto 4)
    gateFields := fields.reverse.asBits
  }
  when(ended && opcode === register && da === io.config.mac.asUInt) {
    val code = fields(2).asUInt
    when(state === REQUESTED && code === regAck) {
      llid := be(Seq(fields(0), fields(1))).resized
      syncTime := be(Seq(fields(3), fields(4)))
      state := ACKING
      needAck := True
    }
    when(state === REQUESTED && code === regNack)(state := UNREGISTERED)
    when((state === ACKING || state === REGISTERED) && (code === regDeregisterOnu || code === regReregister))(toDeregister := True)
  }
  io.timedOut := state === REGISTERED && (sinceGate >> 10) >= io.config.timeout && io.config.timeout =/= 0
  when(io.timedOut)(toDeregister := True)

  // bursts
  object Burst extends SpinalEnum { val IDLE, WAIT, SYNC, SEND, FRAME, DATA, HOLD = newElement() }
  val burst = RegInit(Burst.IDLE)
  val grant = Reg(Grant())
  val startAt, dataAt, endAt = Reg(UInt(32 bits))
  def reached(t: UInt): Bool = !(time - t).msb
  val lfsr = Reg(Bits(16 bits)) init (0xACE1)
  lfsr := (lfsr(0) ## lfsr(15 downto 1)) ^ (lfsr(0) ? B(0xB400, 16 bits) | B(0, 16 bits))
  val head = grants.io.pop
  head.ready := False
  io.late := False
  io.burst := burst === Burst.SYNC || burst === Burst.SEND || burst === Burst.FRAME || burst === Burst.DATA ||
    burst === Burst.HOLD
  // the frame being sent: an MPCPDU from the generator, or a data frame
  object Pdu extends SpinalEnum { val REQ_REGISTER, REQ_DEREGISTER, ACK, REPORT = newElement() }
  val pdu = Reg(Pdu())
  val pduIndex = Reg(UInt(6 bits))
  val stampOut = Reg(UInt(32 bits))
  val reportValue = Reg(UInt(16 bits))
  def left: UInt = endAt - time
  def fits(tq: UInt): Bool = !left.msb && left >= tq
  val sync = (grant.discovery ? discoverySync | syncTime)
  val pduCost = U(cost(pduBytes), 8 bits)
  switch(burst) {
    is(Burst.IDLE) {
      when(head.valid) {
        head.ready := True
        grant := head.payload
        val window = (head.length.resize(17) - io.config.laserOn - io.config.laserOff - discoverySync - pduCost).asSInt
        val offset = (window.msb ? U(0, 17 bits) | window.asUInt) * lfsr.asUInt >> 16
        val start = head.start + (head.discovery ? offset.resize(32) | U(0, 32 bits))
        startAt := start
        dataAt := start + io.config.laserOn + (head.discovery ? discoverySync | syncTime)
        endAt := head.discovery ? (start + io.config.laserOn + discoverySync + pduCost) |
          (head.start + head.length - io.config.laserOff)
        reported := False
        requested := False
        when(reached(head.start)) { io.late := True } otherwise (burst := Burst.WAIT)
      }
    }
    is(Burst.WAIT)(when(reached(startAt))(burst := Burst.SYNC))
    is(Burst.SYNC)(when(reached(dataAt))(burst := Burst.SEND))
    is(Burst.SEND) { // the next frame of the burst
      burst := Burst.HOLD
      pduIndex := 0
      when(fits(pduCost)) {
        when(grant.discovery && !requested && state =/= REGISTERED && state =/= ACKING) {
          pdu := Pdu.REQ_REGISTER; requested := True; burst := Burst.FRAME
        } elsewhen (!grant.discovery && needAck) {
          pdu := Pdu.ACK; needAck := False; burst := Burst.FRAME
        } elsewhen (!grant.discovery && needDeregister) {
          pdu := Pdu.REQ_DEREGISTER; needDeregister := False; burst := Burst.FRAME
        } elsewhen (state === REGISTERED && !grant.discovery && !reported && (grant.force || io.config.alwaysReport)) {
          pdu := Pdu.REPORT; reported := True; reportValue := io.report; burst := Burst.FRAME
        }
      }
      val dataCost = ((io.data.length.resize(12) + U(8 + 4 + Epon.ipg + 3, 12 bits)) >> 1).resize(32)
      when(!grant.discovery && state === REGISTERED && io.data.valid && fits(dataCost) && !needAck && !needDeregister &&
        (reported || !(grant.force || io.config.alwaysReport)))(burst := Burst.DATA)
    }
    is(Burst.FRAME)(when(io.tx.fire && io.tx.last)(burst := Burst.SEND))
    is(Burst.DATA)(when(io.tx.fire && io.tx.last)(burst := Burst.SEND))
    is(Burst.HOLD)(when(reached(endAt))(burst := Burst.IDLE))
  }
  when(burst === Burst.FRAME && io.tx.fire) {
    pduIndex := pduIndex + 1
    when(pduIndex === 0)(stampOut := time)
    when(pdu === Pdu.REQ_REGISTER && io.tx.last)(state := REQUESTED)
    when(pdu === Pdu.ACK && io.tx.last)(state := REGISTERED)
  }
  when(!io.config.enable) { when(state === REGISTERED)(needDeregister := True) otherwise (toDeregister := True) }

  // the MPCPDU bytes
  def bytesOf(v: UInt, n: Int): Seq[Bits] = (n - 1 to 0 by -1).map(k => v.asBits(8 * k + 7 downto 8 * k))
  val opcodeOut = pdu.mux(Pdu.REQ_REGISTER -> U(registerReq, 8 bits), Pdu.REQ_DEREGISTER -> U(registerReq, 8 bits),
    Pdu.ACK -> U(registerAck, 8 bits), Pdu.REPORT -> U(report, 8 bits))
  val fieldsOut = Vec(Bits(8 bits), 5)
  fieldsOut.foreach(_ := 0)
  switch(pdu) {
    is(Pdu.REQ_REGISTER) { fieldsOut(0) := regRegister; fieldsOut(1) := 8 }
    is(Pdu.REQ_DEREGISTER) { fieldsOut(0) := regDeregister; fieldsOut(1) := 8 }
    is(Pdu.ACK) {
      fieldsOut(0) := 1
      (bytesOf(llid.resize(16), 2) ++ bytesOf(syncTime, 2)).zipWithIndex.foreach { case (b, k) => fieldsOut(1 + k) := b }
    }
    is(Pdu.REPORT) {
      fieldsOut(0) := 1; fieldsOut(1) := 1
      bytesOf(reportValue, 2).zipWithIndex.foreach { case (b, k) => fieldsOut(2 + k) := b }
    }
  }
  val stampNow = (pduIndex === 0) ? time | stampOut
  val pduBytesOut = Vec(bytesOf(U(multicast, 48 bits), 6) ++ io.config.mac.subdivideIn(8 bits).reverse ++
    Seq(B(0x88, 8 bits), B(0x08, 8 bits), B(0, 8 bits), opcodeOut.asBits) ++ bytesOf(stampNow, 4) ++ fieldsOut ++
    Seq.fill(pduBytes - 25)(B(0, 8 bits)))
  val sendingData = burst === Burst.DATA
  io.tx.valid := burst === Burst.FRAME || sendingData && io.data.valid
  io.tx.last := sendingData ? io.data.last | pduIndex === pduBytes - 1
  io.tx.fragment.data := sendingData ? io.data.fragment.data | pduBytesOut(pduIndex)
  io.tx.fragment.mode := False
  io.tx.fragment.llid := llid
  io.data.ready := sendingData && io.tx.ready

  when(toDeregister) {
    state := UNREGISTERED
    llid := Epon.broadcastLlid
    needAck := False
    needDeregister := False
    grants.io.flush := True
    gateCount := 0
  }
  when(burst === Burst.FRAME && pdu === Pdu.REQ_DEREGISTER && io.tx.fire && io.tx.last)(toDeregister := True)
}
