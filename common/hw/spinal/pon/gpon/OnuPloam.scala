// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.lib._
import GponCodes._

/** Set by the CPU. */
case class OnuConfig() extends Bundle {
  val enable = Bool()              // low: O1, laser dark
  val sn = Bits(64 bits)
  val password = Bits(80 bits)
  val responseBits = UInt(16 bits) // downstream frame start to upstream frame start, less the MAC's own latency
  val to1Frames = UInt(20 bits)    // SN acquisition and ranging timeout (10 s)
  val to2Frames = UInt(12 bits)    // POPUP timeout (100 ms)
  val key = Bits(128 bits)         // shadow key: generated and sent by the CPU on Request_Key
}

/** Up to n 12-bit IDs (Alloc-IDs, Port-IDs). */
case class IdSet(n: Int) extends Area {
  val ids = Vec(Reg(UInt(12 bits)), n)
  val used = Vec(RegInit(False), n)
  def hits(id: UInt): Seq[Bool] = ids.zip(used).map { case (a, u) => u && a === id }.toSeq
  def contains(id: UInt): Bool = hits(id).orR
  /** Adds `id` unless present; the result is high when the set was full. */
  def add(id: UInt): Bool = {
    val free = used.map(!_)
    val full = !contains(id) && !free.orR
    when(!contains(id) && free.orR) {
      val slot = OHToUInt(OHMasking.first(free.asBits))
      ids(slot) := id
      used(slot) := True
    }
    full
  }
  def remove(id: UInt): Unit = for ((hit, u) <- hits(id).zip(used)) when(hit)(u := False)
  def clear(): Unit = used.foreach(_ := False)
}

object ActivationState extends SpinalEnum { val O1, O2, O3, O4, O5, O6, O7 = newElement() }

/** Burst overhead before the PLOu, from Upstream_Overhead and Extended_Burst_Length. */
case class BurstOverhead() extends Bundle {
  val guard, type1, type2 = UInt(8 bits) // bits: laser off, ones, zeros; type 3 pattern bytes fill up to the delimiter
  val pattern = Bits(8 bits)
  val delimiter = Bits(24 bits)
  val bytes = UInt(8 bits)               // whole overhead, delimiter included
}

/** One allocation to send. */
case class UsJob() extends Bundle {
  val alloc = Allocation()
  val bit = UInt(27 bits)   // ONU bit time of the allocation's first byte (after the PLOu)
  val sn = Bool()           // its PLOAMu is Serial_Number_ONU
  val random = UInt(8 bits) // the random delay used, 32-byte units
}

object OnuPloam {
  val ackTypes = Seq(0x07, 0x08, 0x0A, 0x0E, 0x12, 0x13) // acknowledged on receipt (G.984.3 9.2.1, stock ONUs)
  val usFrameBits = usFrameBytes * 8
}

/** ONU activation (G.984.3 clause 10.2, states O1-O7, timers TO1/TO2) and the fixed downstream PLOAM set, upstream
  * clock domain: Upstream_Overhead, Extended_Burst_Length, Assign_ONU-ID, Ranging_Time (main path), Deactivate_ONU-ID,
  * Disable_Serial_Number, POPUP, Swift_POPUP and Ranging_Adjustment (Annex D), Assign_Alloc-ID, Configure_Port-ID,
  * Encrypted_Port-ID, Key_Switching_Time and Request_Key (to the decryption), Request_Password (Password 3 times),
  * Acknowledge of `ackTypes` (and of a directed Ranging_Adjustment). Every PLOAM addressed to us but No_message also
  * goes to the CPU, which handles the rest.
  * BWmap allocations for us become jobs at their ONU bit time; ownership and delay are those of the frame start's
  * state (a PLOAM acts from the next frame on). Upstream PLOAMs: ours first, then the CPU's. */
case class OnuPloam(maxAllocIds: Int = 8, maxEncryptedPorts: Int = 16, jobDepth: Int = 32) extends Component {
  import OnuPloam._
  val io = new Bundle {
    val config = in(OnuConfig())
    val synced = in Bool ()
    val frameStart = in Bool ()
    val frameBit = in UInt (27 bits) // ONU bit time of that frame start
    val ploam = slave Flow (Checked(Bits(96 bits)))
    val alloc = slave Flow (Checked(Allocation()))
    val job = master Stream (UsJob())
    val usPloam = master Stream (Bits(96 bits))
    val cpuPloam = slave Stream (Bits(96 bits))
    val cpuRx = master Flow (Bits(96 bits))
    val state = out(ActivationState())
    val onuId = out Bits (8 bits)
    val eqd = out UInt (32 bits)
    val omciPort = out UInt (12 bits)
    val omciValid = out Bool ()
    val encryptedPorts = out Vec (UInt(12 bits), maxEncryptedPorts)
    val encryptedUsed = out Bits (maxEncryptedPorts bits)
    val keySwitch = master Flow (UInt(30 bits)) // Key_Switching_Time: superframe of the first frame with the new key
    val keyRequest = out Bool ()                // Request_Key: a scheduled switch is void
    val overhead = out(BurstOverhead())
    val snMessage = out Bits (96 bits)
    val transmit = out Bool ()       // O3-O5: bursts allowed
    val hwLost = out Bool ()         // pulses: our upstream PLOAM queue full
    val jobLost = out Bool ()        // job queue full
    val allocFull = out Bool ()      // Assign_Alloc-ID beyond maxAllocIds
    val encryptedFull = out Bool ()  // Encrypted_Port-ID beyond maxEncryptedPorts
  }
  import ActivationState._

  val state = RegInit(O1)
  val onuId = Reg(Bits(8 bits)) init (0xFF)
  val eqd = Reg(UInt(32 bits)) init (0)
  val guard, type1, type2 = Reg(UInt(8 bits)) init (0)
  val pattern = Reg(Bits(8 bits)) init (0xAA)
  val delimiter = Reg(Bits(24 bits)) init (usDelimiter)
  val preDelay = Reg(UInt(16 bits)) init (0) // 32-byte units
  val power = Reg(Bits(2 bits)) init (0)
  val eblValid = RegInit(False)
  val t3Pre, t3Ranged = Reg(UInt(8 bits)) init (0)
  val snRequested = Reg(Bool()) init (False)
  val omciPort = Reg(UInt(12 bits)) init (0)
  val omciValid = RegInit(False)
  val allocIds = IdSet(maxAllocIds)
  val encrypted = IdSet(maxEncryptedPorts)
  io.state := state
  io.onuId := onuId
  io.eqd := eqd
  io.omciPort := omciPort
  io.omciValid := omciValid
  io.encryptedPorts := encrypted.ids
  io.encryptedUsed := encrypted.used.asBits
  io.transmit := state === O3 || state === O4 || state === O5

  // timers count 125 us frames of the local clock, LOS included
  val tickCount = Reg(UInt(15 bits)) init (0)
  val tick = tickCount === usFrameBytes - 1
  tickCount := tick ? U(0, 15 bits) | tickCount + 1
  val to1 = Reg(UInt(20 bits)) init (0)
  val to1On = RegInit(False)
  val to2 = Reg(UInt(12 bits)) init (0)
  val to2On = RegInit(False)
  when(tick) { to1 := to1 + 1; to2 := to2 + 1 }
  def startTo1(): Unit = { to1 := 0; to1On := True }
  def startTo2(): Unit = { to2 := 0; to2On := True }

  val hwQueue = StreamFifo(Bits(96 bits), 8)
  hwQueue.io.flush := False
  def forget(next: ActivationState.E): Unit = { // back to O1/O2/O7: the activation is gone
    state := next
    onuId := 0xFF
    eblValid := False
    omciValid := False
    allocIds.clear()
    encrypted.clear()
    to1On := False
    to2On := False
    hwQueue.io.flush := True
  }

  val m = io.ploam.value
  def byte(k: Int): Bits = m(103 - 8 * k downto 96 - 8 * k) // message byte k, 1-based
  val id = byte(1)
  val kind = byte(2).asUInt
  val assigned = onuId =/= 0xFF
  val own = assigned && id === onuId
  val broadcast = id === 0xFF
  val snMatch = m(71 downto 8) === io.config.sn
  val received = io.ploam.valid && io.ploam.crcOk && io.synced
  val popupOnly = state === O6 && !(kind === 0x05 || kind === 0x06 || kind === 0x0C || kind === 0x16) // 10.2.2 f), Annex D
  val act = received && state =/= O1 && !popupOnly
  io.cpuRx.valid := received && state =/= O1 && (own || broadcast) && kind =/= 0x0B
  io.cpuRx.payload := m

  val newAllocId = (byte(3) ## byte(4)(7 downto 4)).asUInt
  val portId = (byte(4) ## byte(5)(7 downto 4)).asUInt
  io.allocFull := False
  io.encryptedFull := False
  io.keySwitch.valid := False
  io.keySwitch.payload := (byte(3)(5 downto 0) ## byte(4) ## byte(5) ## byte(6)).asUInt
  io.keyRequest := False

  val passwordLeft = Reg(UInt(2 bits)) init (0)
  val ack = Bool()
  ack := False
  hwQueue.io.push.valid := ack || passwordLeft =/= 0
  hwQueue.io.push.payload := ack ? (onuId ## B"8'x09" ## byte(2) ## m(95 downto 24)) | (onuId ## B"8'x02" ## io.config.password)
  when(!ack && passwordLeft =/= 0)(passwordLeft := passwordLeft - 1)
  io.hwLost := hwQueue.io.push.valid && !hwQueue.io.push.ready

  when(act) {
    switch(kind) {
      is(0x01) {
        when(broadcast && state === O2) {
          guard := byte(3).asUInt
          type1 := byte(4).asUInt
          type2 := byte(5).asUInt
          pattern := byte(6)
          delimiter := byte(7) ## byte(8) ## byte(9)
          power := byte(10)(1 downto 0)
          preDelay := byte(10)(5) ? (byte(11) ## byte(12)).asUInt | U(0, 16 bits)
          snRequested := False
          state := O3
          startTo1()
        }
      }
      is(0x14) {
        when(broadcast && state === O3 && !snRequested) {
          eblValid := True
          t3Pre := byte(3).asUInt
          t3Ranged := byte(4).asUInt
        }
      }
      is(0x03) {
        when(broadcast && state === O3 && snMatch) { onuId := byte(3); state := O4 }
      }
      is(0x04) {
        when(own && (state === O4 || state === O5) && !byte(3)(0)) {
          eqd := m(71 downto 40).asUInt
          when(state === O4) { state := O5; to1On := False }
        }
      }
      is(0x05) {
        when((own || broadcast) && (state === O4 || state === O5 || state === O6))(forget(O2))
      }
      is(0x06) {
        when(broadcast) {
          when(byte(3) === 0xFF && snMatch)(forget(O7))
          when(state === O7 && (byte(3) === 0x0F || (byte(3) === 0x00 && snMatch)))(forget(O2))
        }
      }
      is(0x0C) {
        when(state === O6) {
          when(broadcast) { state := O4; to2On := False; startTo1() }
          when(own) { state := O5; to2On := False }
        }
      }
      is(0x16) { // Swift_POPUP (Annex D): to O5 with the EqD kept
        when(broadcast && state === O6) { state := O5; to2On := False }
      }
      is(0x17) { // Ranging_Adjustment (Annex D): in O5 only, the sign in octet 3 bit 1 (bit 0 too, as stock ONUs), never below 0
        when((own || broadcast) && state === O5) {
          val delta = m(71 downto 40).asUInt
          eqd := (byte(3)(1 downto 0) =/= 0) ? ((delta > eqd) ? U(0, 32 bits) | eqd - delta) | eqd + delta
        }
      }
      is(0x0A) {
        when(own && state === O5) {
          when(byte(5) === 0x01)(io.allocFull := allocIds.add(newAllocId))
          when(byte(5) === 0xFF)(allocIds.remove(newAllocId))
        }
      }
      is(0x0E) {
        when(own) { omciValid := byte(3)(0); omciPort := portId }
      }
      is(0x08) {
        when(own && state === O5 && byte(3)(1)) {
          when(byte(3)(0))(io.encryptedFull := encrypted.add(portId)) otherwise (encrypted.remove(portId))
        }
      }
      is(0x13) {
        io.keySwitch.valid := (own || broadcast) && state === O5
      }
      is(0x0D) {
        io.keyRequest := own
      }
      is(0x09) {
        when(own && (state === O4 || state === O5))(passwordLeft := 3)
      }
    }
    ack := assigned && (own || broadcast) && (state === O4 || state === O5) && ackTypes.map(kind === _).reduce(_ || _) ||
      own && state === O5 && kind === 0x17
  }

  // synchronization, timers, CPU enable: after the messages, so they win
  when(state === O1 && io.synced)(state := O2)
  when(!io.synced && (state === O2 || state === O3 || state === O4))(forget(O1))
  when(!io.synced && state === O5) { state := O6; startTo2() }
  when(to2On && state === O6 && to2 >= io.config.to2Frames)(forget(O1))
  when(to1On && (state === O3 || state === O4) && to1 >= io.config.to1Frames)(forget(O2))
  when(!io.config.enable)(forget(O1))

  // upstream PLOAMs: ours, then the CPU's
  io.usPloam.valid := hwQueue.io.pop.valid || io.cpuPloam.valid
  io.usPloam.payload := hwQueue.io.pop.valid ? hwQueue.io.pop.payload | io.cpuPloam.payload
  hwQueue.io.pop.ready := io.usPloam.ready
  io.cpuPloam.ready := io.usPloam.ready && !hwQueue.io.pop.valid

  val tt = power.mux(0 -> B"10", 1 -> B"01", default -> B"00") // TX power level mode, coded the other way round
  io.snMessage := onuId ## B"8'x01" ## io.config.sn ## B(0, 12 bits) ## B"01" ## tt

  val headBits = guard.resize(11) + type1 + type2 + 24
  val totalBits = eblValid ? (headBits + (((state === O5) ? t3Ranged | t3Pre) << 3).resize(11)) |
    ((headBits < 96) ? U(96, 11 bits) | headBits) // G.984.2 Table I.2 overhead at 1.24416 Gb/s
  io.overhead.guard := guard
  io.overhead.type1 := type1
  io.overhead.type2 := type2
  io.overhead.pattern := pattern
  io.overhead.delimiter := delimiter
  io.overhead.bytes := RegNext((totalBits > 1024) ? U(128, 8 bits) | (totalBits >> 3).resize(8))

  // jobs: frame start reference and ownership as at the frame start
  val frameState = Reg(ActivationState())
  val frameOnuId = Reg(UInt(12 bits))
  val frameBase = Reg(UInt(27 bits))
  when(io.frameStart) {
    frameState := state
    frameOnuId := onuId.asUInt.resized
    frameBase := io.frameBit + io.config.responseBits + ((state === O5) ? eqd.resize(27) | (preDelay << 8).resize(27))
  }
  val lfsr = Reg(Bits(16 bits)) init (0xACE1)
  lfsr := (lfsr |>> 1) ^ (lfsr(0) ? B"16'xB400" | B(0, 16 bits))
  val a = io.alloc.value
  val isSn = frameState === O3 && a.allocId === OltScheduler.snAllocId && a.flags(10)
  val isRanging = frameState === O4 && a.allocId === frameOnuId
  val isOwn = frameState === O5 && (a.allocId === frameOnuId || allocIds.contains(a.allocId))
  val take = io.alloc.valid && io.alloc.crcOk && (isSn || isRanging || isOwn) && a.stop >= a.start
  when(io.alloc.valid && io.alloc.crcOk && isSn)(snRequested := True)

  val picked = RegNext(take) init (False)
  val pickedAlloc = RegNext(a)
  val pickedSn = RegNext(isSn || isRanging)
  val pickedRandom = RegNext(isSn ? ((lfsr(7 downto 0) ^ io.config.sn(7 downto 0)).asUInt * U(234, 8 bits) >> 8).resize(8) | U(0, 8 bits))
  val jobs = StreamFifo(UsJob(), jobDepth)
  jobs.io.flush := !io.transmit
  jobs.io.push.valid := picked
  jobs.io.push.alloc := pickedAlloc
  jobs.io.push.sn := pickedSn
  jobs.io.push.random := pickedRandom
  jobs.io.push.bit := frameBase + (pickedRandom << 8).resize(27) + (pickedAlloc.start << 3).resize(27)
  io.jobLost := jobs.io.push.valid && !jobs.io.push.ready
  io.job << jobs.io.pop
}
