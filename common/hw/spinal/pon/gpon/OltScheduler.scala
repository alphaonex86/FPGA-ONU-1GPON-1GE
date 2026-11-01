// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.lib._
import GponCodes._

/** Set by the CPU; times in upstream bytes (155.52 per us). */
case class OltConfig() extends Bundle {
  val enable = Bool()
  val upstreamOverhead = Bits(80 bits) // data of the broadcast Upstream_Overhead PLOAM (pre-assigned delay included)
  val teqd = UInt(24 bits)         // ranged bursts arrive teqd after their frame start: >= rtdMin + reach
  val rtdMin = UInt(24 bits)       // round trip of the nearest ONU, its response time included
  val reach = UInt(16 bits)        // round trip spread: differential reach + response time spread
  val requestStart = UInt(16 bits) // StartTime of the SN and ranging requests
  val quietBefore = UInt(4 bits)   // empty BWmaps before a request frame, so no ranged burst lands in its window
  val quietAfter = UInt(4 bits)    // and after it
  val snPeriod = UInt(16 bits)     // frames between SN requests
  val grantBytes = UInt(16 bits)   // per active ONU and frame, its PLOAMu included
  val gapBytes = UInt(16 bits)     // between bursts: guard, preamble, delimiter, PLOu
}

object OltEventKind extends SpinalEnum { val SN_FOUND, RANGED, RANGING_FAILED, NO_FREE_ID = newElement() }

case class OltEvent() extends Bundle {
  val kind = OltEventKind()
  val onuId = UInt(8 bits)
  val sn = Bits(64 bits)
  val eqd = UInt(32 bits) // bits, RANGED only
}

object OltScheduler {
  def ploamU = B"12'x400"          // allocation flags: PLOAMu
  val snAllocId = 254              // ONU activation Alloc-ID
  val randomHalf = 3733            // half the 0-48 us random delay of an SN response
  val margin = 8                   // delimiter search margin around a ranged arrival
  val rangingTries = 3
}

/** OLT activation and static BWmap (G.984.3 clauses 10.4, 10.5), upstream clock domain.
  * One ONU at a time: SN request (Alloc-ID 254) -> Assign_ONU-ID -> ranging request -> Ranging_Time (EqD) -> grants;
  * after 3 failed ranging tries, Deactivate_ONU-ID. Only the first SN response of a window is read, so an ONU that
  * keeps failing starves farther ones: the CPU silences it (Disable_Serial_Number) on RANGING_FAILED.
  * Request frames carry only the request, between quietBefore/quietAfter empty frames. Each active ONU gets
  * grantBytes per frame on its default Alloc-ID (= ONU-ID) with PLOAMu. At frameStart the next frame is built.
  * PLOAMu from bursts outside activation go to the CPU, No_message excepted. */
case class OltScheduler(maxOnus: Int = 32) extends Component {
  import OltScheduler._
  val io = new Bundle {
    val config = in(OltConfig())
    val time = in UInt (24 bits)    // same time base as the upstream receiver
    val frameStart = in Bool ()     // a downstream frame starts now
    val bwmapWrite = master Flow (BwmapWrite(maxOnus))
    val bwmapCommit = master Flow (UInt(log2Up(maxOnus + 1) bits))
    val grant = master Stream (UsGrant())
    val ploam = master Stream (Bits(96 bits))  // to the framer: ours first, then the CPU's
    val cpuPloam = slave Stream (Bits(96 bits))
    val burst = slave Flow (UsBurst())
    val usPloam = slave Flow (Checked(Bits(96 bits)))
    val cpuUsPloam = master Flow (Bits(96 bits))
    val event = master Flow (OltEvent())
    val release = slave Flow (UInt(8 bits)) // the CPU frees an ONU-ID (deactivated ONU)
    val scheduleError = out Bool ()          // sticky: grants did not fit a frame, or a build overran it
  }
  val idBits = log2Up(maxOnus)

  object OnuState extends SpinalEnum { val FREE, ASSIGNED, ACTIVE = newElement() }
  val onuState = Vec(RegInit(OnuState.FREE), maxOnus)
  val snTable = Mem(Bits(64 bits), maxOnus)
  when(io.release.valid && io.release.payload < maxOnus)(onuState(io.release.payload.resize(idBits)) := OnuState.FREE)

  // our PLOAM goes before the CPU's; frames since it left
  val ploamValid = RegInit(False)
  val ploamData = Reg(Bits(96 bits))
  io.ploam.valid := ploamValid || io.cpuPloam.valid
  io.ploam.payload := ploamValid ? ploamData | io.cpuPloam.payload
  io.cpuPloam.ready := io.ploam.ready && !ploamValid
  val sentAge = Reg(UInt(2 bits)) init (3)
  when(io.frameStart && sentAge =/= 3)(sentAge := sentAge + 1)
  when(io.ploam.ready && ploamValid) { ploamValid := False; sentAge := 0 }
  def sendPloam(onuId: Bits, messageId: Int, data: Bits): Unit = {
    ploamValid := True
    ploamData := onuId ## B(messageId, 8 bits) ## data
  }

  val preDelay = io.config.upstreamOverhead(21) ? (io.config.upstreamOverhead(15 downto 0).asUInt << 5) | U(0, 21 bits)
  val halfReach = io.config.reach >> 1

  // request window: quietBefore empty frames, the request frame, quietAfter empty frames
  val winActive = RegInit(False)
  val winCount = Reg(UInt(5 bits))
  val reqAlloc = Reg(UInt(12 bits))
  val reqIsSn = Reg(Bool())
  val reqPushed = Reg(Bool())
  val reqTime = Reg(UInt(24 bits)) // its grant time, to know its burst report
  def openWindow(allocId: UInt, sn: Boolean): Unit = {
    winActive := True
    winCount := 0
    reqAlloc := allocId.resized
    reqIsSn := Bool(sn)
    reqPushed := False
  }

  // BWmap builder
  object Kind extends SpinalEnum { val QUIET, REQUEST, NORMAL = newElement() }
  val building = RegInit(False)
  val kind = Reg(Kind())
  val nextTime = Reg(UInt(24 bits))
  val scanId = Reg(UInt(idBits + 1 bits))
  val entries = Reg(UInt(log2Up(maxOnus + 1) bits))
  val cursor = Reg(UInt(16 bits))
  val scheduleError = RegInit(False)
  io.scheduleError := scheduleError

  when(io.frameStart) {
    when(building)(scheduleError := True)
    building := True
    nextTime := io.time + usFrameBytes
    scanId := 0
    entries := 0
    cursor := io.config.gapBytes
    kind := Kind.NORMAL
    when(winActive) {
      kind := (winCount === io.config.quietBefore) ? Kind.REQUEST | Kind.QUIET
      winCount := winCount + 1
      when(winCount === io.config.quietBefore +^ io.config.quietAfter)(winActive := False)
    }
  }

  val grants = StreamFifo(UsGrant(), 4 * maxOnus)
  io.grant << grants.io.pop
  val push = grants.io.push
  push.valid := False
  push.alloc.allocId := scanId.resize(12)
  push.alloc.flags := ploamU
  push.alloc.start := cursor
  push.alloc.stop := cursor + io.config.grantBytes - 1
  push.time := nextTime + io.config.teqd + cursor
  push.window := margin
  io.bwmapWrite.valid := False
  io.bwmapWrite.index := entries.resized
  io.bwmapWrite.alloc := push.alloc
  io.bwmapCommit.valid := False
  io.bwmapCommit.payload := entries

  when(building && !io.frameStart) {
    switch(kind) {
      is(Kind.QUIET) {
        io.bwmapCommit.valid := True
        building := False
      }
      is(Kind.REQUEST) {
        push.alloc.allocId := reqAlloc
        push.alloc.start := io.config.requestStart
        push.alloc.stop := io.config.requestStart + 12
        val spread = reqIsSn ? U(randomHalf, 16 bits) | U(0, 16 bits)
        push.time := nextTime + io.config.requestStart + preDelay + io.config.rtdMin + halfReach + spread
        push.window := halfReach + spread + margin
        push.valid := True
        when(push.ready) {
          io.bwmapWrite.valid := True
          io.bwmapCommit.valid := True
          io.bwmapCommit.payload := 1
          building := False
          reqPushed := True
          reqTime := push.time
        }
      }
      is(Kind.NORMAL) {
        val active = onuState(scanId.resize(idBits)) === OnuState.ACTIVE
        val fits = (cursor +^ io.config.grantBytes) <= usFrameBytes
        when(scanId === maxOnus) {
          io.bwmapCommit.valid := True
          building := False
        } elsewhen (!active) {
          scanId := scanId + 1
        } elsewhen (!fits) {
          scheduleError := True
          scanId := scanId + 1
        } otherwise {
          push.valid := True
          when(push.ready) {
            io.bwmapWrite.valid := True
            entries := entries + 1
            cursor := cursor + io.config.grantBytes + io.config.gapBytes
            scanId := scanId + 1
          }
        }
      }
    }
  }

  // activation, one ONU at a time
  object Step extends SpinalEnum {
    val IDLE, SN_BURST, SN_PLOAM, SEARCH, ASSIGN, RANGE, RANGE_BURST, RANGE_PLOAM, RANGED, ACTIVATE, DEACTIVATE = newElement()
  }
  val step = RegInit(Step.IDLE)
  val snAge = Reg(UInt(16 bits)) init (0)
  when(io.frameStart && snAge =/= snAge.maxValue)(snAge := snAge + 1)
  val sn = Reg(Bits(64 bits))
  val onuId = Reg(UInt(idBits bits))
  val tries = Reg(UInt(2 bits))
  val delayBits = Reg(SInt(28 bits))
  val eqd = Reg(SInt(36 bits))

  val event = Flow(OltEvent())
  event.valid := False
  event.kind := OltEventKind.SN_FOUND
  event.onuId := onuId.resized
  event.sn := sn
  event.eqd := eqd.asUInt.resized
  io.event << event

  val msg = io.usPloam.value
  val msgOnuId = msg(95 downto 88)
  val msgId = msg(87 downto 80)
  val isSnMessage = io.usPloam.crcOk && msgId === 0x01
  val consumed = step === Step.SN_PLOAM || step === Step.RANGE_PLOAM
  val reqBurst = io.burst.valid && reqPushed && io.burst.time === reqTime
  io.cpuUsPloam.valid := io.usPloam.valid && io.usPloam.crcOk && !consumed && msgId =/= 0x04
  io.cpuUsPloam.payload := msg

  // SEARCH: same SN already known (the ONU restarted, or missed its Assign_ONU-ID), else the first free ONU-ID
  val searchId = Reg(UInt(idBits + 1 bits))
  val tableSn = snTable.readSync(searchId.resize(idBits))
  val checkedId = searchId - 1 // the entry in tableSn
  val known, free = Reg(Bool())
  val knownId, freeId = Reg(UInt(idBits bits))

  def rangingFailed(): Unit = {
    tries := tries + 1
    step := Step.RANGE
    when(tries === rangingTries - 1) {
      event.valid := True
      event.kind := OltEventKind.RANGING_FAILED
      step := Step.DEACTIVATE
    }
  }

  switch(step) {
    is(Step.IDLE) {
      when(io.config.enable && snAge >= io.config.snPeriod && !winActive && !ploamValid) {
        sendPloam(B"8'xFF", 0x01, io.config.upstreamOverhead)
        openWindow(U(snAllocId), sn = true)
        snAge := 0
        step := Step.SN_BURST
      }
    }
    is(Step.SN_BURST) {
      when(reqBurst) {
        step := io.burst.found ? Step.SN_PLOAM | Step.IDLE
      }
    }
    is(Step.SN_PLOAM) {
      when(io.usPloam.valid) {
        sn := msg(79 downto 16)
        searchId := 0
        known := False
        free := False
        step := isSnMessage ? Step.SEARCH | Step.IDLE
      }
    }
    is(Step.SEARCH) {
      searchId := searchId + 1
      when(searchId =/= 0) {
        val state = onuState(checkedId.resize(idBits))
        when(!known && state =/= OnuState.FREE && tableSn === sn) { known := True; knownId := checkedId.resized }
        when(!free && state === OnuState.FREE) { free := True; freeId := checkedId.resized }
      }
      when(searchId === maxOnus)(step := Step.ASSIGN)
    }
    is(Step.ASSIGN) {
      val id = known ? knownId | freeId
      when(!known && !free) {
        event.valid := True
        event.kind := OltEventKind.NO_FREE_ID
        step := Step.IDLE
      } elsewhen (!ploamValid) {
        onuId := id
        snTable.write(id, sn)
        onuState(id) := OnuState.ASSIGNED
        event.valid := True
        event.onuId := id.resized
        sendPloam(B"8'xFF", 0x03, id.resize(8).asBits ## sn ## B(0, 8 bits))
        tries := 0
        step := Step.RANGE
      }
    }
    is(Step.RANGE) { // two frames after our last PLOAM left, so the ONU acted on it
      when(sentAge >= 2 && !ploamValid && !winActive) {
        openWindow(onuId.resized, sn = false)
        step := Step.RANGE_BURST
      }
    }
    is(Step.RANGE_BURST) {
      when(reqBurst) {
        delayBits := io.burst.delayBits
        when(io.burst.found && io.burst.onuId === onuId.resize(8).asBits)(step := Step.RANGE_PLOAM) otherwise (rangingFailed())
      }
    }
    is(Step.RANGE_PLOAM) {
      when(io.usPloam.valid) {
        // EqD = teqd - round trip; the ranging response was expected rtdMin + reach/2 after its StartTime
        val base = (io.config.teqd.intoSInt - io.config.rtdMin.intoSInt - halfReach.intoSInt).resize(33 bits)
        val e = (base << 3).resize(36 bits) - delayBits.resize(36 bits)
        eqd := e
        val ok = isSnMessage && msgOnuId === onuId.resize(8).asBits && msg(79 downto 16) === sn && !e.msb && (e >> 32) === 0
        when(ok)(step := Step.RANGED) otherwise (rangingFailed())
      }
    }
    is(Step.RANGED) {
      when(!ploamValid) {
        sendPloam(onuId.resize(8).asBits, 0x04, B(0, 8 bits) ## eqd.asBits(31 downto 0) ## B(0, 40 bits))
        event.valid := True
        event.kind := OltEventKind.RANGED
        step := Step.ACTIVATE
      }
    }
    is(Step.DEACTIVATE) { // the ONU may still hold the ONU-ID: take it back before reuse
      when(!ploamValid) {
        sendPloam(onuId.resize(8).asBits, 0x05, B(0, 80 bits))
        onuState(onuId) := OnuState.FREE
        step := Step.IDLE
      }
    }
    is(Step.ACTIVATE) {
      when(sentAge >= 2 && !ploamValid) {
        onuState(onuId) := OnuState.ACTIVE
        step := Step.IDLE
      }
    }
  }
}
