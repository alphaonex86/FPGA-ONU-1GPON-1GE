// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif._

/** CPU registers of the OLT MAC (APB3, upstream clock domain; the SoC crosses to it). Writing the last word of a
  * PLOAM or T-CONT entry sends it; reading the last word of a received PLOAM or event pops it. A full queue drops and
  * sets its ERRORS bit; ERRORS bits are cleared by writing 1. */
case class OltRegs(maxTconts: Int = 64) extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(Apb3Config(addressWidth = 8, dataWidth = 32)))
    val config = out(OltConfig())
    val cpuPloam = master Stream (Bits(96 bits))
    val cpuUsPloam = slave Flow (Bits(96 bits))
    val event = slave Flow (OltEvent())
    val release = master Flow (UInt(8 bits))
    val tcontWrite = master Flow (TcontWrite(maxTconts))
    val scheduleError = in Bool ()
    val tableError = in Bool ()
    val clearErrors = out Bool ()
    val hecErrors = in UInt (32 bits)
    val gemCuts = in UInt (32 bits)
    val irq = out Bool ()
  }
  val busif = BusInterface(io.apb, SizeMapping(0, 256))

  val CTRL = busif.newReg("Control")
  io.config.enable := CTRL.field(Bool(), AccessType.RW, 0, "activation on: SN requests every SN_PERIOD frames")(SymbolName("enable"))
  val UO_0 = busif.newReg("Upstream_Overhead PLOAM data bytes 1-4 (byte 1 in bits 31-24)")
  val UO_1 = busif.newReg("Upstream_Overhead bytes 5-8")
  val UO_2 = busif.newReg("Upstream_Overhead bytes 9-10 (bits 15-0)")
  io.config.upstreamOverhead := UO_0.field(Bits(32 bits), AccessType.RW, 0, "bytes")(SymbolName("uo0")) ##
    UO_1.field(Bits(32 bits), AccessType.RW, 0, "bytes")(SymbolName("uo1")) ## UO_2.field(Bits(16 bits), AccessType.RW, 0, "bytes")(SymbolName("uo2"))
  val TEQD = busif.newReg("Equalization delay target, upstream bytes after the frame start")
  io.config.teqd := TEQD.field(UInt(24 bits), AccessType.RW, 0, "bytes")(SymbolName("teqd"))
  val RTD_MIN = busif.newReg("Round trip of the nearest ONU, response time included")
  io.config.rtdMin := RTD_MIN.field(UInt(24 bits), AccessType.RW, 0, "bytes")(SymbolName("rtdMin"))
  val RANGING = busif.newReg("Ranging window")
  io.config.reach := RANGING.field(UInt(16 bits), AccessType.RW, 0, "round trip spread, bytes")(SymbolName("reach"))
  io.config.requestStart := RANGING.field(UInt(16 bits), AccessType.RW, 0, "StartTime of SN and ranging requests")(SymbolName("requestStart"))
  val WINDOW = busif.newReg("Request windows")
  io.config.quietBefore := WINDOW.field(UInt(4 bits), AccessType.RW, 0, "empty BWmaps before a request")(SymbolName("quietBefore"))
  io.config.quietAfter := WINDOW.field(UInt(4 bits), AccessType.RW, 0, "empty BWmaps after a request")(SymbolName("quietAfter"))
  io.config.snPeriod := WINDOW.field(UInt(16 bits), AccessType.RW, 0, "frames between SN requests")(SymbolName("snPeriod"))
  val GRANT = busif.newReg("Default allocation of each active ONU")
  io.config.grantBytes := GRANT.field(UInt(16 bits), AccessType.RW, 0, "bytes per frame, PLOAMu included")(SymbolName("grantBytes"))
  io.config.gapBytes := GRANT.field(UInt(16 bits), AccessType.RW, 0, "between bursts: guard, preamble, delimiter, PLOu")(SymbolName("gapBytes"))
  val DBA = busif.newReg("DBA")
  io.config.tconts := DBA.field(UInt(8 bits), AccessType.RW, 0, "T-CONT entries in use, sorted by ONU-ID")(SymbolName("tconts"))
  io.config.blockBytes := DBA.field(UInt(16 bits), AccessType.RW, 48, "DBRu reporting block")(SymbolName("blockBytes"))

  val TCONT_A = busif.newReg("T-CONT entry, first word")
  val tcIndex = TCONT_A.field(UInt(8 bits), AccessType.RW, 0, "table index")(SymbolName("index"))
  val tcAllocId = TCONT_A.field(UInt(12 bits), AccessType.RW, 0, "Alloc-ID")(SymbolName("allocId"))
  val tcOnuId = TCONT_A.field(UInt(8 bits), AccessType.RW, 0, "ONU-ID")(SymbolName("onuId"))
  val tcDbru = TCONT_A.field(Bool(), AccessType.RW, 0, "request a mode 0 DBRu on each grant")(SymbolName("dbru"))
  val TCONT_B = busif.newReg("T-CONT entry, second word: writing it stores the entry (its demand restarts at 0)")
  val tcFixed = TCONT_B.field(UInt(16 bits), AccessType.RW, 0, "payload bytes every frame")(SymbolName("fixedBytes"))
  val tcMax = TCONT_B.field(UInt(16 bits), AccessType.RW, 0, "payload bytes at most per frame")(SymbolName("maxBytes"))
  io.tcontWrite.valid := RegNext(TCONT_B.hitDoWrite) init (False) // the fields hold the written value from here
  io.tcontWrite.index := tcIndex.resized
  io.tcontWrite.tcont.allocId := tcAllocId
  io.tcontWrite.tcont.onuId := tcOnuId
  io.tcontWrite.tcont.dbru := tcDbru
  io.tcontWrite.tcont.fixedBytes := tcFixed
  io.tcontWrite.tcont.maxBytes := tcMax

  val RELEASE = busif.newReg("Free an ONU-ID (deactivated ONU) on write")
  val releaseId = RELEASE.field(UInt(8 bits), AccessType.RW, 0, "ONU-ID")(SymbolName("onuId"))
  io.release.valid := RegNext(RELEASE.hitDoWrite) init (False)
  io.release.payload := releaseId

  // PLOAM to send: ONU-ID, Message-ID, 10 data bytes, first byte in bits 31-24 of PLOAM_TX_0
  val PLOAM_TX_0 = busif.newReg("Downstream PLOAM bytes 1-4")
  val PLOAM_TX_1 = busif.newReg("Downstream PLOAM bytes 5-8")
  val PLOAM_TX_2 = busif.newReg("Downstream PLOAM bytes 9-12: writing it queues the PLOAM")
  val txFifo = StreamFifo(Bits(96 bits), 8)
  txFifo.io.push.valid := RegNext(PLOAM_TX_2.hitDoWrite) init (False)
  txFifo.io.push.payload := PLOAM_TX_0.field(Bits(32 bits), AccessType.RW, 0, "bytes")(SymbolName("tx0")) ##
    PLOAM_TX_1.field(Bits(32 bits), AccessType.RW, 0, "bytes")(SymbolName("tx1")) ## PLOAM_TX_2.field(Bits(32 bits), AccessType.RW, 0, "bytes")(SymbolName("tx2"))
  io.cpuPloam << txFifo.io.pop

  def queue[T <: Data](flow: Flow[T], depth: Int): (Stream[T], Bool) = {
    val fifo = StreamFifo(flow.payloadType, depth)
    fifo.io.push.valid := flow.valid
    fifo.io.push.payload := flow.payload
    (fifo.io.pop, flow.valid && !fifo.io.push.ready)
  }
  val (rx, rxLost) = queue(io.cpuUsPloam, 16)
  val PLOAM_RX_0 = busif.newReg("Upstream PLOAM for the CPU, bytes 1-4 (valid when STATUS.rx)")
  val PLOAM_RX_1 = busif.newReg("Upstream PLOAM bytes 5-8")
  val PLOAM_RX_2 = busif.newReg("Upstream PLOAM bytes 9-12: reading it pops the PLOAM")
  PLOAM_RX_0.field(Bits(32 bits), AccessType.RO, 0, "bytes")(SymbolName("rx0")) := rx.payload(95 downto 64)
  PLOAM_RX_1.field(Bits(32 bits), AccessType.RO, 0, "bytes")(SymbolName("rx1")) := rx.payload(63 downto 32)
  PLOAM_RX_2.field(Bits(32 bits), AccessType.RO, 0, "bytes")(SymbolName("rx2")) := rx.payload(31 downto 0)
  rx.ready := PLOAM_RX_2.hitDoRead

  val (ev, evLost) = queue(io.event, 16)
  val EVENT_0 = busif.newReg("Activation event (valid when STATUS.event)")
  EVENT_0.field(UInt(2 bits), AccessType.RO, 0, "0 SN_FOUND (ONU-ID assigned), 1 RANGED, 2 RANGING_FAILED, 3 NO_FREE_ID")(SymbolName("kind")) :=
    ev.kind.asBits.asUInt
  EVENT_0.field(UInt(8 bits), AccessType.RO, 0, "ONU-ID")(SymbolName("onuId")) := ev.onuId
  val EVENT_1 = busif.newReg("Event serial number, bits 63-32")
  EVENT_1.field(Bits(32 bits), AccessType.RO, 0, "SN")(SymbolName("snHigh")) := ev.sn(63 downto 32)
  val EVENT_2 = busif.newReg("Event serial number, bits 31-0")
  EVENT_2.field(Bits(32 bits), AccessType.RO, 0, "SN")(SymbolName("snLow")) := ev.sn(31 downto 0)
  val EVENT_3 = busif.newReg("Event EqD in bits (RANGED): reading it pops the event")
  EVENT_3.field(UInt(32 bits), AccessType.RO, 0, "bits")(SymbolName("eqd")) := ev.eqd
  ev.ready := EVENT_3.hitDoRead

  val STATUS = busif.newReg("Queues")
  STATUS.field(Bool(), AccessType.RO, 0, "event: an event waits")(SymbolName("event")) := ev.valid
  STATUS.field(Bool(), AccessType.RO, 0, "rx: an upstream PLOAM waits")(SymbolName("rx")) := rx.valid
  STATUS.field(Bool(), AccessType.RO, 0, "txFull: PLOAM_TX would be dropped")(SymbolName("txFull")) := !txFifo.io.push.ready

  val ERRORS = busif.newReg("Sticky errors, write 1 to clear")
  val lost = Vec(txFifo.io.push.valid && !txFifo.io.push.ready, rxLost, evLost).map { hit =>
    val bit = RegInit(False) setWhen (hit)
    bit
  }
  ERRORS.field(Bool(), AccessType.RO, 0, "schedule: grants did not fit a frame")(SymbolName("schedule")) := io.scheduleError
  ERRORS.field(Bool(), AccessType.RO, 0, "table: T-CONT table not sorted by ONU-ID")(SymbolName("table")) := io.tableError
  ERRORS.field(Bool(), AccessType.RO, 0, "txLost: PLOAM_TX written while full")(SymbolName("txLost")) := lost(0)
  ERRORS.field(Bool(), AccessType.RO, 0, "rxLost: upstream PLOAM dropped")(SymbolName("rxLost")) := lost(1)
  ERRORS.field(Bool(), AccessType.RO, 0, "eventLost: event dropped")(SymbolName("eventLost")) := lost(2)
  val clear = ERRORS.hitDoWrite ? busif.writeData | B(0, 32 bits)
  io.clearErrors := clear(0) || clear(1)
  for (k <- 0 until 3) when(clear(2 + k))(lost(k) := False)

  val IRQ_ENABLE = busif.newReg("Interrupt sources")
  val irqEvent = IRQ_ENABLE.field(Bool(), AccessType.RW, 0, "event waits")(SymbolName("event"))
  val irqRx = IRQ_ENABLE.field(Bool(), AccessType.RW, 0, "upstream PLOAM waits")(SymbolName("rx"))
  val irqError = IRQ_ENABLE.field(Bool(), AccessType.RW, 0, "any ERRORS bit")(SymbolName("error"))
  val anyError = io.scheduleError || io.tableError || lost.reduce(_ || _)
  io.irq := RegNext(irqEvent && ev.valid || irqRx && rx.valid || irqError && anyError) init (False)

  val HEC_ERRORS = busif.newReg("Upstream GEM header errors")
  HEC_ERRORS.field(UInt(32 bits), AccessType.RO, 0, "count")(SymbolName("count")) := io.hecErrors
  val GEM_CUTS = busif.newReg("Upstream GEM fragments cut by the allocation end")
  GEM_CUTS.field(UInt(32 bits), AccessType.RO, 0, "count")(SymbolName("count")) := io.gemCuts

  busif.accept(DocCHeader("olt_regs", "OLT")) // olt_regs.h next to the Verilog
}
