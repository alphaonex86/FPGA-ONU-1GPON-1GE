// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.epon

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif._

/** CPU registers of the EPON OLT MAC (APB3, MAC clock; the SoC crosses to it). Reading EVENT_2 pops the event; a
  * full event queue drops and sets ERRORS.eventLost; ERRORS bits are cleared by writing 1. */
case class EponOltRegs(onus: Int) extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(Apb3Config(addressWidth = 8, dataWidth = 32)))
    val config = out(OltMpcpConfig())
    val deregister = master Flow (UInt(log2Up(onus) bits))
    val event = slave Flow (OltMpcpEventData(onus))
    val synced = in Bool ()
    val registered = in Bits (onus bits)
    val time = in UInt (32 bits)
    val underrun, resetLost = in Bool ()
    val counts = in Vec (UInt(32 bits), 3) // upstream frames good, bad, bad preambles
    val irq = out Bool ()
  }
  val busif = BusInterface(io.apb, SizeMapping(0, 256))
  def field(reg: RegInst, bits: Int, name: String, doc: String) = reg.field(UInt(bits bits), AccessType.RW, 0, doc)(SymbolName(name))

  val CTRL = busif.newReg("Control")
  io.config.enable := CTRL.field(Bool(), AccessType.RW, 0, "MPCP on: discovery, registration, grants")(SymbolName("enable"))
  val MAC_HI = busif.newReg("Our MAC address, bits 47-32")
  val MAC_LO = busif.newReg("Our MAC address, bits 31-0")
  io.config.mac := MAC_HI.field(Bits(16 bits), AccessType.RW, 0, "bits")(SymbolName("mac")) ##
    MAC_LO.field(Bits(32 bits), AccessType.RW, 0, "bits")(SymbolName("mac"))
  val LLID = busif.newReg("LLIDs")
  io.config.llidBase := field(LLID, 15, "base", "ONU entry k gets LLID base + k")
  val DISCOVERY = busif.newReg("Discovery windows")
  io.config.discoveryPeriod := field(DISCOVERY, 16, "period", "1024 TQ between windows, 0 none")
  io.config.discoveryLength := field(DISCOVERY, 16, "length", "window, TQ")
  val RANGE = busif.newReg("Reach")
  io.config.maxRtt := field(RANGE, 16, "maxRtt", "longest round trip, TQ (kept free after a window)")
  io.config.syncTime := field(RANGE, 16, "syncTime", "our receiver's sync time, TQ (in GATE and REGISTER)")
  val SCHEDULE = busif.newReg("Grant planning")
  io.config.horizon := field(SCHEDULE, 16, "horizon", "grants planned at most this far ahead, TQ")
  io.config.lead := field(SCHEDULE, 16, "lead", "a GATE's grant at least this far ahead, TQ")
  val GRANT = busif.newReg("Grant length: last REPORT + overhead, within min and max")
  io.config.minGrant := field(GRANT, 16, "min", "TQ")
  io.config.maxGrant := field(GRANT, 16, "max", "TQ, at least overhead + 772 (a 1522-byte frame), else it never fits")
  val OVERHEAD = busif.newReg("Burst overhead")
  io.config.overhead := field(OVERHEAD, 16, "overhead", "added to a REPORT: laser on/off, sync, the REPORT itself, TQ")
  io.config.guard := field(OVERHEAD, 8, "guard", "between bursts, TQ")
  val TIMEOUT = busif.newReg("A registered ONU without MPCPDU for this many 1024 TQ is deregistered, 0 never")
  io.config.timeout := field(TIMEOUT, 16, "units", "1024 TQ")
  val RX_RESET = busif.newReg("Burst-mode receiver reset of the OLT optics, before each burst and discovery window")
  io.config.rxResetLead := field(RX_RESET, 8, "lead", "it ends this many TQ before the burst")
  io.config.rxResetWidth := field(RX_RESET, 8, "width", "TQ, 0 off")

  val DEREGISTER = busif.newReg("Deregister an ONU entry on write")
  io.deregister.valid := RegNext(DEREGISTER.hitDoWrite) init (False)
  io.deregister.payload := DEREGISTER.field(UInt(log2Up(onus) bits), AccessType.RW, 0, "entry")(SymbolName("onu"))

  val STATE = busif.newReg("Line and entries")
  STATE.field(Bool(), AccessType.RO, 0, "synced: upstream code groups aligned (last burst)")(SymbolName("synced")) := io.synced
  STATE.reserved(15 bits)
  STATE.field(Bits(onus bits), AccessType.RO, 0, "registered entries")(SymbolName("registered")) := io.registered
  val TIME = busif.newReg("Local MPCP time, TQ")
  TIME.field(UInt(32 bits), AccessType.RO, 0, "TQ")(SymbolName("tq")) := io.time

  val events = StreamFifo(OltMpcpEventData(onus), 16)
  events.io.push.valid := io.event.valid
  events.io.push.payload := io.event.payload
  val ev = events.io.pop
  val EVENT_0 = busif.newReg("Event (valid when STATUS.event)")
  EVENT_0.field(UInt(2 bits), AccessType.RO, 0, "0 REGISTERED, 1 DEREGISTERED, 2 DRIFT (RTT moved > 8 TQ), 3 FULL (no free entry)")(SymbolName("kind")) :=
    ev.kind.asBits.asUInt
  EVENT_0.reserved(6 bits)
  EVENT_0.field(UInt(log2Up(onus) bits), AccessType.RO, 0, "entry")(SymbolName("onu")) := ev.onu
  val EVENT_1 = busif.newReg("Event MAC, bits 47-32")
  EVENT_1.field(Bits(16 bits), AccessType.RO, 0, "bits")(SymbolName("mac")) := ev.mac(47 downto 32)
  val EVENT_2 = busif.newReg("Event MAC, bits 31-0: reading it pops the event")
  EVENT_2.field(Bits(32 bits), AccessType.RO, 0, "bits")(SymbolName("mac")) := ev.mac(31 downto 0)
  ev.ready := EVENT_2.hitDoRead
  val STATUS = busif.newReg("Queues")
  STATUS.field(Bool(), AccessType.RO, 0, "event: an event waits")(SymbolName("event")) := ev.valid

  def sticky(hit: Bool, clear: Bool): Bool = {
    val bit = RegInit(False)
    when(clear)(bit := False)
    when(hit)(bit := True)
    bit
  }
  val ERRORS = busif.newReg("Sticky errors, write 1 to clear")
  val clearErrors = ERRORS.hitDoWrite ? busif.writeData | B(0, 32 bits)
  val errorBits = for (((name, hit, doc), k) <- Seq(
    ("eventLost", events.io.push.valid && !events.io.push.ready, "event dropped"),
    ("underrun", io.underrun, "a downstream data frame paused within itself: sent broken"),
    ("resetLost", io.resetLost, "more than 16 bursts planned: a receiver reset skipped")).zipWithIndex) yield {
    val bit = sticky(hit, clearErrors(k))
    ERRORS.field(Bool(), AccessType.RO, 0, s"$name: $doc")(SymbolName(name)) := bit
    bit
  }

  val IRQ_ENABLE = busif.newReg("Interrupt sources")
  val irqEvent = IRQ_ENABLE.field(Bool(), AccessType.RW, 0, "event waits")(SymbolName("event"))
  val irqError = IRQ_ENABLE.field(Bool(), AccessType.RW, 0, "any ERRORS bit")(SymbolName("error"))
  io.irq := RegNext(irqEvent && ev.valid || irqError && errorBits.reduce(_ || _)) init (False)

  val COUNTS = for ((name, i) <- Seq("RX_GOOD", "RX_BAD", "RX_BAD_PREAMBLE").zipWithIndex) yield {
    val reg = busif.newReg("Upstream frame count")(SymbolName(name))
    reg.field(UInt(32 bits), AccessType.RO, 0, Seq("good FCS", "bad FCS or code group", "preamble SLD or CRC-8 wrong: dropped")(i))(SymbolName("count")) := io.counts(i)
    reg
  }

  busif.accept(DocCHeader("epon_olt_regs", "EPON_OLT")) // epon_olt_regs.h next to the Verilog
}

/** EPON OLT MAC, one byte per clock (125 MHz, the lane's 10-bit code groups, upstream in bursts): EponMacTx, OltMpcp,
  * EponMacRx and their registers. Downstream: dataTx frames (LLID, mode, fcs with the first byte) between our
  * MPCPDUs; upstream: frames of registered ONUs that are not MAC Control on dataRx (FCS kept, dataRxGood with the
  * last byte). */
case class EponOltMac(onus: Int = 8) extends Component {
  val io = new Bundle {
    val rxCode = in Bits (10 bits)
    val txCode = out Bits (10 bits)
    val apb = slave(Apb3(Apb3Config(addressWidth = 8, dataWidth = 32))) // EponOltRegs
    val dataTx = slave Stream (Fragment(EponTxByte()))
    val dataRx = master Flow (Fragment(EponRxByte()))
    val dataRxGood = out Bool ()
    val rxReset = out Bool () // to the OLT optics' burst reset pin
    val irq = out Bool ()
  }
  val mpcp = OltMpcp(onus)
  mpcp.io.data << io.dataTx
  val tx = EponMacTx()
  tx.io.input << mpcp.io.tx
  io.txCode := tx.io.code
  val rx = EponMacRx()
  rx.io.raw := io.rxCode
  mpcp.io.rx << rx.io.frame
  mpcp.io.rxGood := rx.io.good
  io.dataRx << mpcp.io.dataRx
  io.dataRxGood := mpcp.io.dataRxGood

  val regs = EponOltRegs(onus)
  regs.io.apb <> io.apb
  io.irq := regs.io.irq
  mpcp.io.config := regs.io.config
  mpcp.io.deregister << regs.io.deregister
  regs.io.event << mpcp.io.events
  regs.io.synced := rx.io.synced
  regs.io.registered := mpcp.io.registered
  regs.io.time := mpcp.io.time
  regs.io.underrun := tx.io.underrun
  regs.io.resetLost := mpcp.io.resetLost
  io.rxReset := mpcp.io.rxReset
  val ended = rx.io.frame.valid && rx.io.frame.last
  def counter(hit: Bool): UInt = { val c = Reg(UInt(32 bits)) init (0); when(hit)(c := c + 1); c }
  regs.io.counts := Vec(counter(ended && rx.io.good), counter(ended && !rx.io.good), counter(rx.io.badPreamble))
}
