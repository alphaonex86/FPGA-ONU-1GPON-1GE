// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.epon

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif._

/** CPU registers of the EPON ONU MAC (APB3, MAC clock; the SoC crosses to it). EVENTS and ERRORS bits are sticky,
  * cleared by writing 1. */
case class EponOnuRegs() extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(Apb3Config(addressWidth = 8, dataWidth = 32)))
    val config = out(OnuMpcpConfig())
    val synced, registered = in Bool ()
    val llid = in UInt (15 bits)
    val time = in UInt (32 bits)
    val errors = in Bits (5 bits) // pulses: drift, late, grantLost, timedOut, underrun
    val counts = in Vec (UInt(32 bits), 3) // downstream frames good, bad, bad preambles
    val irq = out Bool ()
  }
  val busif = BusInterface(io.apb, SizeMapping(0, 256))

  val CTRL = busif.newReg("Control")
  io.config.enable := CTRL.field(Bool(), AccessType.RW, 0, "registration on (off: deregistered, laser dark)")(SymbolName("enable"))
  io.config.alwaysReport := CTRL.field(Bool(), AccessType.RW, 0, "a REPORT in every grant, not only when forced")(SymbolName("alwaysReport"))
  val MAC_HI = busif.newReg("Our MAC address, bits 47-32")
  val MAC_LO = busif.newReg("Our MAC address, bits 31-0")
  io.config.mac := MAC_HI.field(Bits(16 bits), AccessType.RW, 0, "bits")(SymbolName("mac")) ##
    MAC_LO.field(Bits(32 bits), AccessType.RW, 0, "bits")(SymbolName("mac"))
  val LASER = busif.newReg("Laser timing of our optics")
  io.config.laserOn := LASER.field(UInt(8 bits), AccessType.RW, 0, "laser on, TQ")(SymbolName("on"))
  io.config.laserOff := LASER.field(UInt(8 bits), AccessType.RW, 0, "laser off, TQ")(SymbolName("off"))
  val TIMEOUT = busif.newReg("Deregistered after this many 1024 TQ (16.4 us) without a GATE, 0 never (50 ms: 3052)")
  io.config.timeout := TIMEOUT.field(UInt(16 bits), AccessType.RW, 0, "1024 TQ")(SymbolName("units"))

  val STATE = busif.newReg("Registration")
  STATE.field(Bool(), AccessType.RO, 0, "synced: downstream code groups aligned")(SymbolName("synced")) := io.synced
  STATE.field(Bool(), AccessType.RO, 0, "registered")(SymbolName("registered")) := io.registered
  STATE.reserved(14 bits)
  STATE.field(UInt(15 bits), AccessType.RO, 0, "our LLID, 0x7FFF unregistered")(SymbolName("llid")) := io.llid
  val TIME = busif.newReg("Local MPCP time, TQ")
  TIME.field(UInt(32 bits), AccessType.RO, 0, "TQ")(SymbolName("tq")) := io.time

  def sticky(hit: Bool, clear: Bool): Bool = {
    val bit = RegInit(False)
    when(clear)(bit := False)
    when(hit)(bit := True)
    bit
  }
  val EVENTS = busif.newReg("Sticky events, write 1 to clear")
  val clearEvents = EVENTS.hitDoWrite ? busif.writeData | B(0, 32 bits)
  val changed = sticky(io.registered =/= RegNext(io.registered).init(False), clearEvents(0))
  EVENTS.field(Bool(), AccessType.RO, 0, "registered: STATE.registered changed")(SymbolName("registered")) := changed
  val ERRORS = busif.newReg("Sticky errors, write 1 to clear")
  val clearErrors = ERRORS.hitDoWrite ? busif.writeData | B(0, 32 bits)
  val errors = Seq(
    "drift" -> "a timestamp more than 12 TQ off our time",
    "late" -> "a grant dropped, its start had passed",
    "grantLost" -> "grant queue full",
    "timedOut" -> "no GATE within TIMEOUT: deregistered",
    "underrun" -> "a dataTx frame paused within itself: sent broken")
  val errorBits = for (((name, doc), k) <- errors.zipWithIndex) yield {
    val bit = sticky(io.errors(k), clearErrors(k))
    ERRORS.field(Bool(), AccessType.RO, 0, s"$name: $doc")(SymbolName(name)) := bit
    bit
  }

  val IRQ_ENABLE = busif.newReg("Interrupt sources")
  val irqRegistered = IRQ_ENABLE.field(Bool(), AccessType.RW, 0, "EVENTS.registered")(SymbolName("registered"))
  val irqError = IRQ_ENABLE.field(Bool(), AccessType.RW, 0, "any ERRORS bit")(SymbolName("error"))
  io.irq := RegNext(irqRegistered && changed || irqError && errorBits.reduce(_ || _)) init (False)

  val COUNTS = for ((name, i) <- Seq("RX_GOOD", "RX_BAD", "RX_BAD_PREAMBLE").zipWithIndex) yield {
    val reg = busif.newReg("Downstream frame count (OAM statistics)")(SymbolName(name))
    reg.field(UInt(32 bits), AccessType.RO, 0, Seq("good FCS", "bad FCS or code group", "preamble SLD or CRC-8 wrong: dropped")(i))(SymbolName("count")) := io.counts(i)
    reg
  }

  busif.accept(DocCHeader("epon_onu_regs", "EPON_ONU")) // epon_onu_regs.h next to the Verilog
}

/** EPON ONU MAC, one byte per clock (125 MHz, the lane's 10-bit code groups): EponMacRx, OnuMpcp, EponMacTx and their
  * registers. Downstream: our frames that are not MAC Control on dataRx (FCS kept, dataRxGood with the last byte);
  * upstream: dataTx frames (with their FCS) in our grants, `report` TQ queued behind them; `burst`: laser on. */
case class EponOnuMac() extends Component {
  val io = new Bundle {
    val rxCode = in Bits (10 bits)
    val txCode = out Bits (10 bits)
    val burst = out Bool ()
    val apb = slave(Apb3(Apb3Config(addressWidth = 8, dataWidth = 32))) // EponOnuRegs
    val dataRx = master Flow (Fragment(EponRxByte()))
    val dataRxGood = out Bool ()
    val dataTx = slave Stream (Fragment(EponUsByte()))
    val report = in UInt (16 bits)
    val irq = out Bool ()
  }
  val rx = EponMacRx()
  rx.io.raw := io.rxCode
  val mpcp = OnuMpcp()
  mpcp.io.rx << rx.io.frame
  mpcp.io.rxGood := rx.io.good
  mpcp.io.data << io.dataTx
  mpcp.io.report := io.report
  io.dataRx << mpcp.io.dataRx
  io.dataRxGood := mpcp.io.dataRxGood
  io.burst := mpcp.io.burst
  val tx = EponMacTx()
  tx.io.input << mpcp.io.tx
  io.txCode := tx.io.code

  val regs = EponOnuRegs()
  regs.io.apb <> io.apb
  io.irq := regs.io.irq
  mpcp.io.config := regs.io.config
  regs.io.synced := rx.io.synced
  regs.io.registered := mpcp.io.registered
  regs.io.llid := mpcp.io.llid
  regs.io.time := mpcp.io.time
  regs.io.errors := tx.io.underrun ## mpcp.io.timedOut ## mpcp.io.grantLost ## mpcp.io.late ## mpcp.io.drift
  val ended = rx.io.frame.valid && rx.io.frame.last
  def counter(hit: Bool): UInt = { val c = Reg(UInt(32 bits)) init (0); when(hit)(c := c + 1); c }
  regs.io.counts := Vec(counter(ended && rx.io.good), counter(ended && !rx.io.good), counter(rx.io.badPreamble))
}
