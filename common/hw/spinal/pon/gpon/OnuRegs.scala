// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif._

/** CPU registers of the ONU MAC (APB3, upstream clock domain). Writing PLOAM_TX_2 queues an upstream PLOAM, reading
  * PLOAM_RX_2 pops a downstream one. A full queue drops and sets its ERRORS bit; EVENTS and ERRORS bits are cleared by
  * writing 1. */
case class OnuRegs(defaultResponseBits: Int, dataTconts: Int) extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(Apb3Config(addressWidth = 8, dataWidth = 32)))
    val config = out(OnuConfig())
    val dataAlloc = Vec(master Flow (UInt(12 bits)), dataTconts) // the Alloc-ID of each upstream data stream
    val blockBytes = out UInt (16 bits)
    val cpuPloam = master Stream (Bits(96 bits))
    val cpuRx = slave Flow (Bits(96 bits))
    val state = in(ActivationState())
    val onuId = in Bits (8 bits)
    val eqd = in UInt (32 bits)
    val omciPort = in UInt (12 bits)
    val omciValid = in Bool ()
    val errors = in Bits (7 bits) // pulses: hwLost, jobLost, allocFull, late, gem, encryptedFull, keyLate
    val bipErrors = in UInt (32 bits)
    val fec = in Vec (UInt(32 bits), 4) // codewords, corrected bytes, corrected codewords, uncorrectable codewords
    val irq = out Bool ()
  }
  val busif = BusInterface(io.apb, SizeMapping(0, 256))

  val CTRL = busif.newReg("Control")
  io.config.enable := CTRL.field(Bool(), AccessType.RW, 0, "activation on (off: O1, laser dark)")(SymbolName("enable"))
  val SN_0 = busif.newReg("Serial number bytes 1-4 (vendor ID)")
  val SN_1 = busif.newReg("Serial number bytes 5-8")
  io.config.sn := SN_0.field(Bits(32 bits), AccessType.RW, 0, "bytes")(SymbolName("sn0")) ##
    SN_1.field(Bits(32 bits), AccessType.RW, 0, "bytes")(SymbolName("sn1"))
  val PASSWORD_0 = busif.newReg("Password bytes 1-4")
  val PASSWORD_1 = busif.newReg("Password bytes 5-8")
  val PASSWORD_2 = busif.newReg("Password bytes 9-10 (bits 15-0)")
  io.config.password := PASSWORD_0.field(Bits(32 bits), AccessType.RW, 0, "bytes")(SymbolName("pw0")) ##
    PASSWORD_1.field(Bits(32 bits), AccessType.RW, 0, "bytes")(SymbolName("pw1")) ##
    PASSWORD_2.field(Bits(16 bits), AccessType.RW, 0, "bytes")(SymbolName("pw2"))
  val RESPONSE = busif.newReg("Response time less the MAC latency, upstream bits (35 us at the line)")
  io.config.responseBits := RESPONSE.field(UInt(16 bits), AccessType.RW, defaultResponseBits, "bits")(SymbolName("bits"))
  val TIMERS = busif.newReg("Activation timers, 125 us frames")
  io.config.to1Frames := TIMERS.field(UInt(20 bits), AccessType.RW, 80000, "TO1, SN acquisition and ranging")(SymbolName("to1"))
  io.config.to2Frames := TIMERS.field(UInt(12 bits), AccessType.RW, 800, "TO2, POPUP")(SymbolName("to2"))

  val keyRegs = (0 until 4).map(i => busif.newReg(s"Shadow key bytes ${4 * i}-${4 * i + 3}: the next key, sent by the CPU on Request_Key"))
  io.config.key := Cat(keyRegs.zipWithIndex.map { case (r, i) => r.field(Bits(32 bits), AccessType.RW, 0, "bytes")(SymbolName(s"key$i")) }.reverse)

  val STATE = busif.newReg("Activation state")
  STATE.field(UInt(3 bits), AccessType.RO, 0, "state, 0 = O1 .. 6 = O7")(SymbolName("state")) := io.state.asBits.asUInt.resized
  STATE.reserved(5 bits)
  STATE.field(Bits(8 bits), AccessType.RO, 0, "ONU-ID, 255 unassigned")(SymbolName("onuId")) := io.onuId
  STATE.field(Bool(), AccessType.RO, 0, "OMCI Port-ID configured")(SymbolName("omciValid")) := io.omciValid
  STATE.reserved(3 bits)
  STATE.field(UInt(12 bits), AccessType.RO, 0, "OMCI Port-ID")(SymbolName("omciPort")) := io.omciPort
  val EQD = busif.newReg("Equalization delay, bits")
  EQD.field(UInt(32 bits), AccessType.RO, 0, "bits")(SymbolName("eqd")) := io.eqd

  val PLOAM_TX_0 = busif.newReg("Upstream PLOAM bytes 1-4")
  val PLOAM_TX_1 = busif.newReg("Upstream PLOAM bytes 5-8")
  val PLOAM_TX_2 = busif.newReg("Upstream PLOAM bytes 9-12: writing it queues the PLOAM")
  val txFifo = StreamFifo(Bits(96 bits), 8)
  txFifo.io.push.valid := RegNext(PLOAM_TX_2.hitDoWrite) init (False)
  txFifo.io.push.payload := PLOAM_TX_0.field(Bits(32 bits), AccessType.RW, 0, "bytes")(SymbolName("tx0")) ##
    PLOAM_TX_1.field(Bits(32 bits), AccessType.RW, 0, "bytes")(SymbolName("tx1")) ## PLOAM_TX_2.field(Bits(32 bits), AccessType.RW, 0, "bytes")(SymbolName("tx2"))
  io.cpuPloam << txFifo.io.pop

  val rxFifo = StreamFifo(Bits(96 bits), 16)
  rxFifo.io.push.valid := io.cpuRx.valid
  rxFifo.io.push.payload := io.cpuRx.payload
  val rx = rxFifo.io.pop
  val PLOAM_RX_0 = busif.newReg("Downstream PLOAM for the CPU, bytes 1-4 (valid when STATUS.rx)")
  val PLOAM_RX_1 = busif.newReg("Downstream PLOAM bytes 5-8")
  val PLOAM_RX_2 = busif.newReg("Downstream PLOAM bytes 9-12: reading it pops the PLOAM")
  PLOAM_RX_0.field(Bits(32 bits), AccessType.RO, 0, "bytes")(SymbolName("rx0")) := rx.payload(95 downto 64)
  PLOAM_RX_1.field(Bits(32 bits), AccessType.RO, 0, "bytes")(SymbolName("rx1")) := rx.payload(63 downto 32)
  PLOAM_RX_2.field(Bits(32 bits), AccessType.RO, 0, "bytes")(SymbolName("rx2")) := rx.payload(31 downto 0)
  rx.ready := PLOAM_RX_2.hitDoRead

  val STATUS = busif.newReg("Queues")
  STATUS.field(Bool(), AccessType.RO, 0, "rx: a downstream PLOAM waits")(SymbolName("rx")) := rx.valid
  STATUS.field(Bool(), AccessType.RO, 0, "txFull: PLOAM_TX would be dropped")(SymbolName("txFull")) := !txFifo.io.push.ready

  def sticky(hit: Bool, clear: Bool): Bool = {
    val bit = RegInit(False)
    when(clear)(bit := False)
    when(hit)(bit := True)
    bit
  }
  val EVENTS = busif.newReg("Sticky events, write 1 to clear")
  val clearEvents = EVENTS.hitDoWrite ? busif.writeData | B(0, 32 bits)
  val stateChanged = sticky(io.state =/= RegNext(io.state).init(ActivationState.O1), clearEvents(0))
  EVENTS.field(Bool(), AccessType.RO, 0, "state: the activation state changed")(SymbolName("state")) := stateChanged
  val ERRORS = busif.newReg("Sticky errors, write 1 to clear")
  val clearErrors = ERRORS.hitDoWrite ? busif.writeData | B(0, 32 bits)
  val errors = Seq(
    "txLost" -> (txFifo.io.push.valid && !txFifo.io.push.ready, "PLOAM_TX written while full"),
    "rxLost" -> (rxFifo.io.push.valid && !rxFifo.io.push.ready, "downstream PLOAM dropped"),
    "hwLost" -> (io.errors(0), "our upstream PLOAM queue full (Acknowledge, Password)"),
    "jobLost" -> (io.errors(1), "allocation queue full"),
    "allocFull" -> (io.errors(2), "Assign_Alloc-ID beyond the table"),
    "late" -> (io.errors(3), "an allocation was dropped, its burst start had passed"),
    "gem" -> (io.errors(4), "an upstream GEM stream broke a packet (gap, wrong last, zero length)"),
    "encryptedFull" -> (io.errors(5), "Encrypted_Port-ID beyond the table"),
    "keyLate" -> (io.errors(6), "a decrypted fragment dropped, its keystream late"))
  val errorBits = for (((name, (hit, doc)), k) <- errors.zipWithIndex) yield {
    val bit = sticky(hit, clearErrors(k))
    ERRORS.field(Bool(), AccessType.RO, 0, s"$name: $doc")(SymbolName(name)) := bit
    bit
  }

  val IRQ_ENABLE = busif.newReg("Interrupt sources")
  val irqRx = IRQ_ENABLE.field(Bool(), AccessType.RW, 0, "downstream PLOAM waits")(SymbolName("rx"))
  val irqState = IRQ_ENABLE.field(Bool(), AccessType.RW, 0, "EVENTS.state")(SymbolName("state"))
  val irqError = IRQ_ENABLE.field(Bool(), AccessType.RW, 0, "any ERRORS bit")(SymbolName("error"))
  io.irq := RegNext(irqRx && rx.valid || irqState && stateChanged || irqError && errorBits.reduce(_ || _)) init (False)

  val BIP_ERRORS = busif.newReg("Downstream BIP errors (bits), for REI")
  BIP_ERRORS.field(UInt(32 bits), AccessType.RO, 0, "count")(SymbolName("count")) := io.bipErrors
  val FEC = for ((name, i) <- Seq("CODEWORDS", "CORRECTED_BYTES", "CORRECTED_CODEWORDS", "UNCORRECTABLE").zipWithIndex) yield {
    val reg = busif.newReg("Downstream FEC count (OMCI FEC PM)")(SymbolName(s"FEC_$name"))
    reg.field(UInt(32 bits), AccessType.RO, 0, "count")(SymbolName("count")) := io.fec(i)
    reg
  }

  val DBRU = busif.newReg("DBRu reports: queued bytes in reporting blocks (OMCI ANI-G GEM block length)")
  io.blockBytes := DBRU.field(UInt(16 bits), AccessType.RW, 48, "reporting block, bytes")(SymbolName("blockBytes"))
  val DATA_ALLOC = for ((alloc, k) <- io.dataAlloc.zipWithIndex) yield {
    val reg = busif.newReg(s"Upstream data queue $k of the Ethernet bridge: its T-CONT")(SymbolName(s"DATA_ALLOC_$k"))
    alloc.payload := reg.field(UInt(12 bits), AccessType.RW, 0, "Alloc-ID")(SymbolName("allocId"))
    alloc.valid := reg.field(Bool(), AccessType.RW, 0, "valid, else the queue waits")(SymbolName("valid"))
    reg
  }

  busif.accept(DocCHeader("onu_regs", "ONU")) // onu_regs.h next to the Verilog
}
