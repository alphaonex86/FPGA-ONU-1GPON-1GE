// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._

object OnuMac {
  val defaultResponseBits = 43546 - 43 // 35 us at 1.24416 Gb/s less the MAC latency (OnuMacTest); GTS latency: on the board
}

/** GPON ONU MAC: downstream deframer in the default clock domain (311.04 MHz recovered clock, one line byte per
  * clock), activation and upstream burst framer in the upstream domain (usClk: the downstream clock divided by two,
  * same source). Downstream GEM fragments out on gemRx (after the AES decryption of the encrypted Port-IDs), with the
  * OMCI Port-ID; upstream gemTx packets (OMCI) in the default allocation (OMCC), each dataTx stream in the Alloc-ID the
  * CPU gives it (DATA_ALLOC_k). Upstream timing is anchored to the downstream frame start to the bit: its byte time, which half of
  * the upstream clock it came in and the PSync bit alignment, so a re-sync (POPUP) keeps the ranged EqD valid. */
case class OnuMac(maxAllocIds: Int = 8, dataTconts: Int = 4) extends Component {
  val io = new Bundle {
    val usClk = in Bool ()
    val usReset = in Bool ()
    // downstream domain
    val dsLine = in Bits (8 bits) // raw downstream bytes, any bit alignment
    // upstream domain
    val usLine = out Bits (8 bits)
    val usBen = out Bits (8 bits) // burst enable per line bit (laser on)
    val apb = slave(Apb3(Apb3Config(addressWidth = 8, dataWidth = 32))) // OnuRegs
    val gemTx = slave Stream (Fragment(GemTxByte()))
    val dataTx = Vec(slave Stream (Fragment(GemTxByte())), dataTconts)
    val gemTxBacklog = in UInt (16 bits) // bytes queued behind each, for the DBRu
    val dataTxBacklog = in Vec (UInt(16 bits), dataTconts)
    // downstream domain
    val gemRx = master Flow (Fragment(UsGemByte())) // allocId 0
    val gemRxCut = out Bool ()
    val omciPort = master Flow (UInt(12 bits))
    val irq = out Bool ()
  }
  val usDomain = ClockDomain(io.usClk, io.usReset, config = ClockDomain.current.config)
  usDomain.setSynchronousWith(ClockDomain.current)

  val deframer = GtcDsDeframer()
  deframer.io.line := io.dsLine

  val us = new ClockingArea(usDomain) {
    val time = Reg(UInt(24 bits)) init (0)
    time := time + 1
    val phase = Reg(Bool()) init (False) // toggles each upstream clock: tells the downstream domain its halves
    phase := !phase
  }

  // downstream -> upstream: a pulse and its payload held two clocks are seen by exactly one upstream edge
  val phaseSeen = RegNext(us.phase) init (False)
  val secondHalf = phaseSeen =/= RegNext(phaseSeen).init(False)
  def held[T <: Data](valid: Bool, payload: T): (Bool, T) =
    (valid || RegNext(valid).init(False), valid ? payload | RegNextWhen(payload, valid))
  // the frame start's place within the upstream byte: half-clock (4 bits) and PSync alignment (2 downstream bits each)
  val (frameStart, frameOffset) = held(deframer.io.frameStart, secondHalf.asUInt @@ ~deframer.io.alignment(2 downto 1))
  val (dsPloamValid, dsPloam) = held(deframer.io.ploam.valid, deframer.io.ploam.payload)
  val (dsAllocValid, dsAlloc) = held(deframer.io.alloc.valid, deframer.io.alloc.payload)
  val decrypt = GemDecrypt()
  decrypt.io.header << deframer.io.gemHeader
  decrypt.io.input << deframer.io.gem
  decrypt.io.inputCut := deframer.io.gemCut
  val (keyLate, _) = held(decrypt.io.late, False)

  val upstream = new ClockingArea(usDomain) {
    val ploam = OnuPloam(maxAllocIds)
    val framer = GtcUsFramer(1 + dataTconts)
    val regs = OnuRegs(OnuMac.defaultResponseBits, dataTconts)
    regs.io.apb <> io.apb
    io.irq := regs.io.irq
    ploam.io.config := regs.io.config
    // registered first: a downstream launch may be sampled one downstream clock later
    ploam.io.synced := RegNext(deframer.io.synced) init (False)
    ploam.io.frameStart := RegNext(frameStart) init (False)
    ploam.io.frameBit := us.time @@ RegNext(frameOffset)
    ploam.io.ploam.valid := RegNext(dsPloamValid) init (False)
    ploam.io.ploam.payload := RegNext(dsPloam)
    ploam.io.alloc.valid := RegNext(dsAllocValid) init (False)
    ploam.io.alloc.payload := RegNext(dsAlloc)
    ploam.io.cpuPloam << regs.io.cpuPloam
    regs.io.cpuRx << ploam.io.cpuRx
    regs.io.state := ploam.io.state
    regs.io.onuId := ploam.io.onuId
    regs.io.eqd := ploam.io.eqd
    regs.io.omciPort := ploam.io.omciPort
    regs.io.omciValid := ploam.io.omciValid
    regs.io.errors := RegNext(keyLate) ## ploam.io.encryptedFull ## framer.io.gemError ## framer.io.late ## ploam.io.allocFull ## ploam.io.jobLost ## ploam.io.hwLost
    regs.io.bipErrors := RegNext(deframer.io.bipErrors)
    regs.io.fec := RegNext(Vec(deframer.io.fecCodewords, deframer.io.fecCorrectedBytes, deframer.io.fecCorrectedCodewords,
      deframer.io.fecUncorrectable))

    framer.io.job << ploam.io.job
    framer.io.time := us.time
    framer.io.enable := ploam.io.transmit
    framer.io.onuId := ploam.io.onuId
    framer.io.overhead := ploam.io.overhead
    framer.io.snMessage := ploam.io.snMessage
    framer.io.ploam << ploam.io.usPloam
    framer.io.gem(0) << io.gemTx
    framer.io.gemAllocId(0).push(ploam.io.onuId.asUInt.resized)
    for (k <- 0 until dataTconts) {
      framer.io.gem(1 + k) << io.dataTx(k)
      framer.io.gemAllocId(1 + k) << regs.io.dataAlloc(k)
    }
    val reporter = DbruReporter(1 + dataTconts)
    reporter.io.bytes := Vec(io.gemTxBacklog +: io.dataTxBacklog)
    reporter.io.blockBytes := regs.io.blockBytes
    framer.io.dbru := reporter.io.codes
    io.usLine := framer.io.line
    io.usBen := framer.io.ben
  }

  // from the synchronous upstream domain: quasi-static or pulses of one upstream clock (seen twice, idempotent)
  decrypt.io.ports := upstream.ploam.io.encryptedPorts
  decrypt.io.portsUsed := upstream.ploam.io.encryptedUsed
  decrypt.io.shadowKey := upstream.regs.io.config.key
  decrypt.io.keySwitch << upstream.ploam.io.keySwitch
  decrypt.io.keyRequest := upstream.ploam.io.keyRequest

  io.omciPort.payload := RegNext(upstream.ploam.io.omciPort)
  io.omciPort.valid := RegNext(upstream.ploam.io.omciValid) init (False)
  val gem = decrypt.io.output
  io.gemRx.valid := gem.valid
  io.gemRx.last := gem.last
  io.gemRx.allocId := 0
  io.gemRx.gem := gem.fragment
  io.gemRxCut := decrypt.io.outputCut
}
