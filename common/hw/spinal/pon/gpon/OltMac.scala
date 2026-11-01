// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._

/** GPON OLT MAC: downstream framer in the default clock domain (311.04 MHz, one line byte per clock), activation
  * scheduler and upstream burst receiver in the upstream domain (usClk: 155.52 MHz, the downstream clock divided by
  * two, same source and phase). The 2:1 crossings need no FIFO: an upstream register holds for two downstream
  * clocks, a downstream pulse held two clocks is seen by exactly one upstream edge. */
case class OltMac(maxOnus: Int = 32, maxTconts: Int = 64) extends Component {
  val io = new Bundle {
    val usClk = in Bool ()
    val usReset = in Bool ()
    // downstream domain
    val dsLine = out Bits (8 bits)
    val gem = slave Stream (Fragment(GemTxByte()))
    val gemError = out Bool ()
    // upstream domain
    val usLine = in Bits (8 bits) // raw upstream bytes, any bit alignment
    val time = out UInt (24 bits) // upstream byte time of usLine
    val apb = slave(Apb3(Apb3Config(addressWidth = 8, dataWidth = 32))) // OltRegs
    val irq = out Bool ()
    val rxReset = out Bool () // to the OLT optics' burst reset pin
    val dbru = master Flow (UsDbru())
    val usGem = master Flow (Fragment(UsGemByte()))
  }
  val usDomain = ClockDomain(io.usClk, io.usReset, config = ClockDomain.current.config)
  usDomain.setSynchronousWith(ClockDomain.current)

  val framer = GtcDsFramer(maxOnus + maxTconts)
  io.dsLine := framer.io.line
  framer.io.gem << io.gem
  io.gemError := framer.io.gemError

  val us = new ClockingArea(usDomain) {
    val time = Reg(UInt(24 bits)) init (0)
    time := time + 1
    io.time := time
    val phase = Reg(Bool()) init (False) // toggles each upstream clock: tells the downstream domain its first half
    phase := !phase

    val scheduler = OltScheduler(maxOnus, maxTconts)
    val receiver = GtcUsDeframer()
    val regs = OltRegs(maxTconts)
    regs.io.apb <> io.apb
    io.irq := regs.io.irq
    scheduler.io.config := regs.io.config
    scheduler.io.time := time
    scheduler.io.cpuPloam << regs.io.cpuPloam
    regs.io.cpuUsPloam << scheduler.io.cpuUsPloam
    regs.io.event << scheduler.io.event
    scheduler.io.release << regs.io.release
    scheduler.io.tcontWrite << regs.io.tcontWrite
    regs.io.scheduleError := scheduler.io.scheduleError
    regs.io.tableError := scheduler.io.tableError
    scheduler.io.clearErrors := regs.io.clearErrors
    receiver.io.grant << scheduler.io.grant
    receiver.io.line := io.usLine
    receiver.io.time := time
    receiver.io.delimiter := B(GponCodes.usDelimiter, 24 bits)
    receiver.io.delimiterErrors := 1
    receiver.io.resetLead := regs.io.config.rxResetLead
    receiver.io.resetBytes := regs.io.config.rxResetBytes
    io.rxReset := receiver.io.rxReset
    scheduler.io.burst := receiver.io.burst
    scheduler.io.usPloam := receiver.io.ploam
    scheduler.io.dbru := receiver.io.dbru
    val bwmapWrite = scheduler.io.bwmapWrite.stage() // registered before the crossing
    val bwmapCommit = scheduler.io.bwmapCommit.stage()
    io.dbru << receiver.io.dbru
    io.usGem << receiver.io.gem
    regs.io.hecErrors := receiver.io.hecErrors
    regs.io.gemCuts := receiver.io.gemCuts
  }

  // upstream -> downstream: a Flow is taken once, in the first downstream clock after the upstream edge
  val phaseSeen = RegNext(us.phase) init (False)
  val firstHalf = phaseSeen =/= RegNext(phaseSeen).init(False)
  framer.io.bwmapWrite.valid := us.bwmapWrite.valid && firstHalf
  framer.io.bwmapWrite.payload := us.bwmapWrite.payload
  framer.io.bwmapCommit.valid := us.bwmapCommit.valid && firstHalf
  framer.io.bwmapCommit.payload := us.bwmapCommit.payload
  framer.io.ploam.valid := us.scheduler.io.ploam.valid
  framer.io.ploam.payload := us.scheduler.io.ploam.payload
  // downstream -> upstream: one-clock pulses held for two clocks
  def held(pulse: Bool): Bool = pulse || RegNext(pulse).init(False)
  us.scheduler.io.ploam.ready := held(framer.io.ploam.ready)
  us.scheduler.io.frameStart := held(framer.io.frameStart)
}
