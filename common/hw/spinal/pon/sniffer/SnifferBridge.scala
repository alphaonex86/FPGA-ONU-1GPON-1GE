// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.lib._

/** MSB-first gearbox: `inBits` per valid clock in, `outBits` out once that many are buffered. */
case class Gearbox(inBits: Int, outBits: Int) extends Component {
  val io = new Bundle {
    val input = slave Flow (Bits(inBits bits))
    val output = master Flow (Bits(outBits bits))
  }
  val width = inBits + outBits
  val buffer = Reg(Bits(width bits)) init (0)
  val fill = Reg(UInt(log2Up(width + 1) bits)) init (0)
  val merged = buffer | ((io.input.payload ## B(0, outBits bits)) |>> fill)
  val total = fill + inBits
  io.output.valid := io.input.valid && total >= outBits
  io.output.payload := merged(width - 1 downto inBits)
  when(io.input.valid) {
    buffer := io.output.valid ? (merged |<< outBits) | merged
    fill := io.output.valid ? (total - outBits) | total
  }
}

/** Upstream 2x oversampling: 20 samples per clock (first in bit 19), 10 bits out, one sample of each pair. A sample
  * on a bit edge reads either bit, so isolated runs of 1 or 3 samples appear centred on the other, good, parity;
  * with the edges between samples every run is 2 long and either parity is good. Votes restart after `quietWords`
  * zero words (the squelch between bursts); a tie keeps the last choice. */
case class PhasePick(quietWords: Int = 4) extends Component {
  val io = new Bundle {
    val samples = in Bits (20 bits)
    val bits = out Bits (10 bits)
    val odd = out Bool () // the second sample of each pair is kept
  }
  // window: the last 4 samples of the previous word then this one; centres 2..21 cover each sample once
  val previous = RegNext(io.samples) init (0)
  val window = previous(3 downto 0) ## io.samples
  def s(p: Int): Bool = window(23 - p)
  def vote(p: Int): Bool = (s(p - 1) =/= s(p) && s(p + 1) =/= s(p)) ||
    (s(p - 1) === s(p) && s(p + 1) === s(p) && s(p - 2) =/= s(p) && s(p + 2) =/= s(p))
  val oddVotes = CountOne(RegNext(Vec((3 to 21 by 2).map(vote))).asBits) // two stages for timing
  val evenVotes = CountOne(RegNext(Vec((2 to 20 by 2).map(vote))).asBits)
  val votes = RegNext(oddVotes.intoSInt.resize(8 bits) - evenVotes.intoSInt.resize(8 bits)) init (0)
  val score = Reg(SInt(8 bits)) init (0)
  val quiet = Reg(UInt(log2Up(quietWords + 1) bits)) init (0)
  quiet := (io.samples === 0) ? (quiet + (quiet =/= quietWords).asUInt) | U(0)
  val next = score + votes
  when(quiet === quietWords)(score := 0) otherwise (score := (next > 63) ? S(63, 8 bits) | ((next < -64) ? S(-64, 8 bits) | next))
  val odd = Reg(Bool()) init (True)
  when(score > 0)(odd := True) elsewhen (score < 0)(odd := False)
  io.odd := odd
  io.bits := RegNext(odd ? Cat((0 until 10).map(k => io.samples(18 - 2 * k)).reverse) |
    Cat((0 until 10).map(k => io.samples(19 - 2 * k)).reverse))
}

/** Locks the VCXO to the downstream. `error`: the DS crossing FIFO's fill from its middle, in words (the passive
  * sniffer), or twice that plus one, never zero (the in-line one: a bang-bang at the middle that holds the clocks'
  * phase, so the downstream latency, where the fill toggles; 2^10, 5, 13 with a 1 kHz RC: locked in 0.1 s, the phase
  * within 0.1 line bit for 33 to 300 ppm over the range, FPGA-USB-GPON-sniffer/ecp5/vcxo_loop_model.py). PI loop every
  * 2^updateShift clocks, duty to a first-order sigma-delta on the VC pin's RC filter (16 Hz, two poles); a higher
  * duty raises the frequency; `hold` (a power of two): clocks each bit of it lasts, for a transceiver's phase
  * interpolator that takes one step per that many (no VCXO). Off (no downstream): the duty holds. Default gains: ~0.5 Hz, damping ~1, for 100 ppm
  * over the VC range at 124.416 MHz (0.19 words/s per duty LSB). Closed-loop model (sniffer/ecp5/vcxo_loop_model.py):
  * locked within 0.5 word in 14 s from 10 ppm, stable for 33 to 300 ppm over the range; the VCXO's pull must exceed
  * its initial error. */
case class VcxoLoop(updateShift: Int = 14, kpShift: Int = 5, kiShift: Int = 9, hold: Int = 1) extends Component {
  require(isPow2(hold) && hold <= (1 << updateShift))
  val io = new Bundle {
    val enable = in Bool ()
    val error = in SInt (8 bits)
    val duty = out UInt (16 bits)
    val vc = out Bool ()
  }
  val tick = Reg(UInt(updateShift bits)) init (0)
  tick := tick + 1
  def clamp(v: SInt, hi: BigInt): SInt = (v < 0) ? S(0, v.getWidth bits) | ((v > hi) ? S(hi, v.getWidth bits) | v)
  // one step per clock on the update's first four clocks, for timing; the error is a few clocks old
  val error = RegNext(io.error) init (0)
  val kiError = RegNext((error << kiShift).resize(34 bits)) init (0) // gains are powers of two: no multiplier
  val kpError = RegNext((error << kpShift).resize(20 bits)) init (0)
  val integral = Reg(SInt(34 bits)) init (S(BigInt(0x8000) << 16, 34 bits)) // 16 fraction bits
  val step = Reg(SInt(34 bits)) init (0)
  val duty = Reg(UInt(16 bits)) init (0x8000)
  val enabled = Reg(Bool()) init (False)
  when(tick.andR)(enabled := io.enable) // for a whole update
  when(enabled) {
    switch(tick) {
      is(0)(step := integral + kiError)
      is(1)(integral := clamp(step, BigInt(0xFFFF) << 16))
      is(2)(step := ((integral >> 16).resize(20 bits) + kpError).resize(34 bits))
      is(3)(duty := clamp(step, 0xFFFF).asUInt.resize(16 bits))
    }
  }
  val sum = Reg(UInt(16 bits)) init (0)
  val carry = sum +^ duty
  val vc = Reg(Bool()) init (False)
  when(if (hold == 1) True else tick(log2Up(hold) - 1 downto 0) === 0) {
    sum := carry.resize(16 bits)
    vc := carry.msb
  }
  io.vc := vc
  io.duty := duty
}

/** The sniffer card's ECP5 fabric, vendor-neutral. Default domain: the VCXO's SerDes clock (124.416 MHz), where the
  * upstream lane is read. `ds`: the downstream lane's recovered clock. `out`: 77.76 MHz from the default domain (x5/8).
  * Lane words: 20 bits, first received bit in bit 0 (the ECP5 DCU's order). To the DE25-Nano, per `out` clock:
  * 32 downstream and 16 upstream line bits, first bit in the MSB; the first half on the rising edge.
  * `inline`: for a repeater without a VCXO, the loop steers the transmitter's phase interpolator and holds the
  * downstream latency (see VcxoLoop); `dsLine` is what it sends again, the upstream stays outside (UsClean). */
case class SnifferBridge(fifoDepth: Int = 16, inline: Boolean = false, dsDomain: ClockDomain = null, outDomain: ClockDomain = null) extends Component {
  val dsCd = if (dsDomain != null) dsDomain else ClockDomain.external("ds")
  val outCd = if (outDomain != null) outDomain else ClockDomain.external("out")
  val io = new Bundle {
    val dsWord = in Bits (20 bits) // ds domain
    val usWord = !inline generate (in Bits (20 bits)) // 2x oversampled
    val dsSignal = in Bool ()      // async: downstream present and its CDR locked
    val vcxo = out Bool ()
    val duty = out UInt (16 bits)
    val usOdd = !inline generate (out Bool ())
    val dsLine = out Bits (20 bits) // the downstream in this domain, first bit in the MSB; zero while `dsValid` is not
    val dsValid = out Bool ()
    val dsPins = out Bits (32 bits) // out domain
    val usPins = !inline generate (out Bits (16 bits))
  }
  // downstream into the VCXO domain: once half full the FIFO pops every clock, so its fill is the clocks' phase
  val crossing = StreamFifoCC(Bits(20 bits), fifoDepth, dsCd, clockDomain)
  crossing.io.push.valid := True
  crossing.io.push.payload := io.dsWord.reversed
  val popping = Reg(Bool()) init (False)
  when(!popping && crossing.io.popOccupancy >= fifoDepth / 2)(popping := True)
  when(popping && !crossing.io.pop.valid)(popping := False)
  crossing.io.pop.ready := popping
  io.dsValid := popping && crossing.io.pop.valid
  val ds = io.dsValid ? crossing.io.pop.payload | B(0, 20 bits)
  io.dsLine := ds

  val loop = if (inline) VcxoLoop(10, 5, 13, hold = 8) else VcxoLoop()
  val fill = crossing.io.popOccupancy.intoSInt - S(fifoDepth / 2)
  loop.io.enable := BufferCC(io.dsSignal, False)
  loop.io.error := RegNext((if (inline) (fill << 1) + 1 else fill).resize(8 bits)) init (0)
  io.vcxo := loop.io.vc
  io.duty := loop.io.duty

  // both gearboxes start together at the same ratio, so their words come on the same clocks
  val dsGear = Gearbox(20, 32)
  dsGear.io.input.push(ds)
  val upstream = !inline generate new Area {
    val pick = PhasePick()
    pick.io.samples := io.usWord.reversed
    io.usOdd := pick.io.odd
    val gear = Gearbox(10, 16)
    gear.io.input.push(pick.io.bits)
  }
  val toOut = StreamFifoCC(Bits((if (inline) 32 else 48) bits), 16, clockDomain, outCd)
  toOut.io.push.valid := dsGear.io.output.valid
  toOut.io.push.payload := (if (inline) dsGear.io.output.payload else dsGear.io.output.payload ## upstream.gear.io.output.payload)

  val output = new ClockingArea(outCd) {
    val started = Reg(Bool()) init (False)
    when(!started && toOut.io.popOccupancy >= 4)(started := True)
    when(started && !toOut.io.pop.valid)(started := False)
    toOut.io.pop.ready := started
    val word = RegNext((started && toOut.io.pop.valid) ? toOut.io.pop.payload | B(0)) init (0)
    io.dsPins := word(word.high downto word.getWidth - 32)
    if (!inline) io.usPins := word(15 downto 0)
  }
}
