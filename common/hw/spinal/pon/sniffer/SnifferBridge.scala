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

/** Locks the VCXO to the downstream. `error`: the DS crossing FIFO's fill from its middle, in words. PI loop every
  * 2^updateShift clocks, duty to a first-order sigma-delta on the VC pin's RC filter (16 Hz, two poles); a higher
  * duty raises the frequency. Off (no downstream): the duty holds. Default gains: ~0.5 Hz, damping ~1, for 100 ppm
  * over the VC range at 124.416 MHz (0.19 words/s per duty LSB). Closed-loop model (sniffer/ecp5/vcxo_loop_model.py):
  * locked within 0.5 word in 14 s from 10 ppm, stable for 33 to 300 ppm over the range; the VCXO's pull must exceed
  * its initial error. */
case class VcxoLoop(updateShift: Int = 14, kpShift: Int = 5, kiShift: Int = 9) extends Component {
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
  sum := carry.resize(16 bits)
  io.vc := RegNext(carry.msb) init (False)
  io.duty := duty
}

/** The sniffer card's ECP5 fabric, vendor-neutral. Default domain: the VCXO's SerDes clock (124.416 MHz), where the
  * upstream lane is read. `ds`: the downstream lane's recovered clock. `out`: 77.76 MHz from the default domain (x5/8).
  * Lane words: 20 bits, first received bit in bit 0 (the ECP5 DCU's order). To the DE25-Nano, per `out` clock:
  * 32 downstream and 16 upstream line bits, first bit in the MSB; the first half on the rising edge. */
case class SnifferBridge(fifoDepth: Int = 16) extends Component {
  val dsCd = ClockDomain.external("ds")
  val outCd = ClockDomain.external("out")
  val io = new Bundle {
    val dsWord = in Bits (20 bits) // ds domain
    val usWord = in Bits (20 bits) // 2x oversampled
    val dsSignal = in Bool ()      // async: downstream present and its CDR locked
    val vcxo = out Bool ()
    val duty = out UInt (16 bits)
    val usOdd = out Bool ()
    val dsPins = out Bits (32 bits) // out domain
    val usPins = out Bits (16 bits)
  }
  // downstream into the VCXO domain: once half full the FIFO pops every clock, so its fill is the clocks' phase
  val crossing = StreamFifoCC(Bits(20 bits), fifoDepth, dsCd, clockDomain)
  crossing.io.push.valid := True
  crossing.io.push.payload := io.dsWord.reversed
  val popping = Reg(Bool()) init (False)
  when(!popping && crossing.io.popOccupancy >= fifoDepth / 2)(popping := True)
  when(popping && !crossing.io.pop.valid)(popping := False)
  crossing.io.pop.ready := popping
  val ds = (popping && crossing.io.pop.valid) ? crossing.io.pop.payload | B(0, 20 bits)

  val loop = VcxoLoop()
  loop.io.enable := BufferCC(io.dsSignal, False)
  loop.io.error := RegNext((crossing.io.popOccupancy.intoSInt - S(fifoDepth / 2)).resize(8 bits)) init (0)
  io.vcxo := loop.io.vc
  io.duty := loop.io.duty

  val pick = PhasePick()
  pick.io.samples := io.usWord.reversed
  io.usOdd := pick.io.odd

  // both gearboxes start together at the same ratio, so their words come on the same clocks
  val dsGear = Gearbox(20, 32)
  val usGear = Gearbox(10, 16)
  dsGear.io.input.push(ds)
  usGear.io.input.push(pick.io.bits)
  val toOut = StreamFifoCC(Bits(48 bits), 16, clockDomain, outCd)
  toOut.io.push.valid := dsGear.io.output.valid
  toOut.io.push.payload := dsGear.io.output.payload ## usGear.io.output.payload

  val output = new ClockingArea(outCd) {
    val started = Reg(Bool()) init (False)
    when(!started && toOut.io.popOccupancy >= 4)(started := True)
    when(started && !toOut.io.pop.valid)(started := False)
    toOut.io.pop.ready := started
    val word = RegNext((started && toOut.io.pop.valid) ? toOut.io.pop.payload | B(0, 48 bits)) init (0)
    io.dsPins := word(47 downto 16)
    io.usPins := word(15 downto 0)
  }
}
