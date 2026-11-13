// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.lib._

/** Upstream 4x oversampled on a clock that is not the ONU's (a fixed oscillator, up to ~100 ppm away): 40 samples per
  * clock, first in bit 39. A bit is a cell of 4 samples that starts at phase `p`; edges seen one sample late or early
  * move a score that steps `p` at `threshold`, so one cell is 5 or 3 samples long and the cells follow the ONU's
  * bits; an edge mid-cell (jitter past a sample, or `p` two off) counts the way the score leans. After `quietWords`
  * zero words (the squelch between bursts) `p` jumps to where the word's edges are. A cell's decision is one of its two middle samples, the one away from where the edges
  * lean. `clean`: the samples three clocks later, each cell filled with its decision: the line again at a constant
  * delay, without the receiver's jitter. `bits`: the cells that start in the word, the first in bit 10: `count` is 10,
  * or 9 / 11 on the clock `p` wrapped (a late / early step past the word's start). */
case class UsClean(quietWords: Int = 4, threshold: Int = 8) extends Component {
  val io = new Bundle {
    val samples = in Bits (40 bits)
    val clean = out Bits (40 bits)
    val bits = out Bits (11 bits)
    val count = out UInt (4 bits)
  }
  val next = RegNext(io.samples) init (0) // the word after the one decided
  val word = RegNext(next) init (0)
  val previous = RegNext(word) init (0)

  // edge before each sample of the incoming word, counted per phase: `hist` goes with `next`
  val edges = (0 until 40).map(i => (if (i == 0) next(0) else io.samples(40 - i)) =/= io.samples(39 - i))
  val hist = Vec((0 until 4).map(k => RegNext(CountOne((k until 40 by 4).map(edges))) init (0)))
  val zero = RegNext(io.samples === 0) init (True)

  // `p`, `score` and the wraps go with `word`
  val p = Reg(UInt(2 bits)) init (0)
  val score = Reg(SInt(6 bits)) init (0)
  val lateWrap, earlyWrap = Reg(Bool()) init (False)
  val quiet = Reg(UInt(log2Up(quietWords + 1) bits)) init (0)
  quiet := zero ? (quiet + (quiet =/= quietWords).asUInt) | U(0)
  val (late, mid, early) = (hist(p + 1), hist(p + 2), hist(p + 3))
  val (low, high) = (hist(1) > hist(0), hist(3) > hist(2))
  val upper = (high ? hist(3) | hist(2)) > (low ? hist(1) | hist(0))
  val best = (upper ## (upper ? high | low)).asUInt
  val far = mid.intoSInt.resize(6 bits)
  val leaning = score + late.intoSInt.resize(6 bits) - early.intoSInt.resize(6 bits) + ((score >= 0) ? far | -far)
  lateWrap := False
  earlyWrap := False
  when(quiet === quietWords) {
    p := best
    score := 0
  } elsewhen (leaning >= threshold) {
    p := p + 1
    score := 0
    lateWrap := p === 3
  } elsewhen (leaning <= -threshold) {
    p := p - 1
    score := 0
    earlyWrap := p === 0
  } otherwise (score := leaning)

  def at(i: Int): Bool = if (i < 0) previous(-1 - i) else if (i < 40) word(39 - i) else next(79 - i)
  // cell j starts at p - 4 + 4 j; its decision is its second sample, the third when the edges lean late
  val offset = p +^ U(1) + (score >= 0).asUInt
  val cells = (0 to 10).map(j => Vec((1 to 8).map(o => at(4 * j - 4 + (o min 5))))(offset - 1))
  def cleaned(i: Int): Bool = Vec((0 to 3).map(phase => cells((i - phase + 4) / 4)))(p)
  io.clean := RegNext(Cat((0 until 40).map(t => cleaned(39 - t)))) init (0)
  val all = Cat((0 to 10).map(j => cells(10 - j))) // the cell before the word in bit 10
  io.bits := RegNext(earlyWrap ? all | (lateWrap ? (all |<< 2) | (all |<< 1))) init (0)
  io.count := RegNext(earlyWrap ? U(11, 4 bits) | (lateWrap ? U(9, 4 bits) | U(10, 4 bits))) init (10)
}
