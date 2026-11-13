// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.lib._

/** Upstream burst regenerator of an in-line repeater: the picked line bits, 10 per clock (first in bit 9), leave
  * `length` half-words (5 bits each) later with the laser enable of each half-word. The line is zero between bursts
  * (squelch). A burst starts at three half-words with ones in a row and ends before `quiet` half-words of zeros, so
  * the laser goes off at the last one (trailing zeros of the data are sent dark: still zeros) and a stray one never
  * lights it. The receiver loses the burst's head while it settles: the laser comes on `head` half-words before the
  * first received one, and from there to `garble` half-words past it the bits are rebuilt from the 8 bits that
  * follow, repeated backwards (the preamble is a repeated byte). The burst keeps its slot: only the constant delay
  * is added, which ranging absorbs. `quiet` is long (65 bits) so that no zero run of scrambled data ends a burst: a
  * rebuilt head there would overwrite data. Bursts closer than that are one to the laser, the second's head as received. */
case class UsRegen(length: Int = 20, quiet: Int = 13) extends Component {
  val tap = 3 // start search here: two newer half-words confirm, `quiet` older ones fit
  require(length - 2 - quiet + 1 >= 0 && tap + 3 + 7 + 2 < length)
  val io = new Bundle {
    val bits = in Bits (10 bits)
    val enable = in Bool ()
    val head = in UInt (3 bits)
    val garble = in UInt (2 bits)
    val output = out Bits (10 bits)
    val ben = out Bits (2 bits) // laser on: bit 1 the first half-word of `output`, bit 0 the second
    val active = out Bool ()
    val busy = out Bool () // a burst is coming in (at the start search)
  }
  // index 0 newest; two half-words in per clock
  val data = Vec(Reg(Bits(5 bits)) init (0), length)
  val one = Vec(Reg(Bool()) init (False), length)   // ones received in the half-word
  val mark = Vec(Reg(Bool()) init (False), length)  // rebuilt: laser on whatever follows
  val dataNext = Vec(Bits(5 bits), length)
  val markNext = Vec(Bool(), length)
  for (i <- 2 until length) {
    dataNext(i) := data(i - 2)
    markNext(i) := mark(i - 2)
    one(i) := one(i - 2)
  }
  dataNext(1) := io.bits(9 downto 5)
  dataNext(0) := io.bits(4 downto 0)
  markNext(1) := False
  markNext(0) := False
  one(1) := io.bits(9 downto 5).orR
  one(0) := io.bits(4 downto 0).orR
  data := dataNext
  mark := markNext

  // burst search at the tap, the older half-word first
  val busy = Reg(Bool()) init (False)
  val zeros = Reg(UInt(log2Up(quiet + 1) bits)) init (0)
  def step(i: Int, busyIn: Bool, zerosIn: UInt): (Bool, Bool, UInt) = {
    val start = !busyIn && one(i) && one(i - 1) && one(i - 2)
    val run = one(i) ? U(0, zerosIn.getWidth bits) | zerosIn + 1
    (start, start || (busyIn && run =/= quiet), start ? U(0, zerosIn.getWidth bits) | run)
  }
  val (startOld, busyMid, zerosMid) = step(tap + 1, busy, zeros)
  val (startNew, busyEnd, zerosEnd) = step(tap, busyMid, zerosMid)
  busy := busyEnd
  zeros := zerosEnd
  val startedOld = RegNext(startOld && io.enable) init (False) // a clock later the burst's first half-word is at tap + 3
  val startedNew = RegNext(startNew && io.enable) init (False) // or tap + 2

  for ((started, first) <- Seq((startedOld, tap + 3), (startedNew, tap + 2)); g <- 0 to 3) {
    when(started && io.garble === g) {
      val origin = first - g // the newest rebuilt half-word; the pattern is the 8 bits after it
      val pattern = (data(origin - 1) ## data(origin - 2))(9 downto 2)
      for (m <- 0 to g + 7) {
        when(if (m <= g) True else io.head >= m - g) {
          // bit b of the half-word is k = 5 m + b + 1 bits before the pattern's first bit (pattern bit 7)
          dataNext(origin + m + 2) := Cat((0 until 5).map(b => pattern(7 - (800 - (5 * m + b + 1)) % 8)))
          markNext(origin + m + 2) := True
        }
      }
    }
  }

  // at the exit: on from a mark, off when no one comes within `quiet` half-words
  def ahead(i: Int): Bool = (i - quiet + 1 to i).map(one(_)).orR
  val lit = Reg(Bool()) init (False)
  val benOld = (lit || mark(length - 1)) && ahead(length - 1) && io.enable
  val benNew = (benOld || mark(length - 2)) && ahead(length - 2) && io.enable
  lit := benNew
  io.output := RegNext((benOld ? data(length - 1) | B(0, 5 bits)) ## (benNew ? data(length - 2) | B(0, 5 bits))) init (0)
  io.ben := RegNext(benOld ## benNew) init (0)
  io.active := lit
  io.busy := busy
}
