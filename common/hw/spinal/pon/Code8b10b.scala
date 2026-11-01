// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.lib._
import scala.collection.mutable

/** IEEE 802.3 clause 36 8b/10b code. A code word: bit 0 = a, sent first; bit 9 = j. Running disparity: true = RD+. */
object Code8b10b {
  // 5b/6b (abcdei, a = MSB here) with RD-, RD+ alternates; balanced ones have the same code
  private val code6 = Seq(0x27, 0x1D, 0x2D, 0x31, 0x35, 0x29, 0x19, 0x38, 0x39, 0x25, 0x15, 0x34, 0x0D, 0x2C, 0x1C, 0x17,
    0x1B, 0x23, 0x13, 0x32, 0x0B, 0x2A, 0x1A, 0x3A, 0x33, 0x26, 0x16, 0x36, 0x0E, 0x2E, 0x1E, 0x2B)
  private val k28_6 = 0x0F
  // 3b/4b (fghj, f = MSB) with RD-; P7 primary, A7 alternate; the control ones all alternate
  private val code4 = Seq(0xB, 0x9, 0x5, 0xC, 0xD, 0xA, 0x6, 0xE)
  private val code4K = Seq(0xB, 0x6, 0xA, 0xC, 0xD, 0x5, 0x9, 0x7)
  private val a7 = 0x7
  private def ones(v: Int) = Integer.bitCount(v)
  private def flip(v: Int, n: Int) = ~v & ((1 << n) - 1)
  /** The sub-block for the current RD: RD- column as given, RD+ its complement when unbalanced (and for 000111/0011). */
  private def pick(v: Int, n: Int, rdPlus: Boolean, k: Boolean = false): Int =
    if (rdPlus && (k || ones(v) != n / 2 || v == 0x38 || v == 0xC)) flip(v, n) else v
  private def after(v: Int, n: Int, rdPlus: Boolean): Boolean = if (ones(v) == n / 2) rdPlus else ones(v) > n / 2
  val validK = Seq(0x1C, 0x3C, 0x5C, 0x7C, 0x9C, 0xBC, 0xDC, 0xFC, 0xF7, 0xFB, 0xFD, 0xFE)

  /** (code word, RD after) of byte (HGF EDCBA) as data or control, None for an invalid control. */
  def encode(byte: Int, k: Boolean, rdPlus: Boolean): Option[(Int, Boolean)] = {
    if (k && !validK.contains(byte)) return None
    val x = byte & 0x1F
    val y = byte >> 5
    val six = pick(if (k && x == 28) k28_6 else code6(x), 6, rdPlus)
    val rd = after(six, 6, rdPlus)
    val alternate = y == 7 && (k || (!rd && (x == 17 || x == 18 || x == 20)) || (rd && (x == 11 || x == 13 || x == 14)))
    val four = pick(if (alternate) a7 else if (k) code4K(y) else code4(y), 4, rd, k)
    val code = (six << 4) | four // abcdei fghj, a = bit 9 here
    Some((Integer.reverse(code) >>> 22, after(four, 4, rd)))
  }
  val comma = 0x7C // K28.5
  def isComma(word: Int): Boolean = (word & 0x7F) == 0x7C || (word & 0x7F) == 0x03 // abcdeif = 0011111 / 1100000
}

case class Symbol8b10b() extends Bundle {
  val data = Bits(8 bits)
  val k = Bool()
}

/** 8b/10b encoder, one symbol per clock, registered. */
case class Encoder8b10b() extends Component {
  val io = new Bundle {
    val input = in(Symbol8b10b())
    val code = out Bits (10 bits)
  }
  val rd = RegInit(False)
  val table = Vec((0 until 1024).map { i =>
    val (code, rdAfter) = Code8b10b.encode(i & 0xFF, (i >> 8 & 1) == 1, (i >> 9) == 1).getOrElse(Code8b10b.encode(0xFE, true, (i >> 9) == 1).get)
    B((if (rdAfter) 1 << 10 else 0) | code, 11 bits)
  })
  val entry = table((rd ## io.input.k ## io.input.data).asUInt)
  rd := entry(10)
  io.code := RegNext(entry(9 downto 0)) init (0)
}

/** 8b/10b decoder of aligned code words, registered: an invalid word or a disparity error gives /V/ (K30.7) and `error`. */
case class Decoder8b10b() extends Component {
  val io = new Bundle {
    val code = in Bits (10 bits)
    val output = out(Symbol8b10b())
    val error = out Bool ()
  }
  // per word: valid, symbol, valid with RD-, valid with RD+, RD after when unbalanced (else unchanged), balanced
  val entries = (0 until 1024).map(_ => mutable.Map[Boolean, (Int, Boolean)]())
  for (rd <- Seq(false, true); k <- Seq(false, true); b <- 0 until 256; (code, rdAfter) <- Code8b10b.encode(b, k, rd))
    entries(code)(rd) = ((if (k) 256 else 0) | b, rdAfter)
  val table = Vec(entries.map { e =>
    val symbol = e.values.headOption.map(_._1).getOrElse(0x1FE)
    val balanced = e.size == 2 && e(false)._2 == false && e(true)._2 == true
    val rdAfter = e.values.headOption.exists(_._2)
    B((if (e.contains(false)) 1 << 12 else 0) | (if (e.contains(true)) 1 << 11 else 0) | (if (balanced) 1 << 10 else 0) |
      (if (rdAfter) 1 << 9 else 0) | symbol, 13 bits)
  })
  val rd = RegInit(False)
  val entry = table(io.code.asUInt)
  val ok = rd ? entry(11) | entry(12)
  when(entry(12) || entry(11))(rd := entry(10) ? rd | entry(9))
  io.output.k := RegNext(!ok || entry(8))
  io.output.data := RegNext(ok ? entry(7 downto 0) | B(0xFE, 8 bits))
  io.error := RegNext(!ok) init (False)
}

/** Word alignment of raw 10-bit words (bit 0 first on the line) on the comma: moves after 3 commas in a row at a new
  * place, registered. */
case class CommaAligner() extends Component {
  val io = new Bundle {
    val raw = in Bits (10 bits)
    val code = out Bits (10 bits)
  }
  val previous = RegNext(io.raw) init (0)
  val window = io.raw ## previous // line order from bit 0
  val hits = Vec((0 until 10).map(o => window(o + 6 downto o) === B"7'b1111100" || window(o + 6 downto o) === B"7'b0000011"))
  val offset, candidate = Reg(UInt(4 bits)) init (0)
  val seen = Reg(UInt(2 bits)) init (0)
  when(hits.orR) {
    val at = OHToUInt(OHMasking.first(hits.asBits))
    when(at === offset) {
      seen := 0
    } elsewhen (at === candidate && seen =/= 0) {
      seen := seen + 1
      when(seen === 2) { offset := at; seen := 0 }
    } otherwise { candidate := at; seen := 1 }
  }
  io.code := RegNext((window >> offset)(9 downto 0)) init (0)
}
