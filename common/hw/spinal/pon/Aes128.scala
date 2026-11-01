// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.lib._

case class Aes128Input() extends Bundle {
  val block = Bits(128 bits) // FIPS-197 input, byte 0 in the MSBs
  val key = Bits(128 bits)
}

/** AES-128 encryption (FIPS-197), round keys expanded on the fly, so a new key costs nothing. Two clocks per round:
  * S-boxes, then MixColumns and AddRoundKey; two blocks in flight, 20 clocks each. A block enters when `input.ready`;
  * results leave in order on `output`, no back-pressure. */
case class Aes128() extends Component {
  val io = new Bundle {
    val input = slave Stream (Aes128Input())
    val output = master Flow (Bits(128 bits))
  }
  def bytes(x: Bits): Seq[Bits] = (0 until x.getWidth / 8).map(i => x(x.getWidth - 1 - 8 * i downto x.getWidth - 8 - 8 * i))
  def subBytes(x: Bits): Bits = Cat(bytes(x).map(Aes128.sbox).reverse)
  def xtime(a: Bits): Bits = (a(6 downto 0) ## False) ^ (a(7) ? B"8'x1B" | B"8'x00")
  def mixColumns(x: Bits): Bits = Cat(bytes(x).grouped(4).toSeq.flatMap { case Seq(a0, a1, a2, a3) =>
    Seq(xtime(a0 ^ a1) ^ a1 ^ a2 ^ a3, a0 ^ xtime(a1 ^ a2) ^ a2 ^ a3, a0 ^ a1 ^ xtime(a2 ^ a3) ^ a3, xtime(a3 ^ a0) ^ a0 ^ a1 ^ a2)
  }.reverse)
  def shiftRows(x: Bits): Bits = { val b = bytes(x); Cat((0 until 16).map(i => b((i + 4 * (i % 4)) % 16)).reverse) }

  case class Slot() extends Bundle {
    val state, key = Bits(128 bits) // key: the last round's
    val round = UInt(4 bits)        // next round, 1-10
  }
  val p = Reg(Flow(Slot())) // AddRoundKey done
  val q = Reg(Flow(Slot())) // SubBytes and ShiftRows done
  val g = Reg(Bits(32 bits)) // SubWord(RotWord(w3)) ^ Rcon, with q
  p.valid init (False)
  q.valid init (False)

  q.valid := p.valid
  q.round := p.round
  q.key := p.key
  q.state := shiftRows(subBytes(p.state))
  val w3 = p.key(31 downto 0)
  g := subBytes(w3(23 downto 0) ## w3(31 downto 24)) ^ (p.round.muxList(B(0, 8 bits), Aes128.rcon.zipWithIndex.map { case (c, i) => (i + 1, B(c, 8 bits)) }) ## B(0, 24 bits))

  val k = (0 until 4).map(i => q.key(127 - 32 * i downto 96 - 32 * i))
  val w0 = k(0) ^ g
  val w1 = k(1) ^ w0
  val w2 = k(2) ^ w1
  val roundKey = w0 ## w1 ## w2 ## (k(3) ^ w2)
  val finalRound = q.round === 10
  val result = ((finalRound ? q.state | mixColumns(q.state)) ^ roundKey)
  val looping = q.valid && !finalRound
  io.input.ready := !looping
  p.valid := looping || io.input.valid
  p.state := looping ? result | (io.input.block ^ io.input.key)
  p.key := looping ? roundKey | io.input.key
  p.round := looping ? (q.round + 1) | U(1, 4 bits)

  io.output.valid := RegNext(q.valid && finalRound) init (False)
  io.output.payload := RegNext(result)
}

object Aes128 {
  val latency = 21 // clocks from input.fire to output.valid
  val rcon = Seq(0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1B, 0x36)
  /** FIPS-197 S-box, computed: affine map of the GF(2^8) inverse. */
  val sboxTable: Seq[Int] = {
    def mul(a: Int, b: Int): Int = (0 until 8).foldLeft((0, a)) { case ((acc, x), i) =>
      (if (((b >> i) & 1) == 1) acc ^ x else acc, ((x << 1) ^ (if ((x & 0x80) != 0) 0x11B else 0)) & 0xFF)
    }._1
    def rotl(x: Int, n: Int): Int = ((x << n) | (x >> (8 - n))) & 0xFF
    (0 until 256).map { x =>
      val inv = if (x == 0) 0 else (1 until 256).find(mul(x, _) == 1).get
      inv ^ rotl(inv, 1) ^ rotl(inv, 2) ^ rotl(inv, 3) ^ rotl(inv, 4) ^ 0x63
    }
  }
  def sbox(x: Bits): Bits = Vec(sboxTable.map(B(_, 8 bits)))(x.asUInt)
}
