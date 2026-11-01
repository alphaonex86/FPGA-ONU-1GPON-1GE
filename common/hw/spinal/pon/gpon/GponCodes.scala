// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._

/** G.984.3 codes in hardware, plus Scala reference models for the tests. All MSB first. */
object GponCodes {
  val frameBytes = 38880                  // downstream GTC frame: 125 us at 2.48832 Gb/s
  val fecDataBytes = 239                  // RS(255,239): data bytes per codeword, 16 parity follow
  val fecLastData = 38864                 // with FEC, the last (short) codeword's parity starts here
  val usFrameBytes = 19440                // upstream GTC frame: 125 us at 1.24416 Gb/s
  val usDelimiter = BigInt("AB5983", 16)  // usual upstream delimiter (set by Upstream_Overhead)
  val psync = BigInt("B6AB31E0", 16)
  val gemMask = BigInt("B6AB31E055", 16)  // GEM header XOR mask, also the idle header on the line
  val noMessage = BigInt("FF0B", 16) << 80 // PLOAM "No message": broadcast ONU-ID, Message-ID 0x0B

  /** CRC-8 x^8+x^2+x+1, init 0, no final XOR (PLOAM, PLend, BWmap): one byte. */
  def crc8Step(crc: Bits, byte: Bits): Bits = {
    var c = crc
    for (i <- 7 downto 0) {
      val feedback = c.msb ^ byte(i)
      c = (c(6 downto 0) ## False) ^ (B"8'x07" & B(8 bits, default -> feedback))
    }
    c
  }

  /** GEM header before the mask: PLI, Port-ID, PTI, BCH(39,12) remainder, even parity. */
  def gemHeader(pli: UInt, portId: UInt, pti: Bits): Bits = {
    val data = pli.asBits ## portId.asBits ## pti
    var rem = B(0, 12 bits)
    for (i <- 26 downto 0) {
      val feedback = rem.msb ^ data(i)
      rem = (rem(10 downto 0) ## False) ^ (B"12'x539" & B(12 bits, default -> feedback))
    }
    val code = data ## rem
    code ## code.xorR
  }

  /** True when an unmasked 40-bit header has a valid HEC (detection only, no correction yet). */
  def gemHeaderValid(header: Bits): Bool =
    gemHeader(header(39 downto 28).asUInt, header(27 downto 16).asUInt, header(15 downto 13)) === header

  /** x^7+x^6+1 frame-synchronous scrambler: next 8 keystream bits and the next state. */
  def scramblerByte(state: Bits): (Bits, Bits) = {
    var s = state
    val bits = for (_ <- 0 until 8) yield {
      val out = s(6)
      s = s(5 downto 0) ## (s(6) ^ s(5))
      out
    }
    (Cat(bits.reverse), s)
  }

  object Model {
    def crc8(bytes: Seq[Int]): Int = bytes.foldLeft(0) { (crc, byte) =>
      (7 to 0 by -1).foldLeft(crc) { (c, i) =>
        val feedback = ((c >> 7) ^ (byte >> i)) & 1
        ((c << 1) & 0xFF) ^ (if (feedback == 1) 0x07 else 0)
      }
    }

    def gemHeader(pli: Int, portId: Int, pti: Int): BigInt = {
      val data = (pli.toLong << 15) | (portId.toLong << 3) | pti
      val rem = (26 to 0 by -1).foldLeft(0L) { (r, i) =>
        val feedback = ((r >> 11) ^ (data >> i)) & 1
        ((r << 1) & 0xFFF) ^ (if (feedback == 1) 0x539 else 0)
      }
      val code = (data << 12) | rem
      BigInt((code << 1) | (java.lang.Long.bitCount(code) & 1))
    }

    /** Keystream from the first byte after PSync (downstream) or after the delimiter (upstream). */
    def keystream(n: Int): Seq[Int] = {
      var s = 0x7F
      Seq.fill(n) {
        (7 to 0 by -1).foldLeft(0) { (byte, _) =>
          val out = (s >> 6) & 1
          s = ((s << 1) & 0x7F) | (((s >> 6) ^ (s >> 5)) & 1)
          (byte << 1) | out
        }
      }
    }

    def gfMul(a: Int, b: Int): Int = (0 until 8).foldLeft((0, a)) { case ((acc, x), i) =>
      (if (((b >> i) & 1) == 1) acc ^ x else acc, ((x << 1) ^ (if ((x & 0x80) != 0) 0x11D else 0)) & 0xFF)
    }._1
    val rsGenerator = Seq(0x3B, 0x24, 0x32, 0x62, 0xE5, 0x29, 0x41, 0xA3, 0x08, 0x1E, 0xD1, 0x44, 0xBD, 0x68, 0x0D, 0x3B) // g0-g15

    val alphaPow: Seq[Int] = Seq.iterate(1, 255)(gfMul(_, 2)) // alpha^i, alpha = 2
    def gfInv(x: Int): Int = if (x == 0) 0 else alphaPow((255 - alphaPow.indexOf(x)) % 255)

    /** RS(255,239) parity of a codeword's data bytes (short: zero pad implied), sent first first (G.984.3 A.3). */
    def rsParity(data: Seq[Int]): Seq[Int] = {
      val p = Array.fill(16)(0)
      for (d <- data) {
        val feedback = d ^ p(15)
        for (i <- 15 to 1 by -1) p(i) = p(i - 1) ^ gfMul(rsGenerator(i), feedback)
        p(0) = gfMul(rsGenerator(0), feedback)
      }
      p.reverse.toSeq
    }

    /** Frame data bytes (36432) -> line bytes with the FEC parity inserted (G.984.3 13.2.1). */
    def fecEncode(data: Seq[Int]): Seq[Int] = data.grouped(fecDataBytes).flatMap(cw => cw ++ rsParity(cw)).toSeq

    /** Line position of frame data byte i, FEC on or off. */
    def linePos(i: Int, fec: Boolean): Int = if (fec) i + 16 * (i / fecDataBytes) else i

    /** Line bytes of one frame -> clear bytes (PSync is not scrambled). */
    def descramble(frame: Seq[Int]): Seq[Int] =
      frame.take(4) ++ frame.drop(4).zip(keystream(frame.length - 4)).map { case (b, k) => b ^ k }
  }
}
