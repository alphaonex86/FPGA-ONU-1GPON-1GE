// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.lib._

object DbruReporter {
  /** G.984.3 Table 8-1: reporting blocks to the DBRu octet (truncated; 0xFE beyond 8191, 0xFF never sent). */
  def code(blocks: UInt): Bits = {
    val c = Bits(8 bits)
    c := B"8'xFE"
    for (n <- 6 to 1 by -1) when(blocks < (1 << (7 + n)))(c := B((1 << n) - 1, n bits) ## B"0" ## blocks(5 + n downto 2 * n - 1))
    when(blocks < 128)(c := B"0" ## blocks(6 downto 0))
    c
  }
  def model(blocks: Int): Int =
    if (blocks < 128) blocks
    else if (blocks > 8191) 0xFE
    else { val n = 31 - Integer.numberOfLeadingZeros(blocks) - 6; ((0xFF << (8 - n)) & 0xFF) | ((blocks >> (2 * n - 1)) & ((1 << (7 - n)) - 1)) }
}

/** DBRu report of each stream (G.984.3 8.4.2): its bytes waiting, rounded up to reporting blocks, as the Table 8-1
  * octet. One serial divider serves the streams in turn: a report is at most 19 clocks per stream old. */
case class DbruReporter(streams: Int) extends Component {
  val io = new Bundle {
    val bytes = in Vec (UInt(16 bits), streams)
    val blockBytes = in UInt (16 bits) // quasi-static (OMCI ANI-G GEM block length)
    val codes = out Vec (Bits(8 bits), streams)
  }
  val stream = Reg(UInt(log2Up(streams) bits)) init (0)
  val step = Reg(UInt(5 bits)) init (0)
  val remainder = Reg(UInt(17 bits))
  val quotient = Reg(UInt(17 bits))
  val codes = Vec(Reg(Bits(8 bits)) init (0), streams)
  io.codes := codes
  // restoring division of bytes + blockBytes - 1 (rounds up), MSB first
  val dividend = io.bytes(stream).resize(17) + io.blockBytes - 1
  val trial = (remainder(15 downto 0) @@ quotient.msb) // the remainder shifted in a dividend bit
  val fits = trial >= io.blockBytes
  step := step + 1
  when(step === 0) {
    remainder := 0
    quotient := dividend
  } elsewhen (step <= 17) {
    remainder := fits ? (trial - io.blockBytes) | trial
    quotient := quotient(15 downto 0) @@ fits
  } otherwise {
    step := 0
    codes(stream) := DbruReporter.code(quotient)
    stream := (stream === streams - 1) ? U(0) | stream + 1
  }
}
