// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._

object GtsPonLanes {
  def double(nibble: Bits): Bits = Vec(nibble.asBools.flatMap(b => Seq(b, b))).asBits // each bit twice, MSB first
  def decimate(bits: Bits): Bits = Vec((0 until bits.getWidth / 2).map(i => bits(2 * i + 1))).asBits // every other bit
}

/** MAC lines <-> the four PON GTS lanes. Every lane runs 2488.32 Mb/s, 8 bits per word at 311.04 MHz (the default
  * clock domain, the lanes' core clock; first bit in bit 0); the MAC lines are MSB first. Upstream 1244.16 Mb/s rides
  * 2x oversampled: each bit sent twice (ONU TX, burst enable), every other bit kept (OLT RX; either sample of a bit
  * is good). usClk is 155.52 MHz from the same source. P/N swapped receive lanes are inverted in the PMA. */
case class GtsPonLanes() extends Component {
  import GtsPonLanes._
  val io = new Bundle {
    val usClk = in Bool ()
    val usReset = in Bool ()
    val oltTxWord = out Bits (8 bits)
    val oltRxWord = in Bits (8 bits)
    val onuTxWord = out Bits (8 bits)
    val benTxWord = out Bits (8 bits)
    val onuRxWord = in Bits (8 bits)
    val oltDsLine = in Bits (8 bits)  // downstream domain
    val onuDsLine = out Bits (8 bits) // downstream domain
    val oltUsLine = out Bits (8 bits) // upstream domain
    val onuUsLine = in Bits (8 bits)  // upstream domain
    val onuUsBen = in Bits (8 bits)   // upstream domain
  }
  val usDomain = ClockDomain(io.usClk, io.usReset, config = ClockDomain.current.config)
  usDomain.setSynchronousWith(ClockDomain.current)

  io.oltTxWord := RegNext(io.oltDsLine.reversed) init (0)
  io.onuDsLine := RegNext(io.onuRxWord.reversed) init (0)

  val usPhase = usDomain(RegInit(False))
  usPhase := !usPhase
  val phaseSeen = RegNext(usPhase) init (False)
  val secondHalf = phaseSeen =/= RegNext(phaseSeen).init(False) // second downstream clock of the upstream byte
  def halfOf(byte: Bits): Bits = (secondHalf ? double(byte(3 downto 0)) | double(byte(7 downto 4))).reversed
  io.onuTxWord := RegNext(halfOf(io.onuUsLine)) init (0)
  io.benTxWord := RegNext(halfOf(io.onuUsBen)) init (0)

  val rxWord = RegNext(io.oltRxWord.reversed) init (0)
  val rxPrevious = RegNext(rxWord) init (0)
  io.oltUsLine := usDomain(RegNext(decimate(rxPrevious ## rxWord)) init (0))
}
