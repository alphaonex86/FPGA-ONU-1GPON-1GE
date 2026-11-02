// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.lib._
import pon.gpon.GponCodes._

/** Upstream sniffer, one line byte per clock (155.52 MHz). The tap carries this ONU's bursts only, and the limiting
  * amp squelches the line to zero between them. Hunts the delimiter on any bit alignment (up to `delimiterErrors`
  * wrong bits), descrambles from the BIP (preset 0x7F) and records the burst (kind 3: BIP, ONU-ID, Ind, allocations)
  * until `quietBytes` zero line bytes, or `maxBytes` (flag 0). Zero line bytes at the burst's end look like the
  * squelch and are left out with it (one burst in 256 loses its last byte; the BWmap gives the true length). */
case class SnifferUs(quietBytes: Int = 8, maxBytes: Int = 20000, depth: Int = 32768) extends Component {
  val io = new Bundle {
    val line = in Bits (8 bits) // raw line bytes, any bit alignment, MSB first
    val time = in UInt (64 bits)
    val delimiter = in Bits (24 bits) // from the Upstream_Overhead PLOAM
    val delimiterErrors = in UInt (2 bits)
    val output = master Stream (Fragment(Bits(8 bits)))
    val drops = out UInt (32 bits)
    val truncations = out UInt (32 bits)
  }
  // delimiter search as GtcUsDeframer: the burst is read from `history`, its first byte two clocks after the hit
  val history = Reg(Bits(32 bits)) init (0)
  val window = history(23 downto 0) ## io.line
  history := window
  val found = RegNext(Vec((0 until 8).map(o => CountOne(window(o + 23 downto o) ^ io.delimiter) <= io.delimiterErrors)).asBits) init (0)
  val alignment = Reg(UInt(3 bits))
  val byte = (history >> alignment)(7 downto 0)

  val scramblerReset = RegNext(False) init (False)
  val scrambler = Reg(Bits(7 bits))
  val (keystream, scramblerNext) = scramblerByte(scramblerReset ? B"7'x7F" | scrambler)
  scrambler := scramblerNext

  // the last `quietBytes` bytes wait in `delay`: when they are all quiet, the burst ended before them
  val inBurst = RegInit(False)
  val delay = Vec(Reg(Bits(8 bits)), quietBytes)
  val filled = Reg(UInt(log2Up(quietBytes + 1) bits))
  val quiet = Reg(UInt(log2Up(quietBytes + 1) bits))
  val length = Reg(UInt(log2Up(maxBytes + 1) bits))
  val start = Reg(UInt(64 bits))
  val emit = inBurst && filled === quietBytes
  val ended = byte === 0 && quiet === quietBytes - 1
  val long = emit && length === maxBytes - 1 && !ended

  val records = RecordWriter(0, depth, depth / 64)
  records.io.data.valid := emit
  records.io.data.payload := delay.last
  records.io.close.valid := inBurst && (ended || long)
  records.io.close.kind := RecordKind.burst
  records.io.close.flags := B(0, 7 bits) ## long
  records.io.close.time := start
  records.io.close.extra.clearAll()
  io.output << records.io.output
  io.drops := records.io.drops
  io.truncations := records.io.truncations

  when(inBurst) {
    delay(0) := byte ^ keystream
    for (i <- 1 until quietBytes) delay(i) := delay(i - 1)
    when(!emit)(filled := filled + 1)
    quiet := (byte === 0) ? (quiet + 1) | U(0)
    when(emit)(length := length + 1)
    when(ended || long)(inBurst := False)
  } elsewhen (found.orR) {
    alignment := OHToUInt(OHMasking.first(found))
    scramblerReset := True
    start := io.time
    filled := 0
    quiet := 0
    length := 0
    inBurst := True
  }
}
