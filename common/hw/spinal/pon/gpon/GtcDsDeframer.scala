// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.lib._
import GponCodes._

case class GemRxByte() extends Bundle {
  val portId = UInt(12 bits)
  val pti = Bits(3 bits)
  val data = Bits(8 bits)
}

case class Checked[T <: Data](valueType: HardType[T]) extends Bundle {
  val value = valueType()
  val crcOk = Bool()
}

object FrameSync extends SpinalEnum { val HUNT, PRESYNC, SYNC = newElement() }

/** ONU downstream GTC deframer (G.984.3 clause 8.1): PSync hunt on any bit alignment, descrambling,
  * Ident, PLOAM, BIP, PLend, BWmap and GEM delineation. No FEC; CRC and HEC detect only (no correction). */
case class GtcDsDeframer() extends Component {
  val io = new Bundle {
    val line = in Bits (8 bits) // raw line bytes, any bit alignment, MSB first
    val synced = out Bool ()
    val superframe = out UInt (30 bits)
    val fecIndication = out Bool ()
    val ploam = master Flow (Checked(Bits(96 bits)))
    val alloc = master Flow (Checked(Allocation()))
    val gem = master Flow (Fragment(GemRxByte())) // last: end of the GEM fragment
    val bipErrors = out UInt (32 bits)
    val plendErrors = out UInt (32 bits)
    val hecErrors = out UInt (32 bits)
    val gemCuts = out UInt (32 bits) // GEM fragments cut by the frame end
  }

  // PSync search on the 8 bit alignments of the last 5 bytes (clause 8.1.3.1, M1 = 2, M2 = 5),
  // registered: the frame is processed from `history`, one byte behind the line
  val history = Reg(Bits(40 bits)) init (0)
  val window = history(31 downto 0) ## io.line
  history := window
  val found = RegNext(Vec((0 until 8).map(o => window(o + 31 downto o) === B(psync, 32 bits))))
  val alignment = Reg(UInt(3 bits)) init (0)
  val byte = (history >> alignment)(7 downto 0)

  val sync = RegInit(FrameSync.HUNT)
  val count = Reg(UInt(3 bits))  // PSync found in a row (PRESYNC) or missed in a row (SYNC)
  val pos = Reg(UInt(16 bits))   // frame position of `byte`, meaningful out of HUNT
  val active = sync === FrameSync.SYNC
  io.synced := active

  when(sync === FrameSync.HUNT) {
    when(found.orR) {
      alignment := OHToUInt(OHMasking.first(found.asBits))
      pos := 4
      count := 1
      sync := FrameSync.PRESYNC
    }
  } otherwise {
    pos := (pos === frameBytes - 1) ? U(0, 16 bits) | pos + 1
    when(pos === 3) {
      val ok = found(alignment)
      when(sync === FrameSync.PRESYNC) {
        count := count + 1
        when(!ok)(sync := FrameSync.HUNT) elsewhen (count === 1)(sync := FrameSync.SYNC)
        when(ok && count === 1)(count := 0)
      } otherwise {
        count := ok ? U(0, 3 bits) | count + 1
        when(!ok && count === 4)(sync := FrameSync.HUNT)
      }
    }
  }

  val scrambler = Reg(Bits(7 bits))
  val (keystream, scramblerNext) = scramblerByte((pos === 4) ? B"7'x7F" | scrambler)
  scrambler := scramblerNext
  val clear = (pos < 4) ? byte | byte ^ keystream

  val crcStart = pos === 8 || pos === 22 || pos === 26 || (pos >= 30 && (pos - 30)(2 downto 0) === 0)
  val crc = Reg(Bits(8 bits))
  val crcIn = crcStart ? B(0, 8 bits) | crc
  val crcOk = crcIn === clear
  crc := crc8Step(crcIn, clear)
  val shift = Reg(Bits(96 bits)) // last field bytes, newest in the low byte
  shift := (shift ## clear)(95 downto 0)

  val superframe = Reg(UInt(30 bits)) init (0)
  val fecIndication = Reg(Bool()) init (False)
  io.superframe := superframe
  io.fecIndication := fecIndication
  when(active && pos === 7) {
    superframe := (shift(23 downto 0) ## clear)(29 downto 0).asUInt
    fecIndication := shift(23)
  }

  val ploam = Flow(Checked(Bits(96 bits)))
  ploam.valid := active && pos === 20
  ploam.value := shift
  ploam.crcOk := crcOk
  io.ploam << ploam.stage()

  // error counters add one clock after the error (timing)
  val bip = Reg(Bits(8 bits)) init (0)
  val bipErrorBits = Reg(UInt(4 bits)) init (0)
  val bipErrors = Reg(UInt(32 bits)) init (0)
  io.bipErrors := bipErrors
  bipErrorBits := 0
  bipErrors := bipErrors + bipErrorBits
  when(sync =/= FrameSync.HUNT && pos === 21) {
    bip := 0
    when(active)(bipErrorBits := CountOne(bip ^ clear))
  } otherwise {
    bip := bip ^ clear
  }

  // PLend sent twice; detect-only subset of table 8-a
  val plendA = Reg(Bits(24 bits))
  val plendAOk = Reg(Bool())
  when(pos === 25) {
    plendA := shift(23 downto 0)
    plendAOk := crcOk
  }
  val plendB = shift(23 downto 0)
  val frameOk = Reg(Bool()) init (False)
  val pcbdEnd = Reg(UInt(16 bits))
  val plendError = RegNext(False) init (False)
  val plendErrors = Reg(UInt(32 bits)) init (0)
  io.plendErrors := plendErrors
  plendErrors := plendErrors + plendError.asUInt
  when(pos === 29) {
    val usable = (plendAOk && crcOk) ? (plendA === plendB) | (plendAOk || crcOk)
    val plend = plendAOk ? plendA | plendB
    frameOk := active && usable
    pcbdEnd := U(30, 16 bits) + (plend(23 downto 12).asUInt << 3)
    plendError := active && !usable
  }
  val inBwmap = frameOk && pos >= 30 && pos < pcbdEnd
  val inPayload = frameOk && pos >= pcbdEnd && pos >= 30

  val alloc = Flow(Checked(Allocation()))
  alloc.valid := inBwmap && (pos - 30)(2 downto 0) === 7
  alloc.value := Allocation.fromLine(shift(55 downto 0))
  alloc.crcOk := crcOk
  io.alloc << alloc.stage()

  val delineator = GemDelineator()
  delineator.io.restart := !inPayload
  delineator.io.byte.valid := inPayload
  delineator.io.byte.payload := clear
  io.gem << delineator.io.gem.stage()
  val hecError = RegNext(delineator.io.hecError) init (False)
  val hecErrors = Reg(UInt(32 bits)) init (0)
  io.hecErrors := hecErrors
  hecErrors := hecErrors + hecError.asUInt
  val gemCut = RegNext(delineator.io.cut) init (False)
  val gemCuts = Reg(UInt(32 bits)) init (0)
  io.gemCuts := gemCuts
  gemCuts := gemCuts + gemCut.asUInt
}
