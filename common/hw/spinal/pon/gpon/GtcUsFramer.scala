// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.lib._
import GponCodes._

object FramerState extends SpinalEnum { val IDLE, OVERHEAD, PLOU, ALLOC = newElement() }

/** ONU upstream GTC burst framer (G.984.3 clause 8.2), no FEC, one line byte per clock (155.52 MHz). Jobs come in
  * time order; a job starting where the previous one ends continues its burst, otherwise a burst opens with guard,
  * preamble, delimiter and PLOu. Allocation content: PLOAMu, DBRu (empty report), idle GEM frames. The line leaves on
  * the jobs' bit alignment with its burst enable (laser on), a fixed latency after io.time. */
case class GtcUsFramer() extends Component {
  val io = new Bundle {
    val job = slave Stream (UsJob())
    val time = in UInt (24 bits)      // ONU byte time
    val enable = in Bool ()           // low: no burst, the current one stops at once, jobs are dropped
    val onuId = in Bits (8 bits)
    val overhead = in(BurstOverhead())
    val snMessage = in Bits (96 bits) // Serial_Number_ONU with a zero random delay field
    val ploam = slave Stream (Bits(96 bits)) // other PLOAMu messages, No_message when none
    val line = out Bits (8 bits)
    val ben = out Bits (8 bits)       // burst enable per line bit
    val late = out Bool ()            // a job whose burst start had passed was dropped
  }
  import FramerState._

  def after(a: UInt, b: UInt): Bool = !(a - b).msb
  val head = io.job.payload
  val burstStart = (head.bit >> 3).resize(24) - 3 - io.overhead.bytes
  io.job.ready := False
  io.ploam.ready := False
  io.late := False

  val state = RegInit(IDLE)
  val shift = Reg(UInt(3 bits)) init (0)
  val index = Reg(UInt(8 bits))
  val allocLeft = Reg(UInt(16 bits))
  val nextBit = Reg(UInt(27 bits)) // where a contiguous job starts
  val ploamLeft = Reg(UInt(4 bits))
  val dbruLeft = Reg(UInt(2 bits))
  val gemIndex = Reg(UInt(3 bits))
  val message = Reg(Bits(96 bits))
  val fieldStart = Reg(Bool())
  val crc = Reg(Bits(8 bits))
  val crcIn = fieldStart ? B(0, 8 bits) | crc
  val bip = Reg(Bits(8 bits)) init (0) // since the BIP field of our last burst
  val scrambler = Reg(Bits(7 bits))
  val (keystream, scramblerNext) = scramblerByte(scrambler)

  def startAlloc(j: UsJob): Unit = {
    val bytes = j.alloc.stop - j.alloc.start + 1
    allocLeft := bytes
    nextBit := j.bit + (bytes << 3).resize(27)
    val ploamU = j.alloc.flags(10)
    ploamLeft := ploamU ? U(13, 4 bits) | U(0, 4 bits)
    dbruLeft := j.alloc.flags(8 downto 7).mux(1 -> U(2, 2 bits), 2 -> U(3, 2 bits), default -> U(0, 2 bits))
    gemIndex := 0
    fieldStart := True
    io.ploam.ready := ploamU && !j.sn
    message := j.sn ? (io.snMessage | (j.random.resize(12).asBits ## B(0, 4 bits)).resize(96)) |
      (io.ploam.valid ? io.ploam.payload | (io.onuId ## B"8'x04" ## B(0, 80 bits)))
  }

  // overhead bits of byte `index`, MSB first
  val ov = io.overhead
  val ones = ov.guard.resize(11) + ov.type1
  val zeros = ones + ov.type2
  val delimiterAt = (ov.bytes @@ U(0, 3 bits)) - 24
  val ovLine, ovBen = Bits(8 bits)
  for (k <- 0 until 8) {
    val p = index @@ U(k, 3 bits)
    ovLine(7 - k) := (p >= ov.guard && p < ones) ||
      (p >= zeros && p < delimiterAt && ov.pattern(U(7, 3 bits) - (p - zeros)(2 downto 0))) ||
      (p >= delimiterAt && ov.delimiter(U(23, 5 bits) - (p - delimiterAt)(4 downto 0)))
    ovBen(7 - k) := p >= ov.guard
  }

  val clear = Bits(8 bits)
  clear := 0
  val scrambled, overheadByte = False
  switch(state) {
    is(IDLE) {
      when(!io.enable) {
        io.job.ready := io.job.valid
      } elsewhen (io.job.valid) {
        when(io.time === burstStart) {
          state := OVERHEAD
          index := 0
          shift := head.bit(2 downto 0)
        } elsewhen (after(io.time, burstStart)) {
          io.job.ready := True
          io.late := True
        }
      }
    }
    is(OVERHEAD) {
      overheadByte := True
      index := index + 1
      when(index === ov.bytes - 1) { state := PLOU; index := 0 }
    }
    is(PLOU) {
      scrambled := True
      index := index + 1
      switch(index(1 downto 0)) {
        is(0)(clear := bip)
        is(1)(clear := io.onuId)
        default {
          clear := io.ploam.valid ## B(0, 7 bits) // Ind: PLOAMu waiting
          io.job.ready := True
          startAlloc(head)
          state := ALLOC
        }
      }
    }
    is(ALLOC) {
      scrambled := True
      allocLeft := allocLeft - 1
      when(ploamLeft =/= 0) {
        ploamLeft := ploamLeft - 1
        clear := (ploamLeft === 1) ? crcIn | message(95 downto 88)
        message := message |<< 8
        fieldStart := ploamLeft === 1 // a DBRu may follow
      } elsewhen (dbruLeft =/= 0) {
        dbruLeft := dbruLeft - 1
        clear := (dbruLeft === 1) ? crcIn | B(0, 8 bits)
        fieldStart := False
      } otherwise {
        when(gemIndex =/= 0 || allocLeft >= 5) { // idle GEM frames, zeros in a tail shorter than a header
          clear := B(gemMask, 40 bits).subdivideIn(8 bits)(U(4, 3 bits) - gemIndex)
          gemIndex := (gemIndex === 4) ? U(0, 3 bits) | gemIndex + 1
        }
      }
      when(allocLeft === 1) {
        when(io.job.valid && head.bit === nextBit) {
          io.job.ready := True
          startAlloc(head)
        } otherwise (state := IDLE)
      }
    }
  }
  when(state === ALLOC && (ploamLeft =/= 0 || dbruLeft =/= 0))(crc := crc8Step(crcIn, clear))
  scrambler := (state === PLOU && index === 0) ? scramblerByte(B"7'x7F")._2 | scramblerNext
  val keyNow = (state === PLOU && index === 0) ? scramblerByte(B"7'x7F")._1 | keystream
  when(scrambled)(bip := (state === PLOU && index === 0) ? B(0, 8 bits) | bip ^ clear)
  when(!io.enable)(state := IDLE)

  // grid bytes, then shifted onto the line by the burst's bit alignment
  val gridLine = RegNext(overheadByte ? ovLine | (scrambled ? (clear ^ keyNow) | B(0, 8 bits))) init (0)
  val gridBen = RegNext(overheadByte ? ovBen | B(8 bits, default -> scrambled)) init (0)
  val shiftOut = RegNextWhen(shift, state === OVERHEAD && index === 0) init (0)
  val previousLine = RegNext(gridLine) init (0)
  val previousBen = RegNext(gridBen) init (0)
  io.line := RegNext(((previousLine ## gridLine) >> shiftOut)(7 downto 0)) init (0)
  io.ben := RegNext(((previousBen ## gridBen) >> shiftOut)(7 downto 0)) init (0)
}
