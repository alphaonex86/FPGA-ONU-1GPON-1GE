// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.lib._
import GponCodes._

object FramerState extends SpinalEnum { val IDLE, OVERHEAD, PLOU, ALLOC = newElement() }

/** ONU upstream GTC burst framer (G.984.3 clause 8.2), one line byte per clock (155.52 MHz). Jobs come in
  * time order; a job starting where the previous one ends continues its burst, otherwise a burst opens with guard,
  * preamble, delimiter and PLOu. Allocation content: PLOAMu, DBRu (empty report), then GEM frames: in the gemAllocId
  * allocations the packets of io.gem (one byte per clock once started), fragmented to fit and continued in the next
  * such allocation, else idle. A packet left open when the bursts stop is discarded. The line leaves on the jobs' bit
  * alignment with its burst enable (laser on), a fixed latency after io.time. FEC (clause 13.3) when the burst's first
  * allocation asks for it (Use_FEC, not on serial number and ranging grants): codewords from the BIP byte, 16 parity
  * bytes after every 239 and at the end of the burst (shorter last codeword), the content sized to the data bytes. */
case class GtcUsFramer() extends Component {
  val io = new Bundle {
    val job = slave Stream (UsJob())
    val time = in UInt (24 bits)      // ONU byte time
    val enable = in Bool ()           // low: no burst, the current one stops at once, jobs are dropped
    val onuId = in Bits (8 bits)
    val overhead = in(BurstOverhead())
    val snMessage = in Bits (96 bits) // Serial_Number_ONU with a zero random delay field
    val ploam = slave Stream (Bits(96 bits)) // other PLOAMu messages, No_message when none
    val gem = slave Stream (Fragment(GemTxByte()))
    val gemAllocId = in UInt (12 bits)   // the allocations carrying io.gem (OMCC: the default Alloc-ID)
    val gemError = out Bool ()           // io.gem broke a packet (gap, wrong last, zero length)
    val line = out Bits (8 bits)
    val ben = out Bits (8 bits)       // burst enable per line bit
    val late = out Bool ()            // a job whose burst start had passed was dropped
  }
  import FramerState._

  def after(a: UInt, b: UInt): Bool = !(a - b).msb
  val jobs = io.job.m2sPipe() // io.job: the job after this one, to know the burst's end when an allocation starts
  val head = jobs.payload
  val burstStart = (head.bit >> 3).resize(24) - 3 - io.overhead.bytes
  jobs.ready := False
  io.ploam.ready := False
  io.late := False
  io.gem.ready := False
  io.gemError := False

  val state = RegInit(IDLE)
  val shift = Reg(UInt(3 bits)) init (0)
  val index = Reg(UInt(8 bits))
  val allocLeft = Reg(UInt(16 bits))   // data bytes of the allocation left
  val encodedLeft = Reg(UInt(16 bits)) // line bytes of the allocation left (parity included)
  val fec = Reg(Bool())
  val codewordPos = Reg(UInt(8 bits))  // of the current byte, from the BIP byte
  val nextBit = Reg(UInt(27 bits)) // where a contiguous job starts
  val ploamLeft = Reg(UInt(4 bits))
  val dbruLeft = Reg(UInt(2 bits))
  val gemIndex = Reg(UInt(3 bits)) init (0) // header byte being sent
  val headerPli, headerPort = Reg(UInt(12 bits))
  val headerPti = Reg(Bits(3 bits))
  val fragmentLeft = Reg(UInt(12 bits)) init (0)
  val packetLeft = Reg(UInt(12 bits)) init (0)
  val portId = Reg(UInt(12 bits))
  val gemAlloc = Reg(Bool())
  val message = Reg(Bits(96 bits))
  val fieldStart = Reg(Bool())
  val crc = Reg(Bits(8 bits))
  val crcIn = fieldStart ? B(0, 8 bits) | crc
  val bip = Reg(Bits(8 bits)) init (0) // since the BIP field of our last burst
  val scrambler = Reg(Bits(7 bits))
  val (keystream, scramblerNext) = scramblerByte(scrambler)

  def div255(x: UInt): UInt = ((x.resize(17) + (x >> 8) + 1) >> 8).resize(16) // x < 65535
  // data bytes of an allocation starting at codeword position `at` (< 239): parity of each codeword started before its
  // end, or with the burst ending there of all its codewords, the last one 16 or all its bytes (fewer than 17: zeros)
  def dataBytes(at: UInt, bytes: UInt, last: Bool): UInt = {
    val end = at.resize(16) + bytes
    val full = div255(end - 1)
    val lastLength = end - (full << 8).resize(16) + full
    val parity = last ? ((full << 4).resize(16) + ((lastLength < 16) ? lastLength | U(16, 16 bits))) |
      (div255(end + 15) << 4).resize(16)
    (parity > bytes) ? U(0, 16 bits) | bytes - parity
  }
  def startAlloc(j: UsJob, burstFec: Bool): Unit = {
    val bytes = j.alloc.stop - j.alloc.start + 1
    val endBit = j.bit + (bytes << 3).resize(27)
    val last = !(io.job.valid && io.job.payload.bit === endBit)
    val at = (codewordPos === 254) ? U(0, 8 bits) | codewordPos + 1
    encodedLeft := bytes
    allocLeft := burstFec ? dataBytes(at, bytes, last) | bytes
    nextBit := endBit
    val ploamU = j.alloc.flags(10)
    ploamLeft := ploamU ? U(13, 4 bits) | U(0, 4 bits)
    dbruLeft := j.alloc.flags(8 downto 7).mux(1 -> U(2, 2 bits), 2 -> U(3, 2 bits), default -> U(0, 2 bits))
    gemAlloc := j.alloc.allocId === io.gemAllocId && !j.sn
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

  val contiguous = jobs.valid && head.bit === nextBit
  val parity = state === ALLOC && fec && (codewordPos >= fecDataBytes || !contiguous && encodedLeft <= 16)
  // RS(255,239) encoder as downstream (G.984.3 A.3), restarted at each codeword
  val rs = Vec(Reg(Bits(8 bits)) init (0), 16)
  val rsState = (codewordPos === 0) ? Vec(rs.map(_ => B(0, 8 bits))) | rs
  val clear = Bits(8 bits)
  clear := 0
  val scrambled, overheadByte = False
  switch(state) {
    is(IDLE) {
      when(!io.enable) {
        jobs.ready := jobs.valid
      } elsewhen (jobs.valid) {
        when(io.time === burstStart) {
          state := OVERHEAD
          index := 0
          shift := head.bit(2 downto 0)
        } elsewhen (after(io.time, burstStart)) {
          jobs.ready := True
          io.late := True
        }
      }
    }
    is(OVERHEAD) {
      overheadByte := True
      index := index + 1
      when(index === ov.bytes - 1) { state := PLOU; index := 0; codewordPos := 0 }
    }
    is(PLOU) {
      scrambled := True
      index := index + 1
      switch(index(1 downto 0)) {
        is(0)(clear := bip)
        is(1)(clear := io.onuId)
        default {
          val burstFec = head.alloc.flags(9) && !head.sn
          clear := io.ploam.valid ## burstFec ## B(0, 6 bits) // Ind: PLOAMu waiting, FEC
          fec := burstFec
          jobs.ready := True
          startAlloc(head, burstFec)
          state := ALLOC
        }
      }
    }
    is(ALLOC) {
      scrambled := True
      encodedLeft := encodedLeft - 1
      when(!parity && allocLeft =/= 0)(allocLeft := allocLeft - 1)
      when(parity || allocLeft === 0) {
        // parity (below), or data bytes beyond the content (an allocation breaking the FEC pointer rules): zeros
      } elsewhen (ploamLeft =/= 0) {
        ploamLeft := ploamLeft - 1
        clear := (ploamLeft === 1) ? crcIn | message(95 downto 88)
        message := message |<< 8
        fieldStart := ploamLeft === 1 // a DBRu may follow
      } elsewhen (dbruLeft =/= 0) {
        dbruLeft := dbruLeft - 1
        clear := (dbruLeft === 1) ? crcIn | B(0, 8 bits)
        fieldStart := False
      } elsewhen (gemIndex =/= 0) { // header bytes 1-4, the HEC from registers (timing)
        val line = gemHeader(headerPli, headerPort, headerPti) ^ B(gemMask, 40 bits)
        clear := line.subdivideIn(8 bits)(U(4, 3 bits) - gemIndex)
        gemIndex := (gemIndex === 4) ? U(0, 3 bits) | gemIndex + 1
      } elsewhen (fragmentLeft =/= 0) {
        io.gem.ready := True
        clear := io.gem.fragment.data
        fragmentLeft := fragmentLeft - 1
        packetLeft := packetLeft - 1
        io.gemError := !io.gem.valid || (io.gem.last =/= (packetLeft === 1))
      } elsewhen (allocLeft >= 5) { // a GEM frame: data if a packet is pending and a byte of it fits, else idle
        val continuing = packetLeft =/= 0
        val length = continuing ? packetLeft | io.gem.fragment.length.resize(12)
        val port = continuing ? portId | io.gem.fragment.portId
        val room = allocLeft - 5
        val endOfPacket = length.resize(16) <= room
        val fragment = endOfPacket ? length | ((room > 4095) ? U(4095, 12 bits) | room.resize(12))
        val sendData = gemAlloc && room =/= 0 && (continuing || io.gem.valid) && length =/= 0
        headerPli := sendData ? fragment | U(0, 12 bits)
        headerPort := sendData ? port | U(0, 12 bits)
        headerPti := sendData ? (B"00" ## endOfPacket) | B"000"
        clear := (sendData ? fragment(11 downto 4).asBits | B(0, 8 bits)) ^ B(gemMask >> 32, 8 bits)
        gemIndex := 1
        when(sendData) {
          fragmentLeft := fragment
          packetLeft := length
          portId := port
        }
        when(!continuing && io.gem.valid && length === 0) { // invalid packet: dropped, reported
          io.gem.ready := True
          io.gemError := True
        }
      } // else zeros: a tail shorter than a header
      when(encodedLeft === 1) {
        when(contiguous) {
          jobs.ready := True
          startAlloc(head, fec)
        } otherwise (state := IDLE)
      }
    }
  }
  when(parity)(clear := rsState(15))
  val feedback = clear ^ rsState(15)
  for (i <- 0 until 16) rs(i) := (if (i == 0) B(0, 8 bits) else rsState(i - 1)) ^
    (parity ? B(0, 8 bits) | Gf.mulConst(feedback, Model.rsGenerator(i)))
  when(scrambled)(codewordPos := (codewordPos === 254) ? U(0, 8 bits) | codewordPos + 1)
  when(state === ALLOC && !parity && (ploamLeft =/= 0 || dbruLeft =/= 0))(crc := crc8Step(crcIn, clear))
  scrambler := (state === PLOU && index === 0) ? scramblerByte(B"7'x7F")._2 | scramblerNext
  val keyNow = (state === PLOU && index === 0) ? scramblerByte(B"7'x7F")._1 | keystream
  when(scrambled && !parity)(bip := (state === PLOU && index === 0) ? B(0, 8 bits) | bip ^ clear)
  when(!io.enable) {
    state := IDLE
    gemIndex := 0
    fragmentLeft := 0
    when(packetLeft =/= 0 && io.gem.valid) { // the open packet is discarded
      io.gem.ready := True
      packetLeft := packetLeft - 1
    }
  }

  // grid bytes, then shifted onto the line by the burst's bit alignment
  val gridLine = RegNext(overheadByte ? ovLine | (scrambled ? (clear ^ keyNow) | B(0, 8 bits))) init (0)
  val gridBen = RegNext(overheadByte ? ovBen | B(8 bits, default -> scrambled)) init (0)
  val shiftOut = RegNextWhen(shift, state === OVERHEAD && index === 0) init (0)
  val previousLine = RegNext(gridLine) init (0)
  val previousBen = RegNext(gridBen) init (0)
  io.line := RegNext(((previousLine ## gridLine) >> shiftOut)(7 downto 0)) init (0)
  io.ben := RegNext(((previousBen ## gridBen) >> shiftOut)(7 downto 0)) init (0)
}
