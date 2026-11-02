// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.epon

import spinal.core._
import spinal.lib._
import pon.{CommaAligner, Crc32, Decoder8b10b, Encoder8b10b}

/** IEEE 802.3 clause 65 EPON: 1.25 Gbaud 8b/10b (clause 36 PCS), one byte per clock (125 MHz, a time quantum TQ of
  * 16 ns = 2 bytes); the preamble carries the LLID: /S/ 55 D5(SLD) 55 55 mode|LLID[14:8] LLID[7:0] CRC-8. */
object Epon {
  val broadcastLlid = 0x7FFF
  val (k28_5, kStart, kTerminate, kCarrier, kError) = (0xBC, 0xFB, 0xFD, 0xF7, 0xFE) // /K28.5/ /S/ /T/ /R/ /V/
  val (d5_6, d16_2) = (0xC5, 0x50) // second code group of /I1/ (flips RD+ to RD-) and /I2/
  val ipg = 12 // bytes from the FCS end to the next /S/

  /** CRC-8 of the preamble (x^8 + x^2 + x + 1, reflected, register initialised to 0, bit 0 first): over SLD to LLID. */
  def crc8(bytes: Seq[Bits]): Bits =
    bytes.foldLeft(B(0, 8 bits))((c, b) => (0 until 8).foldLeft(c ^ b)((x, _) => (x |>> 1) ^ (x(0) ? B(0xE0, 8 bits) | B(0, 8 bits))))
  def crc8Model(bytes: Seq[Int]): Int =
    bytes.foldLeft(0)((c, b) => (0 until 8).foldLeft(c ^ b)((x, _) => if ((x & 1) == 1) (x >> 1) ^ 0xE0 else x >> 1))
  def preamble(mode: Bool, llid: UInt): Seq[Bits] = {
    val covered = Seq(B(0xD5, 8 bits), B(0x55, 8 bits), B(0x55, 8 bits), mode ## llid(14 downto 8).asBits, llid(7 downto 0).asBits)
    Seq(B(0x55, 8 bits), B(0x55, 8 bits)) ++ covered :+ crc8(covered)
  }
}

case class EponTxByte() extends Bundle {
  val data = Bits(8 bits)
  val mode = Bool()      // read with the first byte
  val llid = UInt(15 bits)
}

case class EponRxByte() extends Bundle {
  val data = Bits(8 bits)
  val mode = Bool()
  val llid = UInt(15 bits)
}

/** EPON transmit: frames (DA to the payload end, 64 bytes or more with the FCS) get the LLID preamble and the FCS,
  * /T/R/ (and /R/ so IDLE starts even), at least Epon.ipg bytes before the next /S/; IDLE between (/I1/ or /I2/ by
  * the running disparity). Once started a frame must come one byte per clock: a missing byte is sent as /V/ and
  * pulses `underrun` (the frame then fails its FCS). `start` pulses with the frame's /S/. */
case class EponMacTx() extends Component {
  import Epon._
  val io = new Bundle {
    val input = slave Stream (Fragment(EponTxByte()))
    val code = out Bits (10 bits)
    val start, underrun = out Bool ()
  }
  object State extends SpinalEnum { val IDLE, PREAMBLE, DATA, FCS, END = newElement() }
  import State._
  val state = RegInit(IDLE)
  val even = RegInit(True)
  even := !even
  val index = Reg(UInt(3 bits)) init (0)
  val gap = Reg(UInt(4 bits)) init (ipg)
  val useI1 = Reg(Bool())
  val crc = Reg(Bits(32 bits))
  val header = Reg(EponTxByte())
  val encoder = Encoder8b10b()
  io.code := encoder.io.code
  val symbol = encoder.io.input
  symbol.k := True
  symbol.data := B(k28_5, 8 bits)
  io.input.ready := False
  io.start := False
  io.underrun := False
  when(state =/= IDLE && gap =/= 0)(gap := 0)
  switch(state) {
    is(IDLE) {
      when(gap =/= ipg)(gap := gap + 1)
      when(even) {
        useI1 := encoder.io.rd
        when(io.input.valid && gap === ipg) {
          symbol.data := B(kStart, 8 bits)
          header := io.input.payload.fragment
          state := PREAMBLE
          index := 1
          io.start := True
        }
      } otherwise {
        symbol.k := False
        symbol.data := useI1 ? B(d5_6, 8 bits) | B(d16_2, 8 bits)
      }
    }
    is(PREAMBLE) {
      symbol.k := False
      symbol.data := Vec(preamble(header.mode, header.llid))(index)
      index := index + 1
      when(index === 7) { state := DATA; crc.setAll() }
    }
    is(DATA) {
      io.input.ready := True
      symbol.k := !io.input.valid
      symbol.data := io.input.valid ? io.input.fragment.data | B(kError, 8 bits)
      io.underrun := !io.input.valid
      when(io.input.valid) {
        crc := Crc32.step(crc, io.input.fragment.data)
        when(io.input.last) { state := FCS; index := 0 }
      }
    }
    is(FCS) {
      symbol.k := False
      symbol.data := (~crc).subdivideIn(8 bits)(index(1 downto 0))
      index := index + 1
      when(index === 3) { state := END; index := 0 }
    }
    is(END) { // /T/ /R/, and /R/ while the next position is odd
      symbol.data := (index === 0) ? B(kTerminate, 8 bits) | B(kCarrier, 8 bits)
      index := index + 1
      gap := index.resize(4) + 1
      when(index =/= 0 && !even)(state := IDLE)
    }
  }
}

/** EPON receive: comma alignment, 8b/10b, frames from /S/ to /T/ with a valid preamble (SLD, CRC-8; else dropped,
  * `badPreamble`), each byte with its LLID and mode, DA to FCS; with the last byte `good`: FCS and every code group
  * valid. `synced`: 3 IDLEs in a row, lost after 4 invalid code groups without a comma between. */
case class EponMacRx() extends Component {
  import Epon._
  val io = new Bundle {
    val raw = in Bits (10 bits)
    val frame = master Flow (Fragment(EponRxByte()))
    val good = out Bool ()
    val synced, badPreamble = out Bool ()
  }
  val aligner = CommaAligner()
  aligner.io.raw := io.raw
  val decoder = Decoder8b10b()
  decoder.io.code := aligner.io.code
  val s = decoder.io.output
  val isComma = s.k && s.data === k28_5
  val synced = RegInit(False)
  val commas, errors = Reg(UInt(2 bits)) init (0)
  when(isComma) {
    errors := 0
    when(commas =/= 3)(commas := commas + 1)
    when(commas === 2)(synced := True)
  }
  when(decoder.io.error) {
    commas := 0
    errors := errors + 1
    when(errors === 3)(synced := False)
  }
  io.synced := synced

  object State extends SpinalEnum { val IDLE, PREAMBLE, DATA = newElement() }
  import State._
  val state = RegInit(IDLE)
  val index = Reg(UInt(3 bits))
  val bytes = Vec(Reg(Bits(8 bits)), 8) // preamble bytes 1-6
  val held = Reg(Bits(8 bits)) // the last byte, out once the next symbol tells whether it ends the frame
  val holding, bad = Reg(Bool())
  val crc = Reg(Bits(32 bits))
  val mode = Reg(Bool())
  val llid = Reg(UInt(15 bits))
  io.frame.valid := False
  io.frame.last := False
  io.frame.data := held
  io.frame.mode := mode
  io.frame.llid := llid
  io.good := !bad && crc === Crc32.residue && s.k && s.data === kTerminate && synced
  io.badPreamble := False
  switch(state) {
    is(IDLE) {
      when(synced && s.k && s.data === kStart) { state := PREAMBLE; index := 1 }
    }
    is(PREAMBLE) {
      bytes(index) := s.data
      index := index + 1
      when(s.k)(state := IDLE)
      when(index === 7 && !s.k) {
        val ok = bytes(2) === 0xD5 && crc8(Seq(bytes(2), bytes(3), bytes(4), bytes(5), bytes(6))) === s.data
        io.badPreamble := !ok
        state := ok ? DATA | IDLE
        mode := bytes(5)(7)
        llid := (bytes(5)(6 downto 0) ## bytes(6)).asUInt
        holding := False
        bad := False
        crc.setAll()
      }
    }
    is(DATA) {
      val ends = s.k && !decoder.io.error && s.data =/= kError || !synced // /T/ or any other control code
      io.frame.valid := holding && (ends || !s.k || decoder.io.error)
      io.frame.last := ends
      when(!ends) {
        held := s.data
        holding := True
        crc := Crc32.step(crc, s.data)
        when(decoder.io.error || s.k)(bad := True)
      }
      when(ends)(state := IDLE)
    }
  }
}
