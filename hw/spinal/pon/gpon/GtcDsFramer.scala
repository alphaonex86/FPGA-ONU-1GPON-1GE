// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.lib._
import GponCodes._

case class Allocation() extends Bundle {
  val allocId = UInt(12 bits)
  val flags = Bits(12 bits)
  val start = UInt(16 bits)
  val stop = UInt(16 bits)
  def line: Bits = allocId.asBits ## flags ## start.asBits ## stop.asBits // order on the line
}

object Allocation {
  def fromLine(bits: Bits): Allocation = {
    val a = Allocation()
    a.allocId := bits(55 downto 44).asUInt
    a.flags := bits(43 downto 32)
    a.start := bits(31 downto 16).asUInt
    a.stop := bits(15 downto 0).asUInt
    a
  }
}

case class GemTxByte() extends Bundle {
  val portId = UInt(12 bits) // read on the first byte of a packet
  val length = UInt(16 bits) // packet length in bytes, read on the first byte
  val data = Bits(8 bits)
}

case class BwmapWrite(maxAllocs: Int) extends Bundle {
  val index = UInt(log2Up(maxAllocs) bits)
  val alloc = Allocation()
}

/** OLT downstream GTC framer (G.984.3 clause 8.1), no FEC, one line byte per clock (311.04 MHz).
  * BWmap: write the entries of the next frame, then commit their count before its first byte.
  * A GEM packet must deliver one byte per clock once started; a gap or a wrong length raises gemError. */
case class GtcDsFramer(maxAllocs: Int = 64, superframeReset: BigInt = 0) extends Component {
  val io = new Bundle {
    val ploam = slave Stream (Bits(96 bits)) // ONU-ID, Message-ID, 10 data bytes; the CRC is added here
    val bwmapWrite = slave Flow (BwmapWrite(maxAllocs))
    val bwmapCommit = slave Flow (UInt(log2Up(maxAllocs + 1) bits))
    val gem = slave Stream (Fragment(GemTxByte()))
    val line = out Bits (8 bits)
    val frameStart = out Bool () // line carries the first PSync byte
    val gemError = out Bool ()
  }

  val pos = Reg(UInt(16 bits)) init (0)
  val lastByte = RegNext(pos === frameBytes - 2) init (False) // pos === frameBytes - 1, registered (timing)
  pos := lastByte ? U(0, 16 bits) | pos + 1
  // GEM payload room after a header started here: frameBytes - pos - 5, kept in registers (timing)
  val room = Reg(UInt(16 bits)) init (frameBytes - 5)
  val hasRoom = Reg(Bool()) init (True)
  val fragmentRoom = Reg(UInt(12 bits)) init (4095) // min(room, 4095)
  val roomNext = lastByte ? U(frameBytes - 5, 16 bits) | room - 1
  room := roomNext
  hasRoom := lastByte || (hasRoom && room =/= 1)
  fragmentRoom := (roomNext < 4095) ? roomNext.resize(12) | U(4095, 12 bits)
  val superframe = Reg(UInt(30 bits)) init (U(superframeReset, 30 bits))
  when(lastByte)(superframe := superframe + 1)

  // BWmap double buffer, swapped at the frame start
  val bwmap = Mem(Bits(56 bits), 2 * maxAllocs)
  val front = Reg(Bool()) init (False)
  val allocCount = Reg(UInt(log2Up(maxAllocs + 1) bits)) init (0)
  val committed = Reg(Bool()) init (False)
  val committedCount = Reg(cloneOf(allocCount))
  bwmap.write(((!front) ## io.bwmapWrite.index).asUInt, io.bwmapWrite.alloc.line, io.bwmapWrite.valid)
  when(pos === 0) {
    front := front ^ committed
    allocCount := committed ? committedCount | allocCount.getZero
    committed := False
  }
  when(io.bwmapCommit.valid) {
    committed := True
    committedCount := io.bwmapCommit.payload
  }
  val pcbdLast = RegNext(U(29, 16 bits) + (allocCount << 3)) // last PCBd byte, valid from pos 2
  val inPayload = Reg(Bool()) init (False)
  inPayload := !lastByte && (inPayload || pos === pcbdLast)
  val bwOffset = pos - 30
  val bwEntry = bwmap.readSync((front ## ((bwOffset + 1) >> 3).resize(log2Up(maxAllocs))).asUInt)
  val bwByte = bwOffset(2 downto 0)

  // PLOAM latched just before its field
  val ploam = Reg(Bits(96 bits))
  io.ploam.ready := pos === 7
  when(pos === 7)(ploam := io.ploam.valid ? io.ploam.payload | B(noMessage, 96 bits))

  // GEM encapsulation state; the HEC is computed from registers after the first header byte (timing)
  val gemHeaderLeft = Reg(UInt(3 bits)) init (0)
  val headerPli = Reg(UInt(12 bits))
  val headerPort = Reg(UInt(12 bits))
  val headerPti = Reg(Bits(3 bits))
  val fragmentLeft = Reg(UInt(12 bits)) init (0)
  val packetLeft = Reg(UInt(16 bits)) init (0)
  val portId = Reg(UInt(12 bits))
  io.gem.ready := False
  io.gemError := False

  val field = Bits(8 bits) // stage 1: field byte; CRC and BIP slots are filled in stage 2
  val crcStart, crcEmit = False

  def byteOf(word: Bits, index: UInt): Bits = // byte `index` of `word`, MSB first
    (word >> ((U(word.getWidth / 8 - 1, index.getWidth bits) - index) << 3))(7 downto 0)

  when(pos < 4) {
    field := byteOf(B(psync, 32 bits), pos.resize(2))
  } elsewhen (pos < 8) {
    field := byteOf(B"00" ## superframe, (pos - 4).resize(2))
  } elsewhen (pos < 21) {
    crcStart := pos === 8
    crcEmit := pos === 20
    field := byteOf(ploam, (pos - 8).resize(4))
  } elsewhen (pos === 21) {
    field := B(0, 8 bits) // BIP, inserted in stage 2
  } elsewhen (pos < 30) {
    val i = (pos - 22)(1 downto 0)
    crcStart := i === 0
    crcEmit := i === 3
    field := byteOf(allocCount.resize(12).asBits ## B(0, 12 bits) ## B(0, 8 bits), i)
  } elsewhen (!inPayload) {
    crcStart := bwByte === 0
    crcEmit := bwByte === 7
    field := byteOf(bwEntry ## B(0, 8 bits), bwByte)
  } elsewhen (gemHeaderLeft =/= 0) {
    val line = gemHeader(headerPli, headerPort, headerPti) ^ B(gemMask, 40 bits)
    field := line.subdivideIn(8 bits)(gemHeaderLeft - 1)
    gemHeaderLeft := gemHeaderLeft - 1
  } elsewhen (fragmentLeft =/= 0) {
    io.gem.ready := True
    field := io.gem.fragment.data
    fragmentLeft := fragmentLeft - 1
    packetLeft := packetLeft - 1
    io.gemError := !io.gem.valid || (io.gem.last =/= (packetLeft === 1))
  } otherwise {
    // start a GEM frame: data fragment if a packet is pending and fits, else idle
    val continuing = packetLeft =/= 0
    val length = continuing ? packetLeft | io.gem.fragment.length
    val port = continuing ? portId | io.gem.fragment.portId
    // the whole rest fits in this fragment; both cases compared in parallel (timing)
    val endOfFrame = continuing ? (packetLeft <= fragmentRoom) | (io.gem.fragment.length <= fragmentRoom)
    val fragment = endOfFrame ? length.resize(12) | fragmentRoom
    val sendData = hasRoom && (continuing || io.gem.valid) && length =/= 0
    // idle GEM frame: all-zero header
    headerPli := sendData ? fragment | U(0, 12 bits)
    headerPort := sendData ? port | U(0, 12 bits)
    headerPti := sendData ? (B"00" ## endOfFrame) | B"000"
    field := (sendData ? fragment(11 downto 4).asBits | B(0, 8 bits)) ^ B(gemMask >> 32, 8 bits)
    gemHeaderLeft := 4
    when(sendData) {
      fragmentLeft := fragment
      packetLeft := length
      portId := port
    }
    when(!continuing && io.gem.valid && length === 0) { // invalid packet: drop it, report
      io.gem.ready := True
      io.gemError := True
    }
  }

  when(lastByte) { // a GEM frame never straddles two GTC frames
    gemHeaderLeft := 0
    fragmentLeft := 0
  }

  // stage 2 (registered for timing): CRC and BIP insertion, scrambling
  val fieldByte = RegNext(field) init (0)
  val crcStartSlot = RegNext(crcStart) init (False)
  val crcSlot = RegNext(crcEmit) init (False)
  val bipSlot = RegNext(pos === 21) init (False)
  val scramblerReset = RegNext(pos === 4) init (False)
  val unscrambled = RegNext(pos < 4) init (True)
  val frameStart = RegNext(pos === 0) init (False)

  val crc = Reg(Bits(8 bits))
  val crcIn = crcStartSlot ? B(0, 8 bits) | crc
  val bip = Reg(Bits(8 bits)) init (0)
  val clear = crcSlot ? crcIn | (bipSlot ? bip | fieldByte)
  crc := crc8Step(crcIn, clear)
  bip := bipSlot ? B(0, 8 bits) | bip ^ clear

  val scrambler = Reg(Bits(7 bits))
  val (keystream, scramblerNext) = scramblerByte(scramblerReset ? B"7'x7F" | scrambler)
  scrambler := scramblerNext
  io.line := RegNext(unscrambled ? clear | clear ^ keystream) init (0)
  io.frameStart := RegNext(frameStart) init (False)
}
