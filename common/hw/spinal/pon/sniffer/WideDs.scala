// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.lib._
import pon.gpon._
import pon.gpon.GponCodes._

/** A GEM header with a good HEC and a payload, and the crypto-counter at its first byte. */
case class WideGemHeader() extends Bundle {
  val portId, pli = UInt(12 bits)
  val pti = Bits(3 bits)
  val counter = UInt(46 bits)
}

/** Payload bytes of the open GEM fragment in one word: `count` bytes from byte `first` (byte 0 in bits 31-24). */
case class GemChunk() extends Bundle {
  val bytes = Bits(32 bits)
  val first = UInt(2 bits)
  val count = UInt(3 bits)
  val last = Bool() // the fragment ends with this chunk
  val cut = Bool()  // by the frame end, short of its PLI
}

/** What one word gives: a header that completed in it, then payload (that header's fragment when both). */
case class GemItem() extends Bundle {
  val header = Flow(WideGemHeader())
  val chunk = Flow(GemChunk())
}

/** Downstream GTC deframer for a slow fabric: 4 line bytes per clock (77.76 MHz) where GtcDsDeframer takes one
  * (311.04 MHz), same rules (PSync hunt on any bit alignment, M1 = 2, M2 = 5, PLend twice, CRC and HEC detect only).
  * For a sniffer: FEC parity is skipped, not decoded, no BIP count, and a bad GEM header ends the frame's GEM
  * (no hunt inside the frame). Stages: align, descramble, drop the parity and repack to 4 data bytes, fields; the
  * GEM walk two clocks behind the fields, so PLend is known when the payload starts in its own word. */
case class WideDsDeframer() extends Component {
  val io = new Bundle {
    val word = in Bits (32 bits) // line bits, first in bit 31
    val synced = out Bool ()
    val fecIndication = out Bool ()
    val ploam = master Flow (Checked(Bits(96 bits)))
    val alloc = master Flow (Checked(Allocation()))
    val gem = out(GemItem())
    val plendErrors, hecErrors, syncLosses = out UInt (32 bits)
  }
  val frameWords = frameBytes / 4
  val fecWords = (frameBytes / 255 * fecDataBytes + frameBytes % 255 - 16) / 4 // data words of a frame with FEC
  def crc8(bits: Bits, from: Bits = B(0, 8 bits)): Bits = bits.subdivideIn(8 bits).reverse.foldLeft(from)(crc8Step)
  val fecIndication = Reg(Bool()) init (False)
  io.fecIndication := fecIndication

  // PSync on the 32 alignments of the last two words; `current` is what `found` saw
  val older = RegNext(io.word) init (0)
  val window = RegNext(older) ## older
  val found = RegNext(Vec((0 until 32).map(a => window(63 - a downto 32 - a) === B(psync, 32 bits))).asBits) init (0)
  val current = RegNext(window)
  val alignment = Reg(UInt(5 bits)) init (0)
  val sync = RegInit(FrameSync.HUNT)
  val count = Reg(UInt(3 bits)) // PSync found in a row (PRESYNC) or missed in a row (SYNC)
  val position = Reg(UInt(14 bits)) // frame word of `current`
  val syncLosses = Reg(UInt(32 bits)) init (0)
  io.synced := sync === FrameSync.SYNC
  io.syncLosses := syncLosses
  when(sync === FrameSync.HUNT) {
    when(found.orR) {
      alignment := OHToUInt(OHMasking.first(found))
      position := 1
      count := 1
      sync := FrameSync.PRESYNC
    }
  } otherwise {
    position := (position === frameWords - 1) ? U(0, 14 bits) | position + 1
    when(position === 0) {
      val ok = found(alignment)
      when(sync === FrameSync.PRESYNC) {
        count := count + 1
        when(!ok)(sync := FrameSync.HUNT) elsewhen (count === 1)(sync := FrameSync.SYNC)
        when(ok && count === 1)(count := 0)
      } otherwise {
        count := ok ? U(0, 3 bits) | count + 1
        when(!ok && count === 4) {
          sync := FrameSync.HUNT
          syncLosses := syncLosses + 1
        }
      }
    }
  }

  val aligned = new Area {
    val word = RegNext((current |<< alignment)(63 downto 32))
    val position = RegNext(WideDsDeframer.this.position)
    val start = RegNext(WideDsDeframer.this.position === 0)
    val tail = RegNext(WideDsDeframer.this.position >= fecLastData / 4) // the last codeword's parity
    val run = RegNext(sync =/= FrameSync.HUNT) init (False)
    val active = RegNext(sync === FrameSync.SYNC) init (False)
  }

  val clear = new Area {
    val scrambler = Reg(Bits(7 bits))
    var state = (aligned.position === 1) ? B"7'x7F" | scrambler
    val keystream = Cat((0 until 4).map { _ => val (k, n) = scramblerByte(state); state = n; k }.reverse)
    scrambler := state
    val word = RegNext(aligned.start ? aligned.word | aligned.word ^ keystream)
    val start = RegNext(aligned.start)
    val run = RegNext(aligned.run) init (False)
    val active = RegNext(aligned.active) init (False)
    // the word's parity bytes, a stage ahead of their removal (timing)
    val codeword = Reg(UInt(8 bits)) // codeword position of the word's first byte
    val first = aligned.start ? U(0, 8 bits) | codeword
    val next = first +^ 4
    // byte k is at first + k, or 255 less in the next codeword (data again)
    val parityNow = Vec((0 until 4).map(k => fecIndication && ((first >= fecDataBytes - k && first < 255 - k) || aligned.tail)))
    // the frame PSync is found in runs from its word 1: as after a word 0
    codeword := aligned.run ? ((next >= 255) ? (next - 255) | next).resize(8) | U(4, 8 bits)
    val parity = RegNext(parityNow)
    val keep = RegNext(CountOne(parityNow.map(!_)))
  }

  // parity bytes out (a run of 16: the data bytes of a word are its first or its last), the rest packed to words
  val packed = new Area {
    val (start, parity, keep) = (clear.start, clear.parity, clear.keep)
    val mask = Cat((0 until 4).map(k => B(8 bits, default -> (keep > k))).reverse)
    val fresh = (parity(0) ? (clear.word |<< ((U(4, 3 bits) - keep) << 3)) | clear.word) & mask
    val residue = Reg(Bits(24 bits))
    val held = Reg(UInt(2 bits)) init (0)
    val heldNow = start ? U(0, 2 bits) | held
    val joined = ((start ? B(0, 24 bits) | residue) ## B(0, 32 bits)) | ((fresh ## B(0, 24 bits)) |>> (heldNow << 3))
    val total = heldNow +^ keep
    val full = total >= 4
    val emitted = Reg(UInt(14 bits))
    val indexNow = start ? U(0, 14 bits) | emitted
    // line position of the packed bytes, for the crypto-counter: the parity bytes before, and where the codeword turns
    val dataCodeword = Reg(UInt(8 bits))
    val parityBefore = Reg(UInt(12 bits))
    val dataNow = start ? U(0, 8 bits) | dataCodeword
    val parityNow = start ? U(0, 12 bits) | parityBefore
    val turn = fecIndication && dataNow +^ 4 >= fecDataBytes
    when(clear.run) {
      residue := full ? joined(23 downto 0) | joined(55 downto 32)
      held := (full ? (total - 4) | total).resized
      when(full) {
        emitted := indexNow + 1
        dataCodeword := turn ? (dataNow + 4 - fecDataBytes) | dataNow + 4
        parityBefore := parityNow + (turn ? U(16, 12 bits) | U(0, 12 bits))
      }
    } otherwise { // as clear.codeword
      held := 0
      residue := 0
      emitted := 1
      dataCodeword := 4
      parityBefore := 0
    }
    val valid = RegNext(clear.run && full) init (False)
    val word = RegNext(joined(55 downto 24))
    val index = RegNext(indexNow)
    val active = RegNext(clear.active) init (False)
    val codewordAt = RegNext(dataNow)
    val parityAt = RegNext(parityNow)
  }

  val fields = new Area {
    val (valid, word, index, active) = (packed.valid, packed.word, packed.index, packed.active)
    val h1, h2, h3 = Reg(Bits(32 bits)) // the words before
    when(valid) {
      h1 := word
      h2 := h1
      h3 := h2
    }
    val superframe = Reg(UInt(30 bits)) init (0)
    val lastIdent = Reg(UInt(30 bits)) init (0)
    val fecLast = Reg(Bool()) init (False) // the last frame's Ident FEC bit
    when(valid && active && index === 1) {
      val ident = word(29 downto 0).asUInt
      lastIdent := ident
      superframe := (ident === lastIdent + 1) ? ident | superframe + 1
    }
    when(valid && index === 1) { // from PRESYNC on: FEC is known in the first frame in SYNC
      fecLast := word(31)
      when(word(31) === fecLast)(fecIndication := word(31))
    }

    val ploam = Flow(Checked(Bits(96 bits)))
    val ploamCrc = Reg(Bits(8 bits)) // over words 2 to 4, a word per clock (timing)
    when(valid)(ploamCrc := crc8(word, (index === 2) ? B(0, 8 bits) | ploamCrc))
    ploam.valid := valid && active && index === 5
    ploam.value := h3 ## h2 ## h1
    ploam.crcOk := ploamCrc === word(31 downto 24)
    io.ploam << ploam.stage()

    // PLend twice, bytes 22-25 and 26-29; the BWmap from byte 30, the GEM payload after it
    val frameOk = Reg(Bool()) init (False)
    val gemStart = Reg(UInt(14 bits)) // the payload starts at byte 2 of this word
    val plendError = RegNext(False) init (False)
    val plendErrors = Reg(UInt(32 bits)) init (0)
    plendErrors := plendErrors + plendError.asUInt
    io.plendErrors := plendErrors
    when(valid && index === 7) {
      val a = h2(15 downto 0) ## h1(31 downto 24)
      val b = h1(15 downto 0) ## word(31 downto 24)
      val aOk = crc8(a) === h1(23 downto 16)
      val bOk = crc8(b) === word(23 downto 16)
      val usable = (aOk && bOk) ? (a === b) | (aOk || bOk)
      val plend = aOk ? a | b
      frameOk := active && usable
      gemStart := U(7, 14 bits) + (plend(23 downto 12).asUInt << 1).resized
      plendError := active && !usable
    }

    val entry = h2(15 downto 0) ## h1 ## word(31 downto 24)
    val alloc = Flow(Checked(Allocation()))
    alloc.valid := valid && frameOk && index(0) && index >= 9 && index <= gemStart
    alloc.value := Allocation.fromLine(entry)
    alloc.crcOk := crc8(entry) === word(23 downto 16)
    io.alloc << alloc.stage()
  }

  val gem = new Area {
    val valid = Delay(packed.valid, 2, init = False)
    val word = Delay(packed.word, 2)
    val index = Delay(packed.index, 2)
    val codewordAt = Delay(packed.codewordAt, 2)
    val parityAt = Delay(packed.parityAt, 2)
    // two clocks behind: from word 7 on, the fields' PLend is this frame's
    val (okNow, startNow) = (fields.frameOk, fields.gemStart)
    val isLast = index === (fecIndication ? U(fecWords - 1, 14 bits) | U(frameWords - 1, 14 bits))

    val walking = Reg(Bool()) init (False)
    val skip = Reg(UInt(13 bits))    // bytes from this word's byte 0 to the next header
    val payStart = Reg(UInt(3 bits)) // the open fragment's payload starts at this byte (after its header's tail)
    val open = Reg(Bool()) init (False)
    val late = Reg(Bool()) init (False) // the header began in the last byte of the word before: its PLI ends here
    val pliHigh = Reg(Bits(8 bits))
    val pending = Reg(Bool()) init (False) // a header began in the word before
    val pendingAt = Reg(UInt(2 bits))
    val pendingWord = Reg(Bits(32 bits))
    val pendingCounter = Reg(UInt(46 bits))

    val begin = okNow && index === startNow
    val on = walking || begin
    val skipNow = begin ? U(2, 13 bits) | skip
    val payNow = begin ? U(4, 3 bits) | payStart
    val openNow = open && !begin
    val lateNow = late && !begin
    val headerHere = on && !lateNow && skipNow < 4 && !isLast // a header in the frame's last word has no end
    val at = skipNow(1 downto 0)
    val pliMask = B((gemMask >> 28).toInt, 12 bits)
    val pliHere = (at.mux(0 -> word(31 downto 20), 1 -> word(23 downto 12), default -> word(15 downto 4)) ^ pliMask).asUInt
    val pliLate = ((pliHigh ## word(31 downto 28)) ^ pliMask).asUInt
    val ends = !lateNow && skipNow <= 4 // the open fragment's last byte is in this word
    val stop = ends ? skipNow(2 downto 0) | U(4, 3 bits)
    val cut = isLast && !ends
    def linePos(k: UInt): UInt = (index @@ k).resize(16) + parityAt +
      ((fecIndication && codewordAt +^ k >= fecDataBytes) ? U(16, 16 bits) | U(0, 16 bits))

    val chunk = Flow(GemChunk())
    val voidEnd = lateNow && isLast && pliLate =/= 0 // its header ends the frame: an empty cut fragment
    chunk.valid := valid && on && ((openNow && (stop > payNow || cut)) || voidEnd)
    chunk.bytes := word
    chunk.first := payNow.resized
    chunk.count := (stop > payNow) ? (stop - payNow) | U(0, 3 bits)
    chunk.last := ends || cut
    chunk.cut := cut

    when(valid) {
      when(index === 0) {
        walking := False
        open := False
        late := False
        pending := False
      } elsewhen (on) {
        walking := !isLast
        payStart := 0
        pending := False
        late := False
        when(lateNow) {
          skip := pliLate.resized
          open := pliLate =/= 0
        } elsewhen (headerHere) {
          pending := True
          pendingAt := at
          pendingWord := word
          pendingCounter := fields.superframe @@ (linePos(at) >> 2).resize(16)
          when(at === 3) {
            late := True
            pliHigh := word(7 downto 0)
            open := False
          } otherwise {
            skip := (at +^ 1) +^ pliHere
            open := pliHere =/= 0
            payStart := (at +^ 1).resized
          }
        } otherwise {
          skip := skipNow - 4
          when(ends)(open := False)
        }
        when(isLast)(open := False)
      }
    }

    // the header, whole in the word after its first byte; its HEC a clock later
    val header = pendingAt.mux(
      0 -> (pendingWord ## word(31 downto 24)),
      1 -> (pendingWord(23 downto 0) ## word(31 downto 16)),
      2 -> (pendingWord(15 downto 0) ## word(31 downto 8)),
      3 -> (pendingWord(7 downto 0) ## word)) ^ B(gemMask, 40 bits)
    val checked = RegNext(header)
    val checking = RegNext(valid && pending && walking) init (False)
    val counter = RegNext(pendingCounter)
    val headerOk = gemHeaderValid(checked)
    val killed = Reg(Bool()) init (False) // a bad header: nothing more from this frame
    val bad = checking && !headerOk && !killed
    when(valid && index === 0)(killed := False)
    when(bad) {
      killed := True
      walking := False
    }
    val hecErrors = Reg(UInt(32 bits)) init (0)
    hecErrors := hecErrors + (RegNext(bad) init (False)).asUInt
    io.hecErrors := hecErrors

    val event = Flow(WideGemHeader())
    event.valid := checking && headerOk && !killed && checked(39 downto 28) =/= 0
    event.pli := checked(39 downto 28).asUInt
    event.portId := checked(27 downto 16).asUInt
    event.pti := checked(15 downto 13)
    event.counter := counter
    io.gem.header << event.stage()
    io.gem.chunk << chunk.stage().takeWhen(!bad && !killed).stage()
  }
}

/** The records of one ONU from a WideDsDeframer: SnifferDs's, same formats. GEM fragments of the Port-IDs set in the
  * port map; a fragment is taken whole or not at all (room for it in the chunk FIFO, else dropped and counted), then
  * its bytes go one per clock to the record writer. */
case class WideSnifferDs(gemDepth: Int = 8192, chunkDepth: Int = 2048) extends Component {
  val io = new Bundle {
    val ploam = slave Flow (Checked(Bits(96 bits)))
    val alloc = slave Flow (Checked(Allocation()))
    val gem = in(GemItem())
    val time = in UInt (64 bits)
    val onuId = in Bits (8 bits)
    val allPloams = in Bool ()
    val allocMap = slave Flow (KeepWrite())
    val portMap = slave Flow (KeepWrite())
    val output = master Stream (Fragment(Bits(8 bits)))
    val drops = out UInt (32 bits)
    val truncations = out UInt (32 bits)
  }
  val pcbd = SnifferPcbd()
  pcbd.io.ploam := io.ploam
  pcbd.io.alloc := io.alloc
  pcbd.io.time := io.time
  pcbd.io.onuId := io.onuId
  pcbd.io.allPloams := io.allPloams
  pcbd.io.allocMap << io.allocMap

  // the map answers a clock after the header; the item waits two
  val portMap = Mem(Bool(), 4096) init (Seq.fill(4096)(False))
  portMap.write(io.portMap.id, io.portMap.keep, io.portMap.valid)
  val hit = RegNext(portMap.readSync(io.gem.header.portId))
  val header = io.gem.header.stage().stage()
  val chunk = io.gem.chunk.stage().stage()
  val chunks = StreamFifo(GemChunk(), chunkDepth)
  val heads = StreamFifo(RecordHead(10), 16)
  val room = chunks.io.availability > ((header.pli +^ 3) >> 2) + 2 && heads.io.availability > 1
  val take = hit && room
  val keeping = Reg(Bool()) init (False)
  val meta = Reg(WideGemHeader())
  val start = Reg(UInt(64 bits))
  val keepNow = header.valid ? take | keeping
  val metaNow = header.valid ? header.payload | meta
  val startNow = header.valid ? io.time | start
  val gemDrops = Reg(UInt(32 bits)) init (0)
  when(header.valid) {
    keeping := take
    meta := header.payload
    start := io.time
    when(hit && !room)(gemDrops := gemDrops + 1)
  }
  when(chunk.valid && chunk.last)(keeping := False)
  chunks.io.push.valid := chunk.valid && keepNow
  chunks.io.push.payload := chunk.payload
  heads.io.push.valid := chunk.valid && keepNow && chunk.last
  heads.io.push.payload := GemRecord.head(metaNow.portId, metaNow.pli, metaNow.counter, metaNow.pti, chunk.cut, startNow)

  val gems = RecordWriter(10, gemDepth, 32)
  val popped = chunks.io.pop.stage() // registered: the queues' memories are slow to read
  val head = heads.io.pop.stage()
  val byteIndex = Reg(UInt(3 bits)) init (0)
  val lastByte = byteIndex + 1 >= popped.count
  val finish = lastByte && (!popped.last || head.valid) // a fragment's last byte waits for its head
  val data = Flow(Bits(8 bits))
  val close = Flow(RecordHead(10))
  data.valid := popped.valid && popped.count =/= 0 && (!lastByte || finish)
  data.payload := Vec(popped.bytes.subdivideIn(8 bits).reverse)((popped.first + byteIndex).resize(2))
  close.valid := popped.valid && finish && popped.last
  close.payload := head.payload
  popped.ready := finish
  head.ready := close.valid
  gems.io.data << data.stage() // both a clock later: the queue's memory is slow to read
  gems.io.close << close.stage()
  when(popped.valid)(byteIndex := finish ? U(0, 3 bits) | (lastByte ? byteIndex | byteIndex + 1))

  io.output << StreamArbiterFactory.roundRobin.fragmentLock.on(pcbd.io.outputs ++ Seq(gems.io.output))
  io.drops := pcbd.io.drops + gems.io.drops + gemDrops
  io.truncations := pcbd.io.truncations + gems.io.truncations
}
