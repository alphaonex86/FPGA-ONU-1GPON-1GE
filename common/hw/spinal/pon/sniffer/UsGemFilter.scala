// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.lib._
import pon.gpon.GponCodes._

/** The dump rules on the upstream bursts, between SnifferUs and its record writer: a burst's bytes (BIP first) in
  * their order, less what the rules take out. GEM frames are found by their HEC from the burst's fourth byte on, then
  * followed by their PLI (the PLOAMu / DBRu before them are not known here: 13 HEC bits, a false header about once in
  * 8000 such bytes); idle headers pass. A frame's first fragment is judged on its first bytes (RuleMatch), its later
  * ones follow it, per Port-ID (T-CONTs interleave). Of a fragment a rule drops or shortens the header stays, its HEC
  * field XORed with 0x1000 | payload bytes left, then those bytes. Of a run of idle headers the last one stays, its
  * HEC field XORed with 0x1000 | idle headers left out before it (up to 4095; none: as it is): a granted but unused
  * allocation costs 5 bytes. With `fec` the parity bytes (16 after each 239
  * from the BIP, the burst's last 16 when its last codeword is short) are left out, flag 2. The record gets flag 1 and ends with the burst's line length (16 bits, low
  * byte first). The bytes wait `depth` deep: a verdict is there before its header leaves. `close` comes after the
  * burst's last byte or with it, the next burst 9 clocks later at least (SnifferUs's quiet bytes). */
case class UsGemFilter(ruleCount: Int = 16, depth: Int = 32) extends Component {
  val io = new Bundle {
    val data = slave Flow (Bits(8 bits))
    val close = slave Flow (RecordHead(0))
    val fec = in Bool ()
    val rules = master(RuleRead(ruleCount))
    val defaultAction = in Bits (2 bits)
    val snap = in UInt (12 bits)
    val dataOut = master Flow (Bits(8 bits))
    val closeOut = master Flow (RecordHead(0))
    val filtered = out UInt (32 bits) // fragments a rule dropped
  }
  val active = Reg(Bool()) init (False) // inside a burst
  val lineBytes = Reg(UInt(16 bits)) init (0)
  val codeAt = Reg(UInt(8 bits)) init (0) // the incoming byte's place in its FEC codeword
  val lineNow = lineBytes + io.data.valid.asUInt
  val byteIn = io.data.valid && !(io.fec && codeAt >= 239)
  val closed = RegNext(io.close.valid) init (False)

  val find = new Area {
    val seen = Reg(UInt(3 bits)) init (0) // bytes of the burst so far, held at 7: a header's five then follow BIP, ONU-ID, Ind
    val window = Reg(Bits(32 bits))
    val hunting = Reg(Bool()) init (True)
    val left = Reg(UInt(13 bits)) // bytes to the end of the next header
    val payIndex = Reg(UInt(12 bits))
    val header = (window ## io.data.payload) ^ B(gemMask, 40 bits)
    val pli = header(39 downto 28).asUInt
    val port = header(27 downto 16).asUInt
    val pti = header(15 downto 13)
    val due = !hunting && left === 1
    val found = byteIn && seen === 7 && (hunting || due) && gemHeaderValid(header)
    val begins = found && pli =/= 0
    val idle = found && header === 0
    val payload = byteIn && !hunting && left > 5
    val last = left === 6
    when(byteIn) {
      window := window(23 downto 0) ## io.data.payload
      when(seen =/= 7)(seen := seen + 1)
      when(found) {
        hunting := False
        left := pli +^ 5
        payIndex := 0
      } elsewhen (due) {
        hunting := True
      } elsewhen (!hunting) {
        left := left - 1
      }
      when(payload)(payIndex := payIndex + 1)
    }
  }
  when(io.data.valid) {
    active := True
    lineBytes := lineNow
    codeAt := (codeAt === 254) ? U(0, 8 bits) | codeAt + 1
  }
  when(io.close.valid) {
    active := False
    lineBytes := 0
    codeAt := 0
    find.seen := 0
    find.hunting := True
  }

  val matcher = RuleMatch(us = true, ruleCount)
  matcher.io.table <> io.rules
  matcher.io.defaultAction := io.defaultAction
  matcher.io.start.valid := find.begins
  matcher.io.start.port := find.port
  matcher.io.start.length := find.pli
  matcher.io.start.blind := False // the upstream is never ciphered
  val held = Reg(Bits(24 bits))
  val quad = find.payIndex(1 downto 0)
  val word = Flow(FrameWord())
  word.valid := find.payload && (quad === 3 || find.last)
  word.bytes := (held ## io.data.payload) |<< ((U(3, 2 bits) - quad) << 3)
  word.first := find.payIndex < 4
  word.last := find.last
  matcher.io.word << word.stage() // a byte per clock at the line rate: the compare gets a clock of its own
  when(find.payload)(held := held(15 downto 0) ## io.data.payload)

  case class Verdict() extends Bundle {
    val action = Bits(2 bits)
    val pli = UInt(12 bits)
  }
  // per Port-ID: bit 0 a frame is open (its last fragment was not the end), bit 1 that frame is kept
  val frames = Mem(Bits(2 bits), 4096) init (Seq.fill(4096)(B(0, 2 bits)))
  val state = frames.readSync(find.port, find.begins)
  val fragment = new Area {
    val port = RegNextWhen(find.port, find.begins)
    val pli = RegNextWhen(find.pli, find.begins)
    val pti = RegNextWhen(find.pti, find.begins)
    val waiting = Reg(Bool()) init (False)
    val continued = (RegNext(find.begins) init (False)) && state(0)
    val judged = matcher.io.verdict.valid
    // the burst ended on it: kept, and its frame with it
    val decided = waiting && (continued || judged || RegNext(closed, False))
    val action = continued ? (state(1) ? B(RuleAction.keep, 2 bits) | B(RuleAction.drop, 2 bits)) |
      (judged ? matcher.io.verdict.payload | B(RuleAction.keep, 2 bits))
    when(find.begins)(waiting := True)
    when(decided)(waiting := False)
    frames.write(port, (action === RuleAction.keep) ## (!pti(0) && (continued || judged)), decided)
  }
  val verdicts = StreamFifo(Verdict(), 8)
  verdicts.io.push.valid := fragment.decided
  verdicts.io.push.action := fragment.action
  verdicts.io.push.pli := fragment.pli
  val filtered = Reg(UInt(32 bits)) init (0)
  when(fragment.decided && fragment.action === RuleAction.drop)(filtered := filtered + 1)
  io.filtered := filtered

  case class Tail() extends Bundle {
    val head = RecordHead(0)
    val lineBytes = UInt(16 bits)
    val fec = Bool()
  }
  val tails = StreamFifo(Tail(), 4)
  tails.io.push.valid := io.close.valid
  tails.io.push.head := io.close.payload
  tails.io.push.lineBytes := lineNow
  tails.io.push.fec := io.fec

  // the delay: it moves with each byte inside a burst, each clock outside (a burst's end then leaves by itself)
  case class Stage() extends Bundle {
    val valid, end = Bool()
    val idle = Bool() // a byte of an idle header
    val parity = Bool() // of the burst's short last codeword: known at its end only
    val tag = UInt(2 bits) // 0 as it is, 1 / 2 a judged fragment's two HEC bytes, 3 its payload
    val byte = Bits(8 bits)
  }
  val stages = Vec(Reg(Stage()), depth)
  stages.foreach { s =>
    s.valid init (False)
    s.end init (False)
  }
  val shift = byteIn || (!active && !io.data.valid)
  when(shift) {
    for (i <- 1 until depth) stages(i) := stages(i - 1)
    stages(0).valid := byteIn
    stages(0).end := closed
    stages(0).parity := False
    stages(0).byte := io.data.payload
    stages(0).tag := find.begins ? U(2, 2 bits) | (find.payload ? U(3, 2 bits) | U(0, 2 bits))
    stages(0).idle := find.idle
    when(find.idle)(for (i <- 1 to 4) stages(i).idle := True) // its four bytes before
    when(find.begins)(stages(1).tag := 1) // the byte before: the HEC's first bits
  }
  val codeNow = codeAt +^ io.data.valid.asUInt // bytes of the open codeword at the burst's end
  when(io.close.valid && io.fec && codeNow =/= 0 && codeNow < 239) {
    for (i <- 0 until 16) when(codeNow > i)(stages(i).parity := True)
  }

  val exit = new Area {
    val stage = stages.last
    val leaves = shift && stage.valid
    val kept = Reg(UInt(12 bits)) // payload bytes of the fragment leaving that stay
    val touched = Reg(Bool())
    val gone = Reg(UInt(12 bits))
    val opens = leaves && stage.tag === 1
    val verdict = verdicts.io.pop
    verdict.ready := opens
    val keptNow = verdict.action.mux(
      RuleAction.keep -> verdict.pli,
      RuleAction.drop -> U(0, 12 bits),
      default -> ((verdict.pli < io.snap) ? verdict.pli | io.snap))
    val touchedNow = verdict.valid && keptNow < verdict.pli // no verdict cannot be (the delay's depth): all kept
    assert(!(opens && !verdict.valid), "UsGemFilter: a header left before its verdict", FAILURE)
    when(opens) {
      kept := verdict.valid ? keptNow | U(4095, 12 bits)
      touched := touchedNow
      gone := 0
    }
    when(leaves && stage.tag === 3)(gone := gone + 1)
    // an idle header leaves: left out when the header behind it is one too
    val idleAt = Reg(UInt(3 bits)) init (0)
    val idles = Reg(UInt(12 bits)) init (0) // left out since the last that stayed
    val skip = Reg(Bool())
    val behind = stages(depth - 6)
    val skipNow = behind.valid && behind.idle && !idles.andR
    val skipped = (idleAt === 0) ? skipNow | skip
    when(leaves && stage.idle) {
      idleAt := (idleAt === 4) ? U(0, 3 bits) | idleAt + 1
      when(idleAt === 0)(skip := skipNow)
      when(idleAt === 4)(idles := skip ? (idles + 1) | U(0, 12 bits))
    }
    val idleMark = stage.idle && idles =/= 0
    val fragmentByte = stage.tag.mux(
      1 -> (stage.byte ^ (touchedNow ? (B"0001" ## keptNow(11 downto 8)) | B(0, 8 bits))),
      2 -> (stage.byte ^ (touched ? kept(7 downto 0).asBits | B(0, 8 bits))),
      default -> stage.byte)
    val byte = fragmentByte ^ ((idleMark && idleAt === 3) ? (B"0001" ## idles(11 downto 8)) | B(0, 8 bits)) ^
      ((idleMark && idleAt === 4) ? idles(7 downto 0).asBits | B(0, 8 bits))
    val stays = !stage.parity && (stage.tag =/= 3 || gone < kept) && !(stage.idle && skipped)

    // a burst's end: its line length, then the close
    val tail = tails.io.pop
    val step = Reg(UInt(2 bits)) init (0)
    val ends = shift && stage.end
    when(ends)(step := 1)
    when(step =/= 0)(step := (step === 2) ? U(0, 2 bits) | step + 1)
    tail.ready := step === 2
    io.dataOut.valid := (leaves && stays) || ends || step === 1
    io.dataOut.payload := ends ? tail.lineBytes(7 downto 0).asBits | ((step === 1) ? tail.lineBytes(15 downto 8).asBits | byte)
    io.closeOut.valid := step === 2
    io.closeOut.payload := tail.head
    io.closeOut.flags.allowOverride := tail.head.flags | (B(0, 5 bits) ## tail.fec ## B"10")
  }
}
