// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.lib._
import spinal.lib.bus.regif._

/** What a dump rule does with the frame it matches; `head`: its first bytes only (the snap length). */
object RuleAction { val keep = 0; val drop = 1; val head = 2 }

/** A dump rule besides its bytes: the directions it judges (none: off). `need`: how many bytes of the frame it reads
  * (its last masked byte + 1); a first fragment shorter than that does not match. */
case class RuleConfig() extends Bundle {
  val ds, us, onPort = Bool()
  val action = Bits(2 bits)
  val need = UInt(5 bits)
  val port = UInt(12 bits)
}

case class RuleBytes() extends Bundle {
  val value, mask = Bits(32 bits)
}

case class RuleWrite(rules: Int) extends Bundle {
  val rule = UInt(log2Up(rules) bits)
  val word = UInt(3 bits)
  val bytes = RuleBytes()
}

case class RuleSet(rules: Int) extends Bundle {
  val rule = UInt(log2Up(rules) bits)
  val config = RuleConfig()
}

/** A matcher's view of the store: every rule's word `index`. */
case class RuleRead(rules: Int) extends Bundle with IMasterSlave {
  val index = UInt(3 bits)
  val bytes = Vec(RuleBytes(), rules)
  val configs = Vec(RuleConfig(), rules)
  override def asMaster(): Unit = {
    out(index)
    in(bytes, configs)
  }
}

/** The dump rules: per rule a small memory of value / mask words (4 frame bytes each, first byte in bits 31-24) and
  * its configuration in registers. No reset of the memories: a rule counts once given a direction. */
case class RuleStore(rules: Int = 16, readers: Int = 2) extends Component {
  val io = new Bundle {
    val write = slave Flow (RuleWrite(rules))
    val set = slave Flow (RuleSet(rules))
    val read = Vec(slave(RuleRead(rules)), readers)
  }
  val configs = Vec(Reg(RuleConfig()), rules)
  configs.foreach { c =>
    c.ds init (False)
    c.us init (False)
  }
  when(io.set.valid)(configs(io.set.rule) := io.set.config)
  val words = Seq.tabulate(rules) { i =>
    val mem = Mem(RuleBytes(), 8)
    mem.write(io.write.word, io.write.bytes, io.write.valid && io.write.rule === i)
    mem
  }
  for (r <- io.read) {
    r.bytes := Vec(words.map(_.readAsync(r.index, writeFirst)))
    r.configs := configs
  }
}

object RuleStore {
  /** The registers the host writes the rules through, and the two settings every matcher shares. */
  case class Registers(busif: BusIf, store: RuleStore) extends Area {
    val rules = store.rules
    val SELECT = busif.newReg("Dump rules: which rule and which of its words RULE_MASK and RULE write")(SymbolName("RULE_SELECT"))
    val word = SELECT.field(UInt(3 bits), AccessType.RW, 0, "word: frame bytes 4 x word to 4 x word + 3 (0-4)")(SymbolName("word"))
    SELECT.reserved(5 bits)
    val rule = SELECT.field(UInt(log2Up(rules) bits), AccessType.RW, 0, "rule: the lowest that matches decides")(SymbolName("rule"))
    val VALUE = busif.newReg("Dump rules: value of the selected word, first frame byte in bits 31-24")(SymbolName("RULE_VALUE"))
    val value = VALUE.field(Bits(32 bits), AccessType.RW, 0, "value")(SymbolName("value"))
    val MASK = busif.newReg("Dump rules: mask of the selected word (1: the bit is compared); its write stores value and mask")(SymbolName("RULE_MASK"))
    MASK.field(Bits(32 bits), AccessType.WO, 0, "mask")(SymbolName("mask"))
    store.io.write.valid := MASK.hitDoWrite
    store.io.write.rule := rule
    store.io.write.word := word
    store.io.write.bytes.value := value
    store.io.write.bytes.mask := busif.writeData
    val RULE = busif.newReg("Dump rules: the selected rule, write only")(SymbolName("RULE"))
    RULE.field(Bool(), AccessType.WO, 0, "ds: judges downstream frames")(SymbolName("ds"))
    RULE.field(Bool(), AccessType.WO, 0, "us: judges upstream frames")(SymbolName("us"))
    RULE.field(Bool(), AccessType.WO, 0, "onPort: only frames of the Port-ID `port`")(SymbolName("onPort"))
    RULE.reserved(1 bits)
    RULE.field(Bits(2 bits), AccessType.WO, 0, "action: 0 keep, 1 drop, 2 keep the first FILTER.snap bytes")(SymbolName("action"))
    RULE.reserved(2 bits)
    RULE.field(UInt(5 bits), AccessType.WO, 0, "need: the last compared byte's number + 1 (0-20)")(SymbolName("need"))
    RULE.reserved(3 bits)
    RULE.field(UInt(12 bits), AccessType.WO, 0, "port")(SymbolName("port"))
    val data = busif.writeData
    store.io.set.valid := RULE.hitDoWrite
    store.io.set.rule := rule
    store.io.set.config.ds := data(0)
    store.io.set.config.us := data(1)
    store.io.set.config.onPort := data(2)
    store.io.set.config.action := data(5 downto 4)
    store.io.set.config.need := data(12 downto 8).asUInt
    store.io.set.config.port := data(27 downto 16).asUInt
    val FILTER = busif.newReg("Dump rules: frames no rule matches, and the length action 2 keeps")(SymbolName("FILTER"))
    val defaultAction = FILTER.field(Bits(2 bits), AccessType.RW, RuleAction.keep, "action: as RULE.action")(SymbolName("action"))
    FILTER.reserved(6 bits)
    val usFec = FILTER.field(Bool(), AccessType.RW, 0, "usFec: the ONU's bursts carry FEC parity (its grants say), left out of their records")(SymbolName("usFec"))
    FILTER.reserved(7 bits)
    val snap = FILTER.field(UInt(12 bits), AccessType.RW, 64, "snap: bytes kept by action 2, to the end of their line word (up to 3 more), 24 at least")(SymbolName("snap"))
  }
}

case class RuleStart() extends Bundle {
  val port = UInt(12 bits)
  val length = UInt(12 bits)
}

case class FrameWord() extends Bundle {
  val bytes = Bits(32 bits) // the frame's next 4 bytes, the first in bits 31-24
  val first, last = Bool()
}

/** The verdict of the rules of one direction on a frame: its Port-ID and first fragment's length (`start`, before
  * the first word), then its bytes from the first, 4 per word, until `words` of them or `last`. The verdict comes the
  * clock after that word: the action of the first enabled rule whose masked bytes all compared equal, else
  * `defaultAction`. Registers and comparators only: a rule costs one 32-bit masked compare. */
case class RuleMatch(us: Boolean, rules: Int = 16, words: Int = 5) extends Component {
  val io = new Bundle {
    val table = master(RuleRead(rules))
    val start = slave Flow (RuleStart())
    val word = slave Flow (FrameWord())
    val defaultAction = in Bits (2 bits)
    val verdict = master Flow (Bits(2 bits))
  }
  val configs = io.table.configs
  val candidates = Reg(Bits(rules bits))
  when(io.start.valid) {
    candidates := configs.map(c => (if (us) c.us else c.ds) && (!c.onPort || c.port === io.start.port) && c.need <= io.start.length).asBits()
  }
  val alive = Reg(Bits(rules bits)) init (0)
  val index = Reg(UInt(3 bits))
  val running = Reg(Bool()) init (False)
  val done = Reg(Bool()) init (False)
  val indexNow = io.word.first ? U(0, 3 bits) | index
  io.table.index := indexNow
  val equal = io.table.bytes.map(b => ((io.word.bytes ^ b.value) & b.mask) === 0).asBits()
  done := False
  when(io.word.valid && (io.word.first || running)) {
    val ending = io.word.last || indexNow === words - 1
    alive := (io.word.first ? candidates | alive) & equal
    index := indexNow + 1
    running := !ending
    done := ending
  }
  io.verdict.valid := done
  io.verdict.payload := alive.orR ? MuxOH(OHMasking.first(alive), configs.map(_.action)) | io.defaultAction
}

/** A queue whose pushes reach the pop side once committed (`commit`: all so far, this clock's included);
  * `rollback` takes the uncommitted ones back. The pusher checks `availability`. */
case class CommitFifo[T <: Data](dataType: HardType[T], depth: Int) extends Component {
  require(isPow2(depth))
  val io = new Bundle {
    val push = slave Flow (dataType())
    val commit, rollback = in Bool ()
    val pop = master Stream (dataType())
    val availability = out UInt (log2Up(depth + 1) bits)
  }
  val ram = Mem(dataType(), depth)
  val write, committed, read = Reg(UInt(log2Up(depth) + 1 bits)) init (0)
  val writeNext = write + io.push.valid.asUInt
  ram.write(write.resized, io.push.payload, io.push.valid)
  write := io.rollback ? committed | writeNext
  when(io.commit && !io.rollback)(committed := writeNext)
  io.availability := U(depth) - (write - read)
  val readNext = read + io.pop.fire.asUInt
  read := readNext
  io.pop.valid := RegNext(readNext =/= committed) init (False) // a clock after the commit: the memory has it by then
  io.pop.payload := ram.readSync(readNext.resized)
}

/** The keystream of the first 32 bytes of ciphered GEM fragments, for the rules to read them (G.984.3 12.2: AES-128
  * counter mode, the block is the crypto-counter three times, +1 per block). A fragment's counter is taken while
  * `start.ready` (a key is in use or due, room in the queue), its two blocks leave on `output` in order within
  * `GemKeystream.latency` clocks. `switch`: the superframe from which `nextKey` is the key (Key_Switching_Time);
  * `forget`: no key any more. */
case class GemKeystream(depth: Int = 2) extends Component {
  val io = new Bundle {
    val start = slave Stream (UInt(46 bits))
    val nextKey = in Bits (128 bits)
    val switch = slave Flow (UInt(30 bits))
    val forget = in Bool ()
    val output = master Stream (Bits(256 bits))
  }
  val key = Reg(Bits(128 bits))
  val keyed = Reg(Bool()) init (False)
  val armed = Reg(Bool()) init (False)
  val switchFrame = Reg(UInt(30 bits))
  def due(counter: UInt): Bool = armed && !(counter(45 downto 16) - switchFrame).msb
  when(io.switch.valid) {
    switchFrame := io.switch.payload
    armed := True
  }
  val jobs = StreamFifo(UInt(46 bits), depth)
  jobs.io.push.valid := io.start.valid && (keyed || due(io.start.payload))
  jobs.io.push.payload := io.start.payload
  io.start.ready := jobs.io.push.ready && (keyed || due(io.start.payload))

  val aes = pon.Aes128()
  val job = jobs.io.pop
  val second = Reg(Bool()) init (False)
  val counter = job.payload + second.asUInt
  val fresh = due(job.payload) && !io.switch.valid
  aes.io.input.valid := job.valid
  aes.io.input.block := (counter @@ counter @@ counter).resize(128).asBits
  aes.io.input.key := fresh ? io.nextKey | key
  job.ready := aes.io.input.ready && second
  when(aes.io.input.fire) {
    second := !second
    when(second && fresh) {
      key := io.nextKey
      keyed := True
      armed := False
    }
  }
  when(io.forget) {
    keyed := False
    armed := False
  }
  val half = Reg(Bool()) init (False)
  val first = RegNextWhen(aes.io.output.payload, aes.io.output.valid)
  when(aes.io.output.valid)(half := !half)
  val results = StreamFifo(Bits(256 bits), 8)
  results.io.push.valid := aes.io.output.valid && half
  results.io.push.payload := first ## aes.io.output.payload
  assert(!(results.io.push.valid && !results.io.push.ready), "GemKeystream: no room for a keystream", FAILURE)
  io.output << results.io.pop
}

object GemKeystream {
  val latency = 80 // two jobs ahead (22 clocks each), its own, the queues
}
