// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.collection.mutable
import pon.gpon._
import pon.gpon.GponCodes._
import pon.gpon.GtcDsTest.{check, GemDriver, PloamDriver, writeBwmap}
import pon.sniffer.SnifferTest.{Record, RecordSink, writeMap}

/** Reference: OLT framer -> byte-wide deframer -> SnifferDs, the line bytes out. Test only. */
case class WideDsReference(fec: Boolean) extends Component {
  val framer = GtcDsFramer()
  val deframer = GtcDsDeframer()
  val sniffer = SnifferDs()
  val io = new Bundle {
    val ploam = slave(cloneOf(framer.io.ploam))
    val bwmapWrite = slave(cloneOf(framer.io.bwmapWrite))
    val bwmapCommit = slave(cloneOf(framer.io.bwmapCommit))
    val gem = slave(cloneOf(framer.io.gem))
    val allocMap = slave Flow (KeepWrite())
    val portMap = slave Flow (KeepWrite())
    val line = out Bits (8 bits)
    val frameStart = out Bool ()
    val output = master Stream (Fragment(Bits(8 bits)))
  }
  framer.io.ploam << io.ploam
  framer.io.bwmapWrite << io.bwmapWrite
  framer.io.bwmapCommit << io.bwmapCommit
  framer.io.gem << io.gem
  framer.io.fec := Bool(fec)
  io.line := framer.io.line
  io.frameStart := framer.io.frameStart
  deframer.io.line := framer.io.line
  sniffer.io.fields := DsFields.of(deframer)
  sniffer.io.time := 0
  sniffer.io.onuId := 5
  sniffer.io.allPloams := False
  sniffer.io.allocMap << io.allocMap
  sniffer.io.portMap << io.portMap
  io.output << sniffer.io.output
}

/** The 4-byte-wide chain. Test only. */
case class WideDsBench() extends Component {
  val deframer = WideDsDeframer()
  val sniffer = WideSnifferDs()
  val io = new Bundle {
    val word = in Bits (32 bits)
    val allocMap = slave Flow (KeepWrite())
    val portMap = slave Flow (KeepWrite())
    val ruleWrite = slave Flow (RuleWrite(16))
    val ruleSet = slave Flow (RuleSet(16))
    val defaultAction = in Bits (2 bits)
    val snap = in UInt (12 bits)
    val output = master Stream (Fragment(Bits(8 bits)))
    val synced = out Bool ()
    val drops, truncations, hecErrors, plendErrors, syncLosses, filtered, unread = out UInt (32 bits)
    val cipherMap = slave Flow (KeepWrite())
    val nextKey = in Bits (128 bits)
    val keySwitch = slave Flow (UInt(30 bits))
  }
  sniffer.io.cipherMap << io.cipherMap
  sniffer.io.nextKey := io.nextKey
  sniffer.io.keySwitch << io.keySwitch
  io.unread := sniffer.io.unread
  val store = RuleStore(readers = 1)
  store.io.write << io.ruleWrite
  store.io.set << io.ruleSet
  sniffer.io.rules <> store.io.read(0)
  sniffer.io.defaultAction := io.defaultAction
  sniffer.io.snap := io.snap
  io.filtered := sniffer.io.filtered
  deframer.io.word := io.word
  sniffer.io.ploam := deframer.io.ploam
  sniffer.io.alloc := deframer.io.alloc
  sniffer.io.gem := deframer.io.gem
  sniffer.io.time := 0
  sniffer.io.onuId := 5
  sniffer.io.allPloams := False
  sniffer.io.allocMap << io.allocMap
  sniffer.io.portMap << io.portMap
  io.output << sniffer.io.output
  io.synced := deframer.io.synced
  io.drops := sniffer.io.drops
  io.truncations := sniffer.io.truncations
  io.hecErrors := deframer.io.hecErrors
  io.plendErrors := deframer.io.plendErrors
  io.syncLosses := deframer.io.syncLosses
}

/** A downstream line and what the byte-wide chain records from it (not an App: its values are set when used). */
object DsLine {
  val allocIds = Seq(0x105)
  val portIds = Seq(0x100, 0x101)
  val frames = 12
  val psyncBytes = Seq(0xB6, 0xAB, 0x31, 0xE0)

  /** Line bytes of `frames` frames with traffic for several ONUs, less the framer's first frame (it has no FEC yet),
    * and the reference's records. A quarter of the GEM bytes is ours (what the wide sniffer drains), half when `heavy`. */
  def reference(fec: Boolean, heavy: Boolean = false): (Seq[Int], Seq[Record]) = {
    val ports = if (heavy) Seq(0x100, 0x200, 0x101, 0x300) else Seq(0x100, 0x200, 0x300, 0x200, 0x101, 0x300, 0x200, 0x300)
    val line = mutable.ArrayBuffer[Int]()
    var records = Seq[Record]()
    SimConfig.compile(WideDsReference(fec)).doSim(if (fec) 2 else 1) { dut =>
      val cd = dut.clockDomain
      val gem = new GemDriver(dut.io.gem, cd)
      val ploam = new PloamDriver(dut.io.ploam, cd)
      val sink = new RecordSink(dut.io.output, cd, 1)
      dut.io.bwmapWrite.valid #= false
      dut.io.bwmapCommit.valid #= false
      dut.io.allocMap.valid #= false
      dut.io.portMap.valid #= false
      var frameStarts = 0
      cd.onSamplings {
        line += dut.io.line.toInt
        if (dut.io.frameStart.toBoolean) frameStarts += 1
      }
      cd.forkStimulus(10)
      writeMap(dut.io.allocMap, allocIds, cd)
      writeMap(dut.io.portMap, portIds, cd)
      val rand = new scala.util.Random(3)
      cd.waitSamplingWhere(frameStarts == 5) // both deframers long in sync: the same superframe count
      writeBwmap(dut.io.bwmapWrite, dut.io.bwmapCommit,
        Seq((0x105, 0x080, 0x0100, 0x0400), (0x205, 0x000, 0x0500, 0x0900), (0x106, 0x000, 0x0A00, 0x0B00), (0x105, 0x000, 0x0C00, 0x0C10)), cd)
      ploam.queue ++= Seq(5, 7, 0xFF, 5, 9).map(onu => (BigInt(onu) << 88) | BigInt(88, rand))
      gem.queue ++= Seq.tabulate(60)(i => (ports(i % ports.length), Seq.fill(Seq(64, 1518, 5000, 7, 300, 1, 2, 3, 4, 5, 6)(i % 11))(rand.nextInt(256))))
      cd.waitSamplingWhere(frameStarts == frames)
      cd.waitSampling(3000)
      records = sink.records.toSeq
    }
    (line.drop(line.indexOfSlice(psyncBytes) + frameBytes - 8).toSeq, records)
  }

}

/** A dump rule and what the hardware must do with it, in software. `bytes`: (frame byte, value, mask). */
case class DumpRule(action: Int, bytes: Seq[(Int, Int, Int)], port: Option[Int] = None, us: Boolean = false, enable: Boolean = true) {
  val need = if (bytes.isEmpty) 0 else bytes.map(_._1).max + 1
  def matches(upstream: Boolean, portId: Int, frame: Seq[Int]): Boolean =
    enable && us == upstream && port.forall(_ == portId) && need <= frame.length && bytes.forall { case (at, value, mask) => ((frame(at) ^ value) & mask) == 0 }
}

object DumpRule {
  def write(write: Flow[RuleWrite], set: Flow[RuleSet], rules: Seq[DumpRule], cd: ClockDomain): Unit = {
    for ((r, i) <- rules.zipWithIndex) {
      for (w <- 0 until 5) {
        def word(pick: ((Int, Int, Int)) => Int) = (0 until 4).foldLeft(0L)((a, b) => (a << 8) | r.bytes.filter(_._1 == 4 * w + b).map(pick).headOption.getOrElse(0))
        write.valid #= true; write.rule #= i; write.word #= w
        write.bytes.value #= word(_._2); write.bytes.mask #= word(_._3)
        cd.waitSampling()
      }
      write.valid #= false
      set.valid #= true; set.rule #= i
      set.config.ds #= r.enable && !r.us; set.config.us #= r.enable && r.us; set.config.onPort #= r.port.nonEmpty
      set.config.port #= r.port.getOrElse[Int](0); set.config.action #= r.action; set.config.need #= r.need
      cd.waitSampling()
      set.valid #= false
    }
  }

  def verdict(rules: Seq[DumpRule], default: Int, upstream: Boolean, portId: Int, frame: Seq[Int]): Int =
    rules.find(_.matches(upstream, portId, frame)).map(_.action).getOrElse(default)
}

/** The wide deframer against the byte-wide one on the same line: the same records, FEC off and on, on several bit
  * alignments; a corrupted GEM header; a full-rate burst (whole fragments dropped, each counted); dump rules against
  * their software model. Throws on failure. */
object WideDsTest extends App {
  import DsLine._

  /** The line, `shift` bits late, through the wide chain; `corrupt`: (byte, mask) flipped on the line. The counters
    * are read where the line ends: the zeros that follow (time to drain) are no frame. */
  def wide(compiled: SimCompiled[WideDsBench], line: Seq[Int], shift: Int, corrupt: Option[(Int, Int)] = None,
           rules: Seq[DumpRule] = Nil, default: Int = RuleAction.keep, snap: Int = 64, ciphered: Seq[Int] = Nil, key: Option[BigInt] = None) = {
    val bytes = line.toArray
    corrupt.foreach { case (at, mask) => bytes(at) ^= mask }
    def bit(i: Int): Int = if (i < shift || (i - shift) / 8 >= bytes.length) 0 else (bytes((i - shift) / 8) >> (7 - (i - shift) % 8)) & 1
    var records = Seq[Record]()
    var counters = (0L, 0L, 0L, 0L, 0L)
    filtered = 0
    unread = 0
    compiled.doSim(shift) { dut =>
      val cd = dut.clockDomain
      val sink = new RecordSink(dut.io.output, cd, 2)
      dut.io.allocMap.valid #= false
      dut.io.portMap.valid #= false
      dut.io.word #= 0
      dut.io.ruleWrite.valid #= false
      dut.io.ruleSet.valid #= false
      dut.io.defaultAction #= default
      dut.io.snap #= snap
      dut.io.cipherMap.valid #= false
      dut.io.keySwitch.valid #= false
      dut.io.nextKey #= key.getOrElse(BigInt(0))
      cd.forkStimulus(10)
      writeMap(dut.io.allocMap, allocIds, cd)
      writeMap(dut.io.portMap, portIds, cd)
      writeMap(dut.io.cipherMap, ciphered, cd)
      if (key.nonEmpty) { // from superframe 0: at once
        dut.io.keySwitch.valid #= true; dut.io.keySwitch.payload #= 0
        cd.waitSampling()
        dut.io.keySwitch.valid #= false
      }
      DumpRule.write(dut.io.ruleWrite, dut.io.ruleSet, rules, cd)
      for (w <- 0 until bytes.length / 4 + 12000) {
        dut.io.word #= (0 until 32).foldLeft(BigInt(0))((a, k) => (a << 1) | bit(32 * w + k))
        cd.waitSampling()
        if (w == bytes.length / 4 - 8)
          counters = (dut.io.drops.toLong, dut.io.truncations.toLong, dut.io.hecErrors.toLong, dut.io.plendErrors.toLong, dut.io.syncLosses.toLong)
      }
      records = sink.records.toSeq
      filtered = dut.io.filtered.toLong
      unread = dut.io.unread.toLong
    }
    (records, counters)
  }
  var filtered = 0L
  var unread = 0L

  def bytes16(x: BigInt): Array[Byte] = (15 to 0 by -1).map(i => ((x >> (8 * i)) & 0xFF).toByte).toArray
  /** G.984.3 12.2: AES-128 counter mode, the block is the crypto-counter three times, +1 per block. */
  def keystream(key: BigInt, counter: BigInt, n: Int): Seq[Int] = {
    val aes = javax.crypto.Cipher.getInstance("AES/ECB/NoPadding")
    aes.init(javax.crypto.Cipher.ENCRYPT_MODE, new javax.crypto.spec.SecretKeySpec(bytes16(key), "AES"))
    (0 until (n + 15) / 16).flatMap { b =>
      val c = counter + b
      aes.doFinal(bytes16(((c << 92) | (c << 46) | c) & ((BigInt(1) << 128) - 1))).map(_ & 0xFF)
    }.take(n)
  }

  /** The GEM records the rules leave of `expect`: (flags, payload, bytes a head-only record must have at least), the
    * fragments dropped, the frames kept unread. The rules read the `ciphered` Port-IDs through `key`'s keystream;
    * without a key those frames are kept. */
  def ruled(expect: Seq[Record], rules: Seq[DumpRule], default: Int, snap: Int, ciphered: Seq[Int] = Nil, key: Option[BigInt] = None): (Seq[(Int, Seq[Int], Int)], Int, Int) = {
    var open: Option[(Int, Boolean)] = None
    var dropped = 0
    var blind = 0
    def judge(port: Int, fragment: Seq[Int], counter: BigInt): Int =
      if (!ciphered.contains(port)) DumpRule.verdict(rules, default, upstream = false, port, fragment)
      else key.map(k => DumpRule.verdict(rules, default, upstream = false, port, fragment.zip(keystream(k, counter, fragment.length min 32) ++ Seq.fill(fragment.length)(0)).map { case (a, b) => a ^ b }))
        .getOrElse { blind += 1; RuleAction.keep }
    val left = expect.filter(_.kind == RecordKind.gem).flatMap { r =>
      val port = ((r.payload(0) << 8) | r.payload(1)) & 0xFFF
      val fragment = r.payload.drop(10)
      val counter = r.payload.slice(4, 10).foldLeft(BigInt(0))((a, b) => (a << 8) | b)
      val action = open.filter(_._1 == port).map(o => if (o._2) RuleAction.keep else RuleAction.drop)
        .getOrElse(judge(port, fragment, counter))
      open = if ((r.flags & 9) == 0) Some((port, action == RuleAction.keep)) else None
      if (action == RuleAction.drop) dropped += 1
      action match {
        case RuleAction.keep => Some((r.flags, r.payload, 0))
        case RuleAction.head => Some((r.flags | 0x10, r.payload, 10 + (fragment.length min snap)))
        case _ => None
      }
    }
    (left, dropped, blind)
  }

  def body(r: Record) = (r.flags, r.payload)
  def of(records: Seq[Record], kind: Int) = records.filter(_.kind == kind).map(body)
  val kinds = Seq((RecordKind.ploam, "PLOAM"), (RecordKind.alloc, "BWmap"), (RecordKind.gem, "GEM"))
  val compiled = SimConfig.compile(WideDsBench())
  for (fec <- Seq(false, true)) {
    val (line, expect) = reference(fec)
    val name = if (fec) "FEC" else "no FEC"
    check(of(expect, RecordKind.gem).length > 10 && of(expect, RecordKind.alloc).length >= 2, s"$name: the reference has GEM and BWmap records (${expect.length})")
    for (shift <- Seq(0, 1, 13, 31)) {
      val (got, counters) = wide(compiled, line, shift)
      for ((kind, what) <- kinds) {
        val (g, e) = (of(got, kind), of(expect, kind))
        // the two do not reach SYNC on the same frame: one frame's PLOAM more on either side
        check((g.endsWith(e) || e.endsWith(g)) && (g.length - e.length).abs <= 1, s"$name, $shift bits late: $what records as the byte-wide deframer's (${g.length} of ${e.length})")
      }
      check(counters == (0, 0, 0, 0, 0), s"$name, $shift bits late: nothing dropped, no error counted $counters")
    }
    if (!fec) {
      // the third frame is idle GEM headers from byte 30: one bit of the first one
      val (got, counters) = wide(compiled, line, 5, Some((line.indexOfSlice(psyncBytes) + 2 * frameBytes + 31, 0x10)))
      check(counters == (0, 0, 1, 0, 0), s"one HEC error counted for a corrupted idle header $counters")
      check(of(got, RecordKind.gem) == of(expect, RecordKind.gem), "the GEM records of the next frames are whole")
    }
  }
  for (fec <- Seq(false, true)) {
    val (line, expect) = reference(fec)
    val multicast = DumpRule(RuleAction.drop, Seq((0, 0x01, 0x01)), port = Some(0x100))
    val tagged = DumpRule(RuleAction.head, Seq((12, 0x80, 0xC0), (13, 0x00, 0x00)))
    val deep = DumpRule(RuleAction.drop, Seq((16, 0x00, 0x80), (19, 0x02, 0x03)), port = Some(0x101))
    val key = BigInt("000102030405060708090a0b0c0d0e0f", 16)
    val mixed = Seq(DumpRule(RuleAction.drop, Nil, us = true), DumpRule(RuleAction.drop, Nil, enable = false), multicast, tagged, deep)
    val sets = Seq(
      ("drop and head rules", mixed, RuleAction.keep, 40, Nil, None),
      ("keep rule, the rest dropped", Seq(DumpRule(RuleAction.keep, Seq((1, 0x80, 0x80))), tagged), RuleAction.drop, 100, Nil, None),
      ("Port-ID 0x101 ciphered", mixed, RuleAction.keep, 40, Seq(0x101), Some(key)),
      ("both Port-IDs ciphered, the rest dropped", Seq(DumpRule(RuleAction.keep, Seq((17, 0x40, 0x40))), DumpRule(RuleAction.head, Seq((5, 0x00, 0x80)))), RuleAction.drop, 100, Seq(0x100, 0x101), Some(key)),
      ("Port-ID 0x101 ciphered, no key", mixed, RuleAction.drop, 40, Seq(0x101), None))
    for ((what, rules, default, snap, ciphered, key) <- sets; shift <- Seq(0, 9, 18, 27)) {
      val name = s"${if (fec) "FEC" else "no FEC"}, $shift bits late, $what"
      val (got, counters) = wide(compiled, line, shift, rules = rules, default = default, snap = snap, ciphered = ciphered, key = key)
      val (want, dropped, blind) = ruled(expect, rules, default, snap, ciphered, key)
      val gems = got.filter(_.kind == RecordKind.gem)
      val heads = want.count(_._3 > 0)
      check(blind > 0 || dropped > 2 && heads > 0 && want.length - heads >= 2, s"$name: the model drops $dropped, shortens $heads, keeps ${want.length - heads} whole")
      check(gems.length == want.length && gems.zip(want).forall { case (g, (flags, payload, least)) =>
        g.flags == flags && (if (least == 0) g.payload == payload
        else payload.startsWith(g.payload) && g.payload.length >= least && g.payload.length <= (least + 3 min payload.length))
      }, s"$name: the GEM records the rules leave (${gems.length} of ${want.length})")
      check(filtered == dropped && counters == (0, 0, 0, 0, 0), s"$name: $filtered fragments counted as filtered, none lost $counters")
      check(unread == blind && (blind > 2) == (ciphered.nonEmpty && key.isEmpty), s"$name: $unread ciphered frames kept unread ($blind)")
      check(of(got, RecordKind.alloc) == of(expect, RecordKind.alloc), s"$name: BWmap records untouched")
    }
  }
  val (line, expect) = reference(fec = false, heavy = true)
  val (got, counters) = wide(compiled, line, 7)
  val (g, e) = (of(got, RecordKind.gem), of(expect, RecordKind.gem))
  val rest = e.iterator
  check(counters._1 > 0 && e.length - g.length == counters._1 && g.forall(r => rest.contains(r)),
    s"full-rate burst: ${g.length} of ${e.length} GEM records, whole and in order, the ${counters._1} others counted as drops")
  check(of(expect, RecordKind.ploam).endsWith(of(got, RecordKind.ploam)) && of(got, RecordKind.alloc) == of(expect, RecordKind.alloc), "full-rate burst: no PLOAM or BWmap record lost")
  println("PASS WideDsTest")
}
