// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.collection.mutable
import pon.gpon.GponCodes
import pon.gpon.GtcDsTest.check

/** UsGemFilter with its rule store. Test only. */
case class UsGemFilterBench() extends Component {
  val filter = UsGemFilter()
  val store = RuleStore(readers = 1)
  val io = new Bundle {
    val data = slave(cloneOf(filter.io.data))
    val close = slave(cloneOf(filter.io.close))
    val fec = in Bool ()
    val ruleWrite = slave(cloneOf(store.io.write))
    val ruleSet = slave(cloneOf(store.io.set))
    val defaultAction = in Bits (2 bits)
    val snap = in UInt (12 bits)
    val dataOut = master(cloneOf(filter.io.dataOut))
    val closeOut = master(cloneOf(filter.io.closeOut))
    val filtered = out UInt (32 bits)
  }
  filter.io.data << io.data
  filter.io.close << io.close
  filter.io.fec := io.fec
  filter.io.rules <> store.io.read(0)
  filter.io.defaultAction := io.defaultAction
  filter.io.snap := io.snap
  store.io.write << io.ruleWrite
  store.io.set << io.ruleSet
  io.dataOut << filter.io.dataOut
  io.closeOut << filter.io.closeOut
  io.filtered := filter.io.filtered
}

/** Bursts of overhead, idle headers (alone and in runs) and GEM fragments (frames split over bursts, two Port-IDs interleaved) through
  * the filter, against what each rule set must leave; with and without FEC parity. Throws on failure. */
object UsGemFilterTest extends App {
  val mask = (0 until 5).map(i => ((GponCodes.gemMask >> (8 * (4 - i))) & 0xFF).toInt)
  def header(pli: Int, port: Int, pti: Int, marker: Int = 0): Seq[Int] = {
    val h = GponCodes.Model.gemHeader(pli, port, pti) ^ marker
    (0 until 5).map(i => ((h >> (8 * (4 - i))) & 0xFF).toInt ^ mask(i))
  }
  def validAt(bytes: Seq[Int], at: Int): Boolean = {
    val h = (0 until 5).foldLeft(BigInt(0))((a, i) => (a << 8) | (bytes(at + i) ^ mask(i)))
    GponCodes.Model.gemHeader((h >> 28).toInt, ((h >> 16) & 0xFFF).toInt, ((h >> 13) & 7).toInt) == h
  }
  val rand = new scala.util.Random(11)
  def bytes(n: Int) = Seq.fill(n)(rand.nextInt(256))
  def overhead(n: Int) = bytes(n)
  /** Where a search from the fourth byte finds headers: (offset, PLI). */
  def hunt(data: Seq[Int]): Seq[(Int, Int)] = {
    val found = mutable.ArrayBuffer[(Int, Int)]()
    var at = 3
    while (at + 5 <= data.length) {
      if (validAt(data, at)) {
        val pli = ((data(at) ^ mask(0)) << 4) | ((data(at + 1) ^ mask(1)) >> 4)
        found += ((at, pli))
        at += 5 + pli
      } else at += 1
    }
    found.toSeq
  }

  sealed trait Piece
  case class Raw(bytes: Seq[Int]) extends Piece
  case class Fragment(port: Int, payload: Seq[Int], end: Boolean, first: Boolean = true) extends Piece
  val idle = Raw(mask)
  val big = bytes(900)
  // port 0x40's frame of 900 bytes goes on in the third burst, port 0x41's frames in between and after
  val bursts: Seq[Seq[Piece]] = Seq(
    Seq(Raw(Seq(0x11, 5, 0)), Raw(overhead(13)), Raw(overhead(2)), Fragment(0x40, bytes(64), true), idle, idle, Fragment(0x41, bytes(3), true),
      Fragment(0x40, bytes(21), true), Fragment(0x41, bytes(300), true), Fragment(0x40, big.take(250), false)),
    Seq(Raw(Seq(0x22, 5, 0)), Raw(overhead(2)), Fragment(0x41, bytes(20), true), Fragment(0x41, bytes(19), true), idle, Raw(overhead(2)),
      Fragment(0x41, bytes(1500), true), Fragment(0x41, bytes(1), true), Fragment(0x41, bytes(70), false)),
    Seq(Raw(Seq(0x33, 5, 0)), Fragment(0x40, big.slice(250, 600), false, first = false), Fragment(0x40, big.drop(600), true, first = false),
      Fragment(0x40, bytes(46), true), Fragment(0x40, bytes(4), true), idle),
    Seq(Raw(Seq(0x44, 5, 0)), Raw(overhead(2)), Fragment(0x41, bytes(30), true, first = false), Fragment(0x40, bytes(128), true)),
    Seq(Raw(Seq(0x55, 0xFF, 0)), Raw(overhead(13))),
    // an allocation the ONU does not fill: a run of idle headers longer than a mark counts, one at the burst's end
    Seq(Raw(Seq(0x66, 5, 0)), Fragment(0x41, bytes(40), true)) ++ Seq.fill(4100)(idle) ++ Seq(Fragment(0x40, bytes(30), true), idle, idle, idle))
  val frames = bursts.flatten.collect { case f: Fragment if f.first => f }
  check(frames.length > 12, s"${frames.length} frames to judge")
  for (b <- bursts) {
    var at = 0
    val headers = b.flatMap { piece =>
      val (here, size) = piece match {
        case Raw(x) => (if (x == mask) Some((at, 0)) else None, x.length)
        case Fragment(_, payload, _, _) => (Some((at, payload.length)), 5 + payload.length)
      }
      at += size
      here
    }
    val data = b.flatMap { case Raw(x) => x; case Fragment(port, payload, end, _) => header(payload.length, port, if (end) 1 else 0) ++ payload }
    check(hunt(data) == headers, s"test burst of ${data.length} bytes: no header but its ${headers.length} own is found in it")
  }

  def parity(data: Seq[Int]): Seq[Int] = data.grouped(239).flatMap(_ ++ Seq.fill(16)(rand.nextInt(256))).toSeq

  /** A burst's record payload and the fragments dropped: per Port-ID an open frame follows its first fragment. */
  def expected(burst: Seq[Piece], open: mutable.Map[Int, Int], rules: Seq[DumpRule], default: Int, snap: Int, lineBytes: Int): (Seq[Int], Int) = {
    var dropped = 0
    val body = mutable.ArrayBuffer[Int]()
    var idles = 0
    // of a run of idle headers the last stays, marked with the count of those before it (4095 at most)
    def run(): Unit = while (idles > 0) {
      val taken = idles min 4096
      body ++= header(0, 0, 0, if (taken > 1) 0x1000 | (taken - 1) else 0)
      idles -= taken
    }
    burst.foreach {
      case Raw(b) if b == mask => idles += 1
      case Raw(b) =>
        run()
        body ++= b
      case Fragment(port, payload, end, _) =>
        run()
        val action = open.remove(port).getOrElse(DumpRule.verdict(rules, default, upstream = true, port, payload))
        if (!end) open(port) = if (action == RuleAction.keep) action else RuleAction.drop
        if (action == RuleAction.drop) dropped += 1
        val kept = action match {
          case RuleAction.keep => payload.length
          case RuleAction.drop => 0
          case _ => payload.length min snap
        }
        body ++= header(payload.length, port, if (end) 1 else 0, if (kept < payload.length) 0x1000 | kept else 0) ++ payload.take(kept)
    }
    run()
    ((body ++ Seq(lineBytes & 0xFF, lineBytes >> 8)).toSeq, dropped)
  }

  val group = DumpRule(RuleAction.drop, Seq((0, 0x01, 0x01)), port = Some(0x41), us = true)
  val high = DumpRule(RuleAction.head, Seq((18, 0x80, 0x80), (19, 0x00, 0x00)), us = true)
  val low = DumpRule(RuleAction.drop, Seq((2, 0x00, 0xC0)), port = Some(0x40), us = true)
  val sets = Seq(
    ("no rule", Nil, RuleAction.keep, 64),
    ("drop and head rules", Seq(DumpRule(RuleAction.drop, Nil), group, high, low), RuleAction.keep, 40),
    ("keep rule, the rest dropped", Seq(DumpRule(RuleAction.keep, Seq((1, 0x80, 0x80)), us = true)), RuleAction.drop, 64),
    ("the rest shortened", Seq(group), RuleAction.head, 24))
  val compiled = SimConfig.compile(UsGemFilterBench())
  for (fec <- Seq(false, true); (what, rules, default, snap) <- sets) {
    val name = s"${if (fec) "FEC" else "no FEC"}, $what"
    val lines = bursts.map { b =>
      val data = b.flatMap { case Raw(x) => x; case Fragment(port, payload, end, _) => header(payload.length, port, if (end) 1 else 0) ++ payload }
      if (fec) parity(data) else data
    }
    val got = mutable.ArrayBuffer[(Int, Seq[Int])]()
    var filtered = 0L
    compiled.doSim(name.hashCode) { dut =>
      val cd = dut.clockDomain
      val record = mutable.ArrayBuffer[Int]()
      dut.io.data.valid #= false
      dut.io.close.valid #= false
      dut.io.ruleWrite.valid #= false
      dut.io.ruleSet.valid #= false
      dut.io.fec #= fec
      dut.io.defaultAction #= default
      dut.io.snap #= snap
      cd.onSamplings {
        if (dut.io.dataOut.valid.toBoolean) record += dut.io.dataOut.payload.toInt
        if (dut.io.closeOut.valid.toBoolean) {
          got += ((dut.io.closeOut.flags.toInt, record.toList))
          record.clear()
        }
      }
      cd.forkStimulus(10)
      DumpRule.write(dut.io.ruleWrite, dut.io.ruleSet, rules, cd)
      for ((line, i) <- lines.zipWithIndex) {
        val closeWithLast = i % 2 == 0
        for ((b, k) <- line.zipWithIndex) {
          dut.io.data.valid #= true
          dut.io.data.payload #= b
          if (closeWithLast && k == line.length - 1) {
            dut.io.close.valid #= true
            dut.io.close.flags #= 0
            dut.io.close.time #= i
          }
          cd.waitSampling()
        }
        dut.io.data.valid #= false
        if (!closeWithLast) {
          cd.waitSampling(3)
          dut.io.close.valid #= true
          dut.io.close.flags #= 1
          dut.io.close.time #= i
          cd.waitSampling()
        }
        dut.io.close.valid #= false
        cd.waitSampling(9)
      }
      cd.waitSampling(100)
      filtered = dut.io.filtered.toLong
    }
    val open = mutable.Map[Int, Int]()
    val want = bursts.zip(lines).zipWithIndex.map { case ((b, line), i) =>
      val (payload, dropped) = expected(b, open, rules, default, snap, line.length)
      ((if (i % 2 == 0) 0 else 1) | 2 | (if (fec) 4 else 0), payload, dropped)
    }
    val dropped = want.map(_._3).sum
    check(got.length == want.length, s"$name: a record per burst (${got.length})")
    for (((flags, payload), (wantFlags, wantPayload, _)) <- got.zip(want))
      check(flags == wantFlags && payload == wantPayload, s"$name: burst of ${wantPayload.length} bytes left, flags $wantFlags (got ${payload.length}, $flags)")
    check(filtered == dropped && (rules.isEmpty || dropped > 1), s"$name: $filtered fragments counted as dropped ($dropped)")
  }
  println("PASS UsGemFilterTest")
}
