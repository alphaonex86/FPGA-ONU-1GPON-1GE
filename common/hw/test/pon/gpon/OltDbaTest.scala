// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.core.sim._
import scala.collection.mutable
import GponCodes._
import GtcDsTest.check

/** DBA after the activation scenario: the CPU writes a T-CONT table, the ONU models queue traffic and report it. */
object OltDbaTest extends App {
  for (b <- 0 to 9000) {
    val code = OnuModel.dbruCode(b)
    val back = OnuModel.dbruBlocks(code)
    if (back < b || OnuModel.dbruCode(back) != code) throw new Exception(s"FAIL: DBRu code of $b blocks")
  }
  println("ok   DBRu mode 0 coding round trip (Table 8-1)")

  val maxFrames = 140
  val act = new OltActivation(maxFrames)
  case class Entry(onu: OnuModel, allocId: Int, fixed: Int, max: Int, dbru: Boolean)

  SimConfig.compile(OltUsLoop(8)).doSim { dut =>
    val cd = dut.clockDomain
    val io = dut.io
    act.configure(io.config)
    io.frameStart #= false
    io.ploam.ready #= false
    io.release.valid #= false
    io.tcontWrite.valid #= false
    io.time #= 0
    io.line #= 0

    val entries = mutable.Map[Int, Alloc]()
    var committed = Seq[Alloc]()
    val bwmaps = mutable.ArrayBuffer[Seq[Alloc]]()
    var dbruBad = 0
    var dbruCount = 0
    var cycle = 0
    var errorFrame, tableFrame = -1
    cd.onSamplings {
      if (errorFrame < 0 && io.scheduleError.toBoolean) errorFrame = act.frames
      if (tableFrame < 0 && io.tableError.toBoolean) tableFrame = act.frames
      if (io.bwmapWrite.valid.toBoolean) {
        val w = io.bwmapWrite.alloc
        entries(io.bwmapWrite.index.toInt) = Alloc(w.allocId.toInt, w.flags.toInt, w.start.toInt, w.stop.toInt)
      }
      if (io.bwmapCommit.valid.toBoolean) committed = (0 until io.bwmapCommit.payload.toInt).map(entries)
      if (io.dbru.valid.toBoolean) { dbruCount += 1; if (!io.dbru.crcOk.toBoolean) dbruBad += 1 }
      act.sample(io.event, io.cpuUsPloam, io.burst)
      if (io.frameStart.toBoolean) {
        val ploam = if (io.ploam.valid.toBoolean) Some(io.ploam.payload.toBigInt) else None
        if (act.cpuQueue.nonEmpty && ploam.contains(act.cpuQueue.head)) act.cpuQueue.dequeue()
        act.frame(cycle, committed, ploam, cycle)
        bwmaps += committed
      }
      cycle += 1
      val start = cycle % usFrameBytes == 0
      io.time #= cycle & 0xFFFFFF
      io.frameStart #= start
      io.ploam.ready #= start
      io.line #= act.line.byte(cycle)
      io.cpuPloam.valid #= act.cpuQueue.nonEmpty
      if (act.cpuQueue.nonEmpty) io.cpuPloam.payload #= act.cpuQueue.head
    }
    cd.forkStimulus(10)
    cd.waitSamplingWhere(act.done)
    check(Seq(act.a, act.b, act.c).forall(_.state == 5), "A, B, C active")

    // the CPU's T-CONT table, sorted by ONU-ID
    val table = Seq(
      Entry(act.a, 300, 0, 1500, dbru = true),  // bursty
      Entry(act.b, 301, 200, 200, dbru = false), // fixed only
      Entry(act.c, 302, 0, 800, dbru = true),   // steady
      Entry(act.c, 303, 100, 3000, dbru = true)).sortBy(_.onu.onuId)
    for ((e, i) <- table.zipWithIndex) {
      e.onu.queues(e.allocId) = 0
      io.tcontWrite.valid #= true
      io.tcontWrite.index #= i
      val t = io.tcontWrite.tcont
      t.allocId #= e.allocId
      t.onuId #= e.onu.onuId
      t.fixedBytes #= e.fixed
      t.maxBytes #= e.max
      t.dbru #= e.dbru
      cd.waitSampling()
    }
    io.tcontWrite.valid #= false
    io.config.tconts #= table.length
    io.config.snPeriod #= 0xFFFF // in operation the CPU opens discovery windows rarely: every frame is granted
    val first = act.frames + 5 // BWmaps from here carry the table, past any open window
    act.a.queues(300) = 20000
    act.c.queues(303) = 5000
    val steady = 300 // bytes per frame into T-CONT 302
    var arrived302 = 0L
    while (act.frames < first + 40) {
      val f = act.frames
      cd.waitSamplingWhere(act.frames > f)
      if (act.frames < first + 25) { act.c.queues(302) += steady; arrived302 += steady }
    }

    val maps = bwmaps.drop(first)
    def grants(id: Int) = maps.map(_.filter(_.allocId == id).map(x => x.stop - x.start + 1))
    for (e <- table) {
      val g = grants(e.allocId)
      val ovh = if (e.dbru) 2 else 0
      check(g.forall(_.length == 1) && g.flatten.forall(l => l >= e.fixed + ovh && l <= e.max + ovh),
        s"T-CONT ${e.allocId}: one grant per frame within fixed ${e.fixed} .. max ${e.max} (+ DBRu) " +
          s"(${g.take(12).map(_.mkString("/")).mkString(" ")})")
    }
    check(grants(301).flatten.forall(_ == 200) && maps.flatten.filter(_.allocId == 301).forall(_.flags == 0),
      "fixed-only T-CONT: 200 bytes every frame, no DBRu requested")
    val ids = table.map(_.allocId)
    check(maps.forall { m =>
      Seq(act.a, act.b, act.c).forall { o =>
        val mine = m.filter(x => x.allocId == o.onuId || table.exists(e => e.onu == o && e.allocId == x.allocId))
        mine.head.allocId == o.onuId && mine.head.flags == 0x400 && mine.zip(mine.tail).forall { case (p, q) => q.start == p.stop + 1 }
      }
    }, "each ONU's burst: default allocation with PLOAMu, then its T-CONTs contiguous")
    for ((id, total) <- Seq(300 -> 20000L, 303 -> 5000L, 302 -> arrived302)) {
      val o = table.find(_.allocId == id).get.onu
      check(o.queues(id) == 0 && o.sent(id) == total, s"T-CONT $id: its ${total} queued bytes all sent (left ${o.queues(id)})")
    }
    val last = maps.takeRight(5)
    check(last.flatten.filter(x => Seq(300, 302).contains(x.allocId)).forall(x => x.stop - x.start + 1 == 2) &&
      last.flatten.filter(_.allocId == 303).forall(x => x.stop - x.start + 1 == 102),
      "queues empty: grants back to fixed + DBRu")
    val dynamic300 = grants(300).flatten.map(_ - 2).sum
    check(dynamic300 >= 20000 && dynamic300 <= 20000 + 3 * 1500,
      s"T-CONT 300 over-grant bounded by the report delay ($dynamic300 bytes granted for 20000)")
    val bursts = act.bursts.takeRight(30)
    check(bursts.forall(x => x._1 && x._4 == 0) && dbruCount > 0 && dbruBad == 0,
      s"multi-allocation bursts received to the bit, $dbruCount DBRu with good CRC")
    check(!io.scheduleError.toBoolean && !io.tableError.toBoolean,
      s"no schedule or table error (schedule error from frame $errorFrame, table error from $tableFrame, table loaded at ${first - 5})")

    // an unsorted table is reported, not silently mis-granted
    io.tcontWrite.valid #= true
    io.tcontWrite.index #= 0
    io.tcontWrite.tcont.onuId #= act.c.onuId
    cd.waitSampling()
    io.tcontWrite.valid #= false
    val f = act.frames
    cd.waitSamplingWhere(act.frames == f + 2)
    check(io.tableError.toBoolean, "T-CONT table out of ONU-ID order: tableError")
  }
  println("PASS OltDbaTest")
}
