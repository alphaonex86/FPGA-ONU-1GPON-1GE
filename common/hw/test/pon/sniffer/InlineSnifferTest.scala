// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.core.sim._
import scala.collection.mutable
import pon.gpon._
import pon.gpon.GponCodes._
import pon.gpon.GtcDsTest.{check, bigToBytes, bytesToBig}
import pon.sniffer.SnifferTest.Record

/** The in-line sniffer whole, its three clocks and a USB host on the link: both directions sent again bit for
  * bit, the lasers on only when they must, the registers over the link, the records of both directions as the
  * byte-wide chain's. Throws on failure. */
object InlineSnifferTest extends App {
  // the reference's line to its last whole frame, then its first frame (idle) again while the link empties
  val (reference, expect) = DsLine.reference(fec = false)
  val idleFrames = 14
  val firstFrame = reference.indexOfSlice(DsLine.psyncBytes)
  val line = reference.take(reference.lastIndexOfSlice(DsLine.psyncBytes)) ++
    Seq.fill(idleFrames)(reference.slice(firstFrame, firstFrame + frameBytes)).flatten ++ DsLine.psyncBytes
  val dsBits = line.flatMap(b => (7 to 0 by -1).map(k => (b >> k) & 1)).toArray

  // upstream, line bits at 1.244 Gb/s: preamble, delimiter, scrambled burst; zero between
  val rand = new scala.util.Random(21)
  val bursts = Seq(400000, 700003, 700003 + 4000).map(at => (at, Seq(0x3C, 5, 0x00) ++ Seq.fill(300)(rand.nextInt(256))))
  def scrambled(clear: Seq[Int]) = clear.zip(Model.keystream(clear.length)).map { case (c, k) => c ^ k }
  val usBits = Array.fill(dsBits.length / 2 + 64)(0)
  def put(bit: Int, bytes: Seq[Int]): Unit = for ((b, i) <- bytes.zipWithIndex; k <- 0 until 8) usBits(bit + 8 * i + k) = (b >> (7 - k)) & 1
  for ((at, clear) <- bursts) {
    put(at - 11 * 8, Seq.fill(8)(0xAA))
    put(at - 3 * 8, bigToBytes(usDelimiter, 3))
    put(at, scrambled(clear))
  }

  val number = Seq("CTRL", "DELIMITER", "ALLOC_MAP", "PORT_MAP", "ID", "LASERS", "REGEN", "I2C", "STATUS", "ERRORS", "DS_LOST",
    "US_LOST", "US_OVERFLOWS", "US_LEVEL", "RX").zipWithIndex.toMap
  def write(name: String, value: Long) = Seq(0x5A, 0x80 | number(name)) ++ (0 until 4).map(i => ((value >> (8 * i)) & 0xFF).toInt)
  def read(name: String) = Seq(0x5A, number(name), 0, 0, 0, 0)

  val toDevice = mutable.Queue[Int]()
  val fromDevice = mutable.ArrayBuffer[Int]()
  val dsOut, usOut = mutable.ArrayBuffer[Int]()
  val ben = mutable.ArrayBuffer[Boolean]() // per half-word
  var zeros = 0 // the dut's
  var laserBeforeData, benWithoutLaser = false
  var sdaLowSeen = false
  var i2cWrites = mutable.ArrayBuffer[Long]() // out clock of each change of the lines

  val compiled = SimConfig.compile(InlineSniffer(steadyBits = 8))
  compiled.doSim(3) { dut =>
    zeros = dut.zeros
    dut.io.dsRx #= 0
    dut.io.usRx #= 0
    dut.io.dsSignal #= true
    dut.io.usb.rx.valid #= false
    dut.io.usb.tx.ready #= false
    dut.io.scl #= true
    dut.io.usAbove #= false
    dut.io.burstCut #= false
    dut.io.sda #= false
    var dsWord, usWord, outClock = 0
    dut.dsCd.onSamplings {
      dut.io.dsRx #= (0 until 20).map(k => BigInt(if (20 * dsWord + k < dsBits.length) dsBits(20 * dsWord + k) else 0) << k).sum
      dsWord += 1
    }
    dut.clockDomain.onSamplings {
      val ds = dut.io.dsTx.toBigInt
      if (ds != 0 && !dut.io.dsLaser.toBoolean) laserBeforeData = true
      if (dut.io.dsLaser.toBoolean) dsOut ++= (0 until 20).map(k => ((ds >> k) & 1).toInt)
    }
    dut.usCd.onSamplings { // 4 samples per bit
      dut.io.usRx #= (0 until 40).map(k => BigInt(if (10 * usWord + k / 4 < usBits.length) usBits(10 * usWord + k / 4) else 0) << k).sum
      usWord += 1
      // the comparator: over its level during the first and the last burst
      dut.io.usAbove #= Seq(bursts.head, bursts.last).exists { case (at, clear) => 10 * usWord >= at && 10 * usWord < at + 8 * clear.length }
      if (dut.io.usBen.toInt != 0 && !dut.io.usLaser.toBoolean) benWithoutLaser = true
      val us = dut.io.usTx.toBigInt
      usOut ++= (0 until 40).map(k => ((us >> k) & 1).toInt)
      ben += ((dut.io.usBen.toInt >> 1) & 1) == 1
      ben += (dut.io.usBen.toInt & 1) == 1
    }
    // the USB host: takes a byte when it can (stalls at random), sends its bytes in order
    dut.usbCd.onSamplings {
      if (dut.io.usb.tx.valid.toBoolean && dut.io.usb.tx.ready.toBoolean) fromDevice += dut.io.usb.tx.payload.toInt
      if (dut.io.usb.rx.valid.toBoolean && dut.io.usb.rx.ready.toBoolean) toDevice.dequeue()
      dut.io.usb.tx.ready #= rand.nextInt(4) != 0
      dut.io.usb.rx.valid #= toDevice.nonEmpty
      if (toDevice.nonEmpty) dut.io.usb.rx.payload #= toDevice.head
    }
    var lines = (false, false)
    dut.outCd.onSamplings {
      outClock += 1
      val now = (dut.io.sclLow.toBoolean, dut.io.sdaLow.toBoolean)
      if (now != lines) i2cWrites += outClock
      lines = now
      if (now._2) sdaLowSeen = true
    }
    dut.clockDomain.forkStimulus(20)
    fork { sleep(7); dut.dsCd.forkStimulus(20) }
    dut.outCd.forkStimulus(32)
    fork { sleep(3); dut.usCd.forkStimulus(20) }
    fork { sleep(5); dut.usbCd.forkStimulus(42) }
    dut.outCd.waitSampling(20)
    toDevice ++= Seq(0x11, 0x22) ++ read("ID") // bytes before a command are skipped
    toDevice ++= write("CTRL", 0x10501) ++ write("LASERS", 3) ++ write("DELIMITER", usDelimiter.toLong)
    toDevice ++= DsLine.allocIds.flatMap(id => write("ALLOC_MAP", 0x1000 | id)) ++ DsLine.portIds.flatMap(id => write("PORT_MAP", 0x1000 | id))
    toDevice ++= write("I2C", 1) ++ write("I2C", 3) ++ write("I2C", 2) ++ read("I2C") ++ write("I2C", 3)
    dut.clockDomain.waitSampling(dsBits.length / 20 - 6000) // the link is empty by then
    toDevice ++= Seq("STATUS", "ERRORS", "DS_LOST", "US_LOST", "US_OVERFLOWS", "US_LEVEL").flatMap(read) ++ write("US_LEVEL", 0) ++ read("US_LEVEL")
    dut.clockDomain.waitSampling(7000)
  }

  val records = mutable.ArrayBuffer[Record]()
  var at = 0
  var padError = false
  while (at + 12 <= fromDevice.length && (fromDevice(at) & 0xF8) == 0xA0) {
    val length = fromDevice(at + 2) | (fromDevice(at + 3) << 8)
    records += Record(fromDevice(at) & 7, fromDevice(at + 1), bytesToBig(fromDevice.slice(at + 4, at + 12).reverse.toSeq), fromDevice.slice(at + 12, at + 12 + length).toSeq)
    val padded = (12 + length + 3) & ~3
    if (fromDevice.slice(at + 12 + length, at + padded).exists(_ != 0)) padError = true
    at += padded
  }
  check(at == fromDevice.length && records.nonEmpty && !padError, s"link: ${fromDevice.length} bytes, all of them whole records padded with zeros to 4 bytes (${records.length})")
  val replies = records.filter(_.kind == RecordKind.reply).map(r => (r.payload.head, bytesToBig(r.payload.drop(1).reverse).toLong))
  check(replies.map(_._1) == Seq("ID", "I2C", "STATUS", "ERRORS", "DS_LOST", "US_LOST", "US_OVERFLOWS", "US_LEVEL", "US_LEVEL").map(number), s"registers: one reply per read, in order (${replies.map(_._1)})")
  check(replies(0)._2 == 0x47530001L, f"registers: identity (${replies(0)._2}%x)")
  check(replies(1)._2 == 0x102 && sdaLowSeen, f"I2C: the lines written and read back (${replies(1)._2}%x)")
  check(i2cWrites.length == 4 && i2cWrites.zip(i2cWrites.drop(1)).forall { case (a, b) => b - a >= 256 }, s"I2C: line changes 256 clocks apart (${i2cWrites.mkString(" ")})")
  check((replies(2)._2 & 0xF) == 0xD && math.abs((replies(2)._2 >> 16) - 0x8000) < 0x2000, f"status: synced, no FEC, signal, downstream on, the VCXO duty near its middle (${replies(2)._2}%x)")
  check(replies.slice(3, 7).forall(_._2 == 0), s"no error, record lost or burst left out (${replies.slice(3, 7).map(_._2)})")
  check(replies(7)._2 == 0x0203 && replies(8)._2 == 0 && !benWithoutLaser, f"upstream level: 3 bursts, 2 over the comparator's level, cleared by a write (${replies(7)._2}%x)")

  def body(r: Record) = (r.flags, r.payload)
  for ((kind, what) <- Seq((RecordKind.ploam, "PLOAM"), (RecordKind.alloc, "BWmap"), (RecordKind.gem, "GEM"))) {
    val all = records.filter(_.kind == kind).map(body).toSeq
    // an idle frame: one broadcast PLOAM, the first as the one of the reference's last frame, cut here
    val g = if (kind == RecordKind.ploam) all.dropRight(idleFrames - 1) else all
    val e = expect.filter(_.kind == kind).map(body)
    check((g.endsWith(e) || e.endsWith(g)) && (g.length - e.length).abs <= 1 && g.nonEmpty, s"downstream: $what records as the byte-wide chain's (${g.length} of ${e.length})")
  }
  val got = records.filter(_.kind == RecordKind.burst)
  check(got.length == bursts.length, s"upstream: one record per burst (${got.length})")
  for (((at, clear), r) <- bursts.zip(got)) {
    val keep = scrambled(clear).lastIndexWhere(_ != 0) + 1
    check(r.payload == clear.take(keep) && r.flags == 0 && (r.time - at / 4).abs < 500, s"upstream: burst at bit $at, ${r.payload.length} clear bytes, time ${r.time} (line ${at / 4})")
  }
  val times = records.filterNot(_.kind == RecordKind.reply).map(_.time)
  check(times.forall(_ > 0), "record times set")

  check(!laserBeforeData && dsOut.length > dsBits.length - 100000, s"downstream: nothing sent with the laser off, then on (${dsOut.length} bits)")
  val from = 8 * (line.indexOfSlice(DsLine.psyncBytes) + 4 * frameBytes) // a PSync and its Ident: no other frame has them
  val found = dsOut.indexOfSlice(dsBits.slice(from, from + 64))
  check(found >= 0 && dsOut.slice(found, found + 1000000) == dsBits.slice(from, from + 1000000).toSeq, "downstream: sent again bit for bit")
  check((0 until usOut.length by 4).forall(k => usOut.slice(k, k + 4).distinct.length == 1), "upstream: each bit four samples on the lane")
  val usSent = (0 until usOut.length by 4).map(usOut(_))
  for ((at, clear) <- bursts) {
    val whole = usBits.slice(at - 24, at + 8 * clear.length).toSeq // from the delimiter
    val bits = whole.take(whole.lastIndexOf(1) + 1) // its trailing zeros are the laser off, whatever the lane carries
    val sent = usSent.indexOfSlice(bits)
    // the enable is a clock (two half-words) behind the data here (the SerDes adds its own in the card), on to the last one
    check(sent >= 0 && (sent / 5 + 2 to (sent + bits.length - 1) / 5 + 2).forall(ben(_)), s"upstream: burst at bit $at sent again from its delimiter to its last one, laser on")
  }
  // the enable is two half-words behind the data here: bit i is off when its half-word's enable is; after a burst its
  // first `zeros` half-words off carry zeros, the others 1010
  def off(i: Int, back: Int) = { val j = i / 5 + 2 - back; j < 0 || j < ben.length && !ben(j) } // off before the start
  def dark(i: Int) = (0 to zeros).forall(off(i, _))
  check((0 until usSent.length - 1).forall(i => !(dark(i) && dark(i + 1)) || usSent(i) != usSent(i + 1)), "upstream: 1010 on the lane while the laser is off")
  check(usSent.indices.forall(i => !off(i, 0) || dark(i) || usSent(i) == 0), "upstream: zeros on the lane just after a burst")
  val lit = ben.count(identity) * 5
  val need = bursts.map(b => 8 * (b._2.length + 11)).sum
  check(lit >= need && lit < need + bursts.length * 200, s"upstream: laser on for the bursts only ($lit bits for $need)")

  // both receivers inverted (RX = 3) and their lines too: the same downstream bits and the same burst leave the box
  val (burstAt, burstClear) = bursts.head
  val usFrom = burstAt - 20000
  val dsInverted, usInverted = mutable.ArrayBuffer[Int]()
  compiled.doSim("inverted", 4) { dut =>
    val commands = mutable.Queue[Int]() ++ write("RX", 3) ++ write("LASERS", 3)
    dut.io.dsRx #= (BigInt(1) << 20) - 1
    dut.io.usRx #= (BigInt(1) << 40) - 1
    dut.io.dsSignal #= true
    dut.io.usb.rx.valid #= false
    dut.io.usb.tx.ready #= true
    dut.io.scl #= true
    dut.io.sda #= true
    dut.io.usAbove #= false
    dut.io.burstCut #= false
    var dsWord, usWord = 0
    dut.dsCd.onSamplings {
      dut.io.dsRx #= (0 until 20).map(k => BigInt(1 - dsBits(20 * dsWord + k)) << k).sum
      dsWord += 1
    }
    dut.clockDomain.onSamplings(if (dut.io.dsLaser.toBoolean) dsInverted ++= (0 until 20).map(k => ((dut.io.dsTx.toBigInt >> k) & 1).toInt))
    dut.usCd.onSamplings {
      dut.io.usRx #= (0 until 40).map(k => BigInt(1 - usBits(usFrom + 10 * usWord + k / 4)) << k).sum
      usWord += 1
      usInverted ++= (0 until 40 by 4).map(k => ((dut.io.usTx.toBigInt >> k) & 1).toInt)
    }
    dut.usbCd.onSamplings {
      if (dut.io.usb.rx.valid.toBoolean && dut.io.usb.rx.ready.toBoolean) commands.dequeue()
      dut.io.usb.rx.valid #= commands.nonEmpty
      if (commands.nonEmpty) dut.io.usb.rx.payload #= commands.head
    }
    dut.clockDomain.forkStimulus(20)
    fork { sleep(7); dut.dsCd.forkStimulus(20) }
    dut.outCd.forkStimulus(32)
    fork { sleep(3); dut.usCd.forkStimulus(20) }
    fork { sleep(5); dut.usbCd.forkStimulus(42) }
    dut.usCd.waitSampling(2000 + burstClear.length + 400)
  }
  val dsAt = dsInverted.indexOfSlice(dsBits.slice(30000, 30064))
  check(dsAt >= 0 && dsInverted.slice(dsAt, dsAt + 10000) == dsBits.slice(30000, 40000).toSeq, "inverted receivers: downstream sent again bit for bit")
  val burstBits = usBits.slice(burstAt - 24, burstAt + 8 * burstClear.length).toSeq
  check(usInverted.indexOfSlice(burstBits.take(burstBits.lastIndexOf(1) + 1)) >= 0, "inverted receivers: the burst sent again from its delimiter")
  println("PASS InlineSnifferTest")
}
