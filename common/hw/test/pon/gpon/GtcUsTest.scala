// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.core.sim._
import scala.collection.mutable
import GponCodes._
import GtcDsTest.{check, bytesToBig, bigToBytes}

/** OLT upstream burst receiver against a Scala ONU model: bursts placed on the line bit by bit. */
object GtcUsTest extends App {
  case class Alloc(allocId: Int, flags: Int, ploam: Seq[Int], dba: Seq[Int], packets: Seq[(Int, Seq[Int])]) {
    // PLOAMu, DBRu, GEM frames, then idle GEM frames to `bytes`
    val content: Seq[Int] =
      (if (ploam.nonEmpty) ploam :+ Model.crc8(ploam) else Nil) ++ (if (dba.nonEmpty) dba :+ Model.crc8(dba) else Nil) ++
        packets.flatMap { case (port, data) => bigToBytes(Model.gemHeader(data.length, port, 1) ^ gemMask, 5) ++ data }
    val bytes = content.length + 10
    val line = content ++ Seq.fill(2)(bigToBytes(gemMask, 5)).flatten
  }
  /** One ONU burst: grants start at `time`; `delay` bits late on the line; `send` false: ONU silent. */
  case class Burst(time: Int, onuId: Int, ind: Int, allocs: Seq[Alloc], delay: Int = 0, window: Int = 4,
                   send: Boolean = true, delimiterFlips: Int = 0, dataFlip: Option[Int] = None)

  val rnd = new scala.util.Random(7)
  def data(n: Int) = Seq.fill(n)(rnd.nextInt(256))
  val ploamSn = Seq(0xFF, 0x01) ++ data(10)
  val ploam1 = Seq(0x01, 0x09) ++ data(10)
  val bursts = {
    val b1 = Burst(200, 1, 0x80, Seq(Alloc(0x101, 0x480, ploam1, Seq(0x42), Seq((0x10, data(64)), (0x11, data(30)))),
      Alloc(0x102, 0x100, Nil, Seq(0x12, 0x34), Seq((0x12, data(100))))))
    val b2 = Burst(b1.time + b1.allocs.map(_.bytes).sum + 40, 2, 0, Seq(Alloc(0x201, 0, Nil, Nil, Seq((0x20, data(200))))),
      delay = 3, dataFlip = Some(100))
    val b3 = Burst(b2.time + b2.allocs.head.bytes + 40, 3, 0, Seq(Alloc(0x301, 0, Nil, Nil, Seq((0x30, data(20)))),
      Alloc(0x302, 0, Nil, Nil, Seq((0x31, data(20))))), send = false)
    val b4 = Burst(b3.time + b3.allocs.map(_.bytes).sum + 40, 1, 0, Seq(Alloc(0x103, 0, Nil, Nil, Seq((0x13, data(50))))),
      delay = -2, delimiterFlips = 1)
    val b5 = Burst(b4.time + b4.allocs.head.bytes + 40, 2, 0, Seq(Alloc(0x202, 0x080, Nil, Seq(0x07), Seq((0x21, data(40))))))
    val b6 = Burst(b5.time + b5.allocs.head.bytes + 100, 0xFF, 0, Seq(Alloc(0x3FE, 0x400, ploamSn, Nil, Nil)),
      delay = 100, window = 40) // serial number response, ranging-like window
    Seq(b1, b2, b3, b4, b5, b6)
  }

  // line bits: laser off (0), preamble 0xAA, delimiter, then the scrambled burst
  val totalBytes = bursts.last.time + 400
  val bits = Array.fill(totalBytes * 8)(0)
  def put(bit: Int, bytes: Seq[Int]): Unit =
    for ((b, i) <- bytes.zipWithIndex; k <- 0 until 8) bits(bit + 8 * i + k) = (b >> (7 - k)) & 1
  for (b <- bursts if b.send) {
    val bipFirst = (b.time - 3) * 8 + b.delay
    val clear = Seq(0 /* BIP, set below */, b.onuId, b.ind) ++ b.allocs.flatMap(_.line)
    put(bipFirst - 11 * 8, Seq.fill(8)(0xAA))
    put(bipFirst - 3 * 8, bigToBytes(usDelimiter, 3))
    for (k <- 0 until b.delimiterFlips) bits(bipFirst - 20 + 7 * k) ^= 1
    put(bipFirst, clear.zip(Model.keystream(clear.length)).map { case (c, k) => c ^ k })
    b.dataFlip.foreach(i => bits(bipFirst + 8 * (3 + i) + 2) ^= 1)
  }
  // BIP: XOR of the clear bytes this ONU sent since its last BIP (line bit errors excluded)
  val running = mutable.Map[Int, Int]()
  for (b <- bursts if b.send) {
    val bipFirst = (b.time - 3) * 8 + b.delay
    val bip = running.getOrElse(b.onuId, 0)
    put(bipFirst, Seq(bip ^ Model.keystream(1).head))
    running(b.onuId) = (Seq(b.onuId, b.ind) ++ b.allocs.flatMap(_.line)).reduce(_ ^ _)
  }
  def lineByte(t: Int): Int = if (t < 0 || t >= totalBytes) 0 else (0 until 8).foldLeft(0)((a, k) => (a << 1) | bits(t * 8 + k))

  SimConfig.compile(GtcUsDeframer()).doSim { dut =>
    val cd = dut.clockDomain
    val grants = mutable.Queue[(Alloc, Int, Int)]()
    for (b <- bursts) {
      var t = b.time
      for (a <- b.allocs) { grants += ((a, t, b.window)); t += a.bytes }
    }
    val gotBursts = mutable.ArrayBuffer[(Boolean, Int, Int, Int, Long, Int)]()
    val gotPloams = mutable.ArrayBuffer[(BigInt, Boolean)]()
    val gotDbrus = mutable.ArrayBuffer[(Int, Int, Boolean)]()
    val gotPackets = mutable.ArrayBuffer[(Int, Int, Seq[Int])]()
    val packet = mutable.ArrayBuffer[Int]()
    var cycle = 0
    dut.io.delimiter #= usDelimiter
    dut.io.delimiterErrors #= 1
    dut.io.grant.valid #= false
    dut.io.time #= 0
    dut.io.line #= 0
    cd.onSamplings {
      val g = dut.io.grant
      if (g.valid.toBoolean && g.ready.toBoolean) grants.dequeue()
      g.valid #= grants.nonEmpty
      if (grants.nonEmpty) {
        val (a, t, w) = grants.head
        g.alloc.allocId #= a.allocId
        g.alloc.flags #= a.flags
        g.alloc.start #= t % usFrameBytes
        g.alloc.stop #= t % usFrameBytes + a.bytes - 1
        g.time #= t
        g.window #= w
      }
      val io = dut.io
      if (io.burst.valid.toBoolean) gotBursts += ((io.burst.found.toBoolean, io.burst.allocId.toInt, io.burst.onuId.toInt,
        io.burst.ind.toInt, io.burst.delayBits.toLong, io.burst.bipErrors.toInt))
      if (io.ploam.valid.toBoolean) gotPloams += ((io.ploam.value.toBigInt, io.ploam.crcOk.toBoolean))
      if (io.dbru.valid.toBoolean) gotDbrus += ((io.dbru.allocId.toInt, io.dbru.report.toInt, io.dbru.crcOk.toBoolean))
      if (io.gem.valid.toBoolean) {
        packet += io.gem.gem.data.toInt
        if (io.gem.last.toBoolean) {
          gotPackets += ((io.gem.allocId.toInt, io.gem.gem.portId.toInt, packet.toSeq))
          packet.clear()
        }
      }
      cycle += 1
      io.time #= cycle
      io.line #= lineByte(cycle)
    }
    cd.forkStimulus(10)
    cd.waitSamplingWhere(cycle >= totalBytes + 20)

    val Seq(b1, b2, b3, b4, b5, b6) = bursts
    check(gotBursts.map(b => (b._1, b._2, b._3, b._4, b._5)) == Seq(
      (true, 0x101, 1, 0x80, 0L), (true, 0x201, 2, 0, 3L), (false, 0x301, gotBursts(2)._3, gotBursts(2)._4, gotBursts(2)._5),
      (true, 0x103, 1, 0, -2L), (true, 0x202, 2, 0, 0L), (true, 0x3FE, 0xFF, 0, 100L)),
      "bursts found, ONU-ID, Ind, delay in bits (0, +3, lost, -2 with a delimiter bit error, 0, +100 in a wide window)")
    check(gotBursts(3)._6 == 0 && gotBursts(4)._6 == 1, s"BIP against the last burst of the ONU: clean 0, after a bit error 1 " +
      s"(got ${gotBursts(3)._6}, ${gotBursts(4)._6})")
    check(gotPloams == Seq((bytesToBig(ploam1), true), (bytesToBig(ploamSn), true)), "PLOAMu messages with a good CRC")
    check(gotDbrus == Seq((0x101, 0x42, true), (0x102, 0x1234, true), (0x202, 0x07, true)), "DBRu mode 0 and mode 1 with a good CRC")
    val sent = for (b <- bursts if b.send && b != b2; a <- b.allocs; (port, d) <- a.packets) yield (a.allocId, port, d)
    check(gotPackets.filter(_._1 != 0x201) == sent, s"${sent.length} GEM packets per allocation, contiguous allocations")
    check(gotPackets.count(_._1 == 0x201) == 1 && gotPackets.find(_._1 == 0x201).get._3 != b2.allocs.head.packets.head._2,
      "the packet with the flipped bit is delivered, corrupted")
    check(dut.io.hecErrors.toLong == 0 && dut.io.gemCuts.toLong == 0 && grants.isEmpty, "no HEC error, no cut, all grants used")
  }
  println("PASS GtcUsTest")
}
