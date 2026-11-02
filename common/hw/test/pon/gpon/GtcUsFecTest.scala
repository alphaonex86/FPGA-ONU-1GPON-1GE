// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.core.sim._
import scala.collection.mutable
import GponCodes._
import GtcDsTest.{check, bytesToBig, bigToBytes}

/** ONU upstream framer with FEC (G.984.3 13.3): bursts parsed back by a Scala OLT model, parity checked per codeword
  * (shorter last one, fewer than 17 bytes: zeros), the content sized to the data bytes, BIP without parity; two GEM
  * streams in their own allocations, each packet continued in its stream's next one. */
object GtcUsFecTest extends App {
  case class Alloc(allocId: Int, flags: Int, bytes: Int)
  case class Burst(start: Int, fec: Boolean, allocs: Seq[Alloc], data: Seq[Int]) // data bytes of each allocation
  val gemAllocId = 0x55
  val dataAllocId = 0x77
  val onuId = 0x2A
  val ploamU = 0x400
  val useFec = 0x200
  val bursts = Seq(
    Burst(1000, fec = true, Seq(Alloc(gemAllocId, ploamU | useFec | 0x080, 600)), Seq(600 - 48)), // 3 codewords
    Burst(3000, fec = true, Seq(Alloc(gemAllocId, useFec | 0x080, 507), Alloc(dataAllocId, useFec | 0x100, 300)),
      Seq(507 - 32, 300 - 32)), // the first ends with the second codeword's parity
    Burst(5000, fec = true, Seq(Alloc(gemAllocId, useFec, 262)), Seq(236)), // last codeword 10 bytes: zeros
    Burst(7000, fec = false, Seq(Alloc(gemAllocId, 0x080, 100), Alloc(dataAllocId, 0, 200)), Seq(100, 200)))
  val rnd = new scala.util.Random(3)
  def random(sizes: (Int, Int)*): Seq[(Int, Seq[Int])] = sizes.map { case (p, n) => p -> Seq.fill(n)(rnd.nextInt(256)) }
  val streams = Seq(gemAllocId -> random(0x101 -> 200, 0x102 -> 300, 0x103 -> 700, 0x104 -> 40),
    dataAllocId -> random(0x201 -> 350)) // over both of its allocations
  val packets = streams.flatMap(_._2)
  val reportOf = Seq(0x5A, 0xC3) // each stream's DBRu octet
  val message = BigInt(96, rnd)

  SimConfig.compile(GtcUsFramer(streams.length)).doSim { dut =>
    val io = dut.io
    val cd = dut.clockDomain
    io.enable #= true
    io.onuId #= onuId
    io.snMessage #= 0
    val ov = io.overhead
    ov.guard #= 32; ov.type1 #= 0; ov.type2 #= 0; ov.pattern #= 0xAA; ov.delimiter #= usDelimiter; ov.bytes #= 8
    for (((allocId, ps), k) <- streams.zipWithIndex) {
      io.gemAllocId(k).valid #= true
      io.gemAllocId(k).payload #= allocId
      io.dbru(k) #= reportOf(k)
      new GtcDsTest.GemDriver(io.gem(k), cd).queue ++= ps
    }
    val ploam = new GtcDsTest.PloamDriver(io.ploam, cd)
    ploam.queue += message
    val jobs = mutable.Queue[(Alloc, Int)]()
    for (b <- bursts) b.allocs.foldLeft(b.start) { (at, a) => jobs += ((a, at)); at + a.bytes }
    io.job.valid #= false
    var time = 0
    val line = mutable.ArrayBuffer[(Int, Int)]() // line byte, burst enable
    cd.onSamplings {
      if (io.job.valid.toBoolean && io.job.ready.toBoolean) jobs.dequeue()
      io.job.valid #= jobs.nonEmpty
      for ((a, at) <- jobs.headOption) {
        io.job.alloc.allocId #= a.allocId
        io.job.alloc.flags #= a.flags
        io.job.alloc.start #= 0
        io.job.alloc.stop #= a.bytes - 1
        io.job.bit #= at * 8
        io.job.sn #= false
        io.job.random #= 0
      }
      time += 1
      io.time #= time
      line += ((io.line.toInt, io.ben.toInt))
    }
    cd.forkStimulus(10)
    cd.waitSampling(bursts.last.start + 400)

    // bursts on the line: preamble byte and delimiter, then the scrambled bytes from the BIP
    val runs = mutable.ArrayBuffer[Seq[Int]]()
    var i = 0
    while (i < line.length) {
      if (line(i)._2 == 0xFF) {
        val j = line.indexWhere(_._2 == 0, i)
        runs += line.slice(i, j).map(_._1).toSeq
        i = j
      } else i += 1
    }
    check(runs.length == bursts.length && runs.forall(r => bytesToBig(r.slice(1, 4)) == usDelimiter), s"${runs.length} bursts")
    val got = mutable.Map[Int, Seq[Int]]().withDefaultValue(Nil) // GEM payload by port
    var badCodewords, badFields, earlyIdle = 0
    var previousData = Seq[Int]()
    for ((b, run) <- bursts.zip(runs)) {
      val total = 3 + b.allocs.map(_.bytes).sum
      val clear = run.drop(4).zip(Model.keystream(total)).map { case (x, k) => x ^ k }
      check(run.length == 4 + total, s"burst at ${b.start}: $total bytes from the BIP")
      val lastLength = total - 255 * ((total - 1) / 255)
      def parity(p: Int) = b.fec && (p % 255 >= fecDataBytes || p >= total - scala.math.min(lastLength, 16))
      for (cw <- clear.grouped(255)) {
        val ok = if (cw.length >= 17) cw.takeRight(16) == Model.rsParity(cw.dropRight(16)) else cw.forall(_ == 0)
        if (b.fec && !ok) badCodewords += 1
      }
      val data = clear.indices.filter(p => !parity(p)).map(clear)
      check(data(0) == previousData.foldLeft(0)(_ ^ _) && data(1) == onuId && (data(2) & 0x40) == (if (b.fec) 0x40 else 0),
        s"burst at ${b.start}: BIP of the last burst's data bytes, ONU-ID, Ind FEC ${b.fec}")
      previousData = data.drop(1)
      var at = 3
      val allocData = for ((a, n) <- b.allocs.zip(b.data)) yield {
        val in = (at until at + a.bytes).filter(p => !parity(p)).map(clear)
        at += a.bytes
        check(in.length == n, s"allocation ${a.allocId.toHexString} of ${a.bytes} bytes: $n data bytes")
        (a, in)
      }
      for ((a, in) <- allocData) {
        var k = 0
        def field(n: Int): Seq[Int] = { val f = in.slice(k, k + n); k += n; if (Model.crc8(f.init) != f.last) badFields += 1; f }
        if ((a.flags & ploamU) != 0 && bytesToBig(field(13).init) != message) badFields += 1
        val report = streams.indexWhere(_._1 == a.allocId)
        if ((a.flags & 0x180) == 0x080 && field(2).head != reportOf(report)) badFields += 1
        if ((a.flags & 0x180) == 0x100 && field(3).init != Seq(0, reportOf(report))) badFields += 1
        while (in.length - k >= 5) {
          val header = bytesToBig(in.slice(k, k + 5)) ^ gemMask
          val (pli, port, pti) = ((header >> 28).toInt, ((header >> 16) & 0xFFF).toInt, ((header >> 13) & 7).toInt)
          if (Model.gemHeader(pli, port, pti) != header) badFields += 1
          for ((allocId, ps) <- streams if allocId == a.allocId)
            if (pli == 0 && ps.map(p => got(p._1).length).sum < ps.map(_._2.length).sum) earlyIdle += 1
          got(port) = got(port) ++ in.slice(k + 5, k + 5 + pli)
          k += 5 + pli
        }
        if (k > in.length || !in.drop(k).forall(_ == 0)) badFields += 1
      }
    }
    check(badCodewords == 0, "every codeword's parity right, the last one shorter")
    check(badFields == 0, "PLOAMu, DBRu (mode 0, mode 1 green) with their CRC, GEM HEC, the content filling each allocation's data bytes")
    check(packets.forall { case (p, d) => got(p) == d } && earlyIdle == 0,
      s"${packets.length} GEM packets of 2 streams fragmented over their allocations' data bytes, no idle frame while one is pending")
  }
  println("PASS GtcUsFecTest")
}
