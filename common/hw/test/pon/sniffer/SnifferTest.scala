// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.collection.mutable
import pon.gpon._
import pon.gpon.GponCodes._
import pon.gpon.GtcDsTest.{check, bigToBytes, bytesToBig, GemDriver, PloamDriver, writeBwmap}

/** OLT framer -> 3-bit delay -> downstream sniffer. Test only. */
case class SnifferDsBench() extends Component {
  val framer = GtcDsFramer()
  val sniffer = SnifferDs()
  val io = new Bundle {
    val ploam = slave(cloneOf(framer.io.ploam))
    val bwmapWrite = slave(cloneOf(framer.io.bwmapWrite))
    val bwmapCommit = slave(cloneOf(framer.io.bwmapCommit))
    val gem = slave(cloneOf(framer.io.gem))
    val time = in UInt (64 bits)
    val onuId = in Bits (8 bits)
    val allPloams = in Bool ()
    val allocMap = slave Flow (KeepWrite())
    val portMap = slave Flow (KeepWrite())
    val synced = out Bool ()
    val output = master Stream (Fragment(Bits(8 bits)))
    val drops, truncations = out UInt (32 bits)
  }
  framer.io.ploam << io.ploam
  framer.io.bwmapWrite << io.bwmapWrite
  framer.io.bwmapCommit << io.bwmapCommit
  framer.io.gem << io.gem
  framer.io.fec := False
  val previous = RegNext(framer.io.line) init (0)
  sniffer.io.line := (previous ## framer.io.line)(12 downto 5)
  sniffer.io.time := io.time
  sniffer.io.onuId := io.onuId
  sniffer.io.allPloams := io.allPloams
  sniffer.io.allocMap << io.allocMap
  sniffer.io.portMap << io.portMap
  io.synced := sniffer.io.synced
  io.output << sniffer.io.output
  io.drops := sniffer.io.drops
  io.truncations := sniffer.io.truncations
  framer.io.frameStart.simPublic()
}

/** Downstream filter against the OLT framer, upstream bursts placed on the line bit by bit. Throws on failure. */
object SnifferTest extends App {
  case class Record(kind: Int, flags: Int, time: BigInt, payload: Seq[Int])

  /** Collects the record stream, ready 3 clocks in 4; checks the framing of every record. */
  class RecordSink(s: Stream[Fragment[Bits]], cd: ClockDomain, seed: Int) {
    val records = mutable.ArrayBuffer[Record]()
    private val bytes = mutable.ArrayBuffer[Int]()
    private val rand = new scala.util.Random(seed)
    s.ready #= false
    cd.onSamplings {
      if (s.valid.toBoolean && s.ready.toBoolean) {
        bytes += s.fragment.toInt
        if (s.last.toBoolean) {
          val length = bytes(2) | (bytes(3) << 8)
          if ((bytes(0) & 0xF8) != 0xA0 || bytes.length != 12 + length) check(false, s"record framing (${bytes.take(4)}, ${bytes.length} bytes)")
          records += Record(bytes(0) & 7, bytes(1), bytesToBig(bytes.slice(4, 12).reverse.toSeq), bytes.drop(12).toSeq)
          bytes.clear()
        }
      }
      s.ready #= rand.nextInt(4) != 0
    }
  }

  def writeMap(f: Flow[KeepWrite], ids: Seq[Int], cd: ClockDomain): Unit = {
    for (id <- ids) { f.valid #= true; f.id #= id; f.keep #= true; cd.waitSampling() }
    f.valid #= false
  }

  // downstream: ONU-ID 5, Alloc-ID 0x105, Port-IDs 0x100 and 0x101 kept; the rest is another ONU's
  SimConfig.compile(SnifferDsBench()).doSim { dut =>
    val cd = dut.clockDomain
    val gem = new GemDriver(dut.io.gem, cd)
    val ploam = new PloamDriver(dut.io.ploam, cd)
    val sink = new RecordSink(dut.io.output, cd, 1)
    dut.io.bwmapWrite.valid #= false
    dut.io.bwmapCommit.valid #= false
    dut.io.allocMap.valid #= false
    dut.io.portMap.valid #= false
    dut.io.onuId #= 5
    dut.io.allPloams #= false
    var frameStarts = 0
    var cycle = 0L
    cd.onSamplings {
      cycle += 1
      dut.io.time #= cycle
      if (dut.framer.io.frameStart.toBoolean) frameStarts += 1
    }
    cd.forkStimulus(10)
    writeMap(dut.io.allocMap, Seq(0x105), cd)
    writeMap(dut.io.portMap, Seq(0x100, 0x101), cd)
    val rand = new scala.util.Random(3)
    val sentPloams = Seq(5, 7, 0xFF, 5, 9).map(onu => (BigInt(onu) << 88) | BigInt(88, rand))
    val sentPackets = Seq.tabulate(40)(i => (Seq(0x100, 0x200, 0x101, 0x300)(i % 4), Seq.fill(Seq(64, 1518, 5000, 7, 300)(i % 5))(rand.nextInt(256))))
    val allocs = Seq((0x105, 0x080, 0x0100, 0x0400), (0x205, 0x000, 0x0500, 0x0900), (0x106, 0x000, 0x0A00, 0x0B00))
    cd.waitSamplingWhere(dut.io.synced.toBoolean)
    cd.waitSamplingWhere(frameStarts == 3)
    writeBwmap(dut.io.bwmapWrite, dut.io.bwmapCommit, allocs, cd)
    ploam.queue ++= sentPloams
    gem.queue ++= sentPackets
    cd.waitSamplingWhere(frameStarts == 10)
    cd.waitSampling(2000)

    val ploams = sink.records.filter(_.kind == RecordKind.ploam)
    val expectPloams = sentPloams.filter(p => Seq(5, 0xFF).contains((p >> 88).toInt))
    check(ploams.forall(r => r.flags == 0 && r.payload.length == 12), "PLOAM records: 12 bytes, CRC good")
    check(ploams.map(r => bytesToBig(r.payload)).filter(_ != noMessage) == expectPloams,
      "PLOAMs to ONU-ID 5 and broadcast kept, in order; 7 and 9 dropped")
    val got = sink.records.filter(_.kind == RecordKind.alloc).map(_.payload)
    val (id, flags, start, stop) = allocs.head
    check(got.length == 1 && got.head == bigToBytes((BigInt(id) << 44) | (BigInt(flags) << 32) | (start << 16) | stop, 7),
      s"BWmap: only Alloc-ID 0x105, 7 bytes as on the line (${got.length} records)")
    val gems = sink.records.filter(_.kind == RecordKind.gem)
    def port(r: Record) = (r.payload(0) << 8) | r.payload(1)
    def pli(r: Record) = (r.payload(2) << 8) | r.payload(3)
    check(gems.forall(r => Seq(0x100, 0x101).contains(port(r)) && r.payload.length == 10 + pli(r) && (r.flags & 0xF8) == 0),
      s"GEM records: ports 0x100 and 0x101 only, PLI bytes each, not cut (${gems.length} fragments)")
    val counters = gems.map(r => bytesToBig(r.payload.slice(4, 10)))
    check(counters.zip(counters.drop(1)).forall { case (a, b) => a < b }, "GEM crypto-counters increase")
    val packets = mutable.ArrayBuffer[(Int, Seq[Int])]()
    val partial = mutable.Map[Int, Seq[Int]]()
    for (r <- gems) {
      val data = partial.getOrElse(port(r), Nil) ++ r.payload.drop(10)
      if ((r.flags & 1) == 1) { packets += ((port(r), data)); partial.remove(port(r)) } else partial(port(r)) = data
    }
    check(packets == sentPackets.filter(p => p._1 == 0x100 || p._1 == 0x101),
      s"GEM packets of ports 0x100 and 0x101 rebuilt from their fragments (${packets.length})")
    val times = sink.records.map(_.time)
    check(times.forall(t => t > 0 && t <= cycle), "record times inside the run")
    check(dut.io.drops.toLong == 0 && dut.io.truncations.toLong == 0, "no record dropped or truncated")
  }

  // writer overflow: 20-byte records into a 16-byte data FIFO, then a third record with both head entries taken
  SimConfig.compile(RecordWriter(2, 16, 2)).doSim { dut =>
    val cd = dut.clockDomain
    dut.io.data.valid #= false
    dut.io.close.valid #= false
    dut.io.output.ready #= false
    cd.forkStimulus(10)
    cd.waitSampling(5)
    for (r <- 0 until 3) {
      for (i <- 0 until 20) { dut.io.data.valid #= true; dut.io.data.payload #= 16 * r + i; cd.waitSampling() }
      dut.io.data.valid #= false
      dut.io.close.valid #= true
      dut.io.close.kind #= 1
      dut.io.close.flags #= 2
      dut.io.close.time #= r
      dut.io.close.extra #= 0xBEEF
      cd.waitSampling()
      dut.io.close.valid #= false
    }
    cd.waitSampling()
    check(dut.io.drops.toLong == 1 && dut.io.truncations.toLong == 2,
      s"overflow: records 1 and 2 truncated, record 3 dropped whole (${dut.io.truncations.toLong}, ${dut.io.drops.toLong})")
    val sink = new RecordSink(dut.io.output, cd, 3)
    cd.waitSampling(200)
    val got = sink.records.map(r => (r.flags, r.time.toInt, r.payload))
    check(got == Seq((0x82, 0, Seq(0xBE, 0xEF) ++ (0 until 16)), (0x82, 1, Seq(0xBE, 0xEF))),
      s"overflow: the first 16 bytes, then the second with no data, both flagged truncated ($got)")
  }

  // upstream: our ONU's bursts, squelched gaps, three bit alignments, a delimiter bit error, a too long burst
  val maxBytes = 300
  val rnd = new scala.util.Random(11)
  case class Burst(at: Int, delay: Int, clear: Seq[Int], delimiterFlip: Boolean = false)
  val bursts = Seq(
    Burst(100, 0, Seq(0x00, 5, 0x80) ++ Seq.fill(200)(rnd.nextInt(256))),
    Burst(500, 3, Seq(0x3C, 5, 0x00) ++ Seq.fill(40)(rnd.nextInt(256)), delimiterFlip = true),
    Burst(700, 7, Seq(0x11, 5, 0x00) ++ Seq.fill(maxBytes + 100)(rnd.nextInt(256))),
    Burst(1300, 5, Seq(0x22, 5, 0x00) ++ Seq.fill(64)(rnd.nextInt(256))))
  val totalBytes = 1600
  val bits = Array.fill(totalBytes * 8)(0)
  def put(bit: Int, bytes: Seq[Int]): Unit =
    for ((b, i) <- bytes.zipWithIndex; k <- 0 until 8) bits(bit + 8 * i + k) = (b >> (7 - k)) & 1
  def scrambled(b: Burst) = b.clear.zip(Model.keystream(b.clear.length)).map { case (c, k) => c ^ k }
  for (b <- bursts) {
    val first = b.at * 8 + b.delay
    put(first - 11 * 8, Seq.fill(8)(0xAA))
    put(first - 3 * 8, bigToBytes(usDelimiter, 3))
    if (b.delimiterFlip) bits(first - 13) ^= 1
    put(first, scrambled(b))
  }
  def lineByte(t: Int): Int = if (t >= totalBytes) 0 else (0 until 8).foldLeft(0)((a, k) => (a << 1) | bits(t * 8 + k))

  SimConfig.compile(SnifferUs(maxBytes = maxBytes)).doSim { dut =>
    val cd = dut.clockDomain
    val sink = new RecordSink(dut.io.output, cd, 2)
    dut.io.delimiter #= usDelimiter
    dut.io.delimiterErrors #= 1
    var cycle = 0
    dut.io.line #= 0
    cd.onSamplings {
      dut.io.line #= lineByte(cycle)
      dut.io.time #= cycle
      cycle += 1
    }
    cd.forkStimulus(10)
    cd.waitSampling(totalBytes + 3000)
    val got = sink.records
    check(got.length == bursts.length && got.forall(_.kind == RecordKind.burst), s"one record per burst (${got.length})")
    for ((b, r) <- bursts.zip(got)) {
      // a burst ends where the line goes quiet: its last zero line bytes look like the squelch
      val keep = scrambled(b).lastIndexWhere(_ != 0) + 1
      val expect = b.clear.take(keep min maxBytes)
      check(r.payload == expect && r.flags == (if (keep > maxBytes) 1 else 0),
        s"burst at byte ${b.at} bit +${b.delay}: ${r.payload.length} clear bytes from the BIP, flags ${r.flags}")
    }
    check(got.map(_.time).zip(bursts).forall { case (t, b) => t >= b.at - 3 && t <= b.at + 3 }, "burst times at their delimiter")
    check(dut.io.drops.toLong == 0 && dut.io.truncations.toLong == 0, "no record dropped or truncated")
  }
  println("PASS SnifferTest")
}
