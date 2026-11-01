// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.collection.mutable
import GponCodes._

/** Framer -> k-bit delay -> deframer, with a bit-error injection point. Test only. */
case class GtcDsLoopback(bitDelay: Int, fec: Boolean = false) extends Component {
  val framer = GtcDsFramer()
  val deframer = GtcDsDeframer()
  val io = new Bundle {
    val ploam = slave(cloneOf(framer.io.ploam))
    val bwmapWrite = slave(cloneOf(framer.io.bwmapWrite))
    val bwmapCommit = slave(cloneOf(framer.io.bwmapCommit))
    val gem = slave(cloneOf(framer.io.gem))
    val errorMask = in Bits (8 bits)
  }
  framer.io.ploam << io.ploam
  framer.io.bwmapWrite << io.bwmapWrite
  framer.io.bwmapCommit << io.bwmapCommit
  framer.io.gem << io.gem
  framer.io.fec := Bool(fec)
  val previous = RegNext(framer.io.line) init (0)
  deframer.io.line := (previous ## framer.io.line)(15 - bitDelay downto 8 - bitDelay) ^ io.errorMask
  framer.io.frameStart.simPublic()
  framer.io.line.simPublic()
  framer.io.gemError.simPublic()
  deframer.io.simPublic()
}

/** G.984.3 test vectors (annex A.5, appendix III) and a framer/deframer loopback. Throws on failure. */
object GtcDsTest extends App {
  def hex(s: String): Seq[Int] = s.trim.split("\\s+").toSeq.map(Integer.parseInt(_, 16))

  // annex A.5: first 138 bytes of a downstream frame, before and after scrambling (BIP not applicable)
  val vectorClear = hex("""
    B6 AB 31 E0 00 05 12 76 12 13 21 01 05 00 00 00 00 00 00 00 CA 55 00 20 00 AE 00 20 00 AE 01 00 00 10 00 15 00 AE 15 04 00 16 00 17 00 F2
    B2 AA 31 CD 74 FF FF FF FF FF FF 00 0E 7F 5F F1 DF 08 06 00 01 08 00 06 04 00 01 00 0E 7F 5F F1 DF C0 A8 01 84 00 00 00 00 00 00 C0 A8 01
    41 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 F9 A6 DF 13 B7 8A 12 CD 2D 76 12 05 72 08 11 77 06 08 74 10 20 73 03 14 81 01 21""")
  val vectorLine = hex("""
    B6 AB 31 E0 FE 01 0A 27 F6 4A F5 FB 19 49 B5 BD 8D 2E E6 55 36 5D 30 83 C8 1D A9 D4 38 3D 6A 7B 1A 4D CC BE F8 BE 74 43 91 71 53 FF 71 D4
    64 5C 05 76 ED A8 0F DF 3D 70 DD CE A9 AF BD BC 72 E4 6F 77 33 A7 E0 47 81 1E 44 9D 41 DE 9B 6A 84 18 7A EF E1 5F C0 83 0A 3C 8B FA 37 42
    C8 36 B7 B1 A5 DC CA BF 81 06 14 79 16 75 3E 87 12 6D 6F 9A ED 66 86 C8 88 1E E5 DF 5A F8 78 7C 2C CB A9 C0 9F 07 3A DE 77 1B 45 65 58 F5""")
  val vectorPloam = vectorClear.slice(8, 20)
  val vectorPacket1 = vectorClear.slice(51, 115) // 64-byte Ethernet frame, Port-ID 0x100
  val vectorPacket2 = vectorClear.slice(120, 138) // 18 bytes, Port-ID 0x123
  // appendix III: valid headers, before the mask
  val validHeaders = """528A739F79 B61925D883 BF2D33B47F 9727D4C430 7D3A32AA75 A257E5A295 7F2963C54B 7F0BF34736
    7EF99F35F6 974CF521A3 86785F3E30 BB4A72F128 BEDB6545BA CE98AC73EF 7C6CA16F93 E617D9905C 0B2A61476B 95F1933472
    BA487424EA 95F8B97926 BAB7C5FC86 BEBBF4A2E7 B9F1AFBA45 04E7E3A963 A6FB9FAEFF 7F4A25750A 9A696E9B88 86EA5F7CE3
    CA47E19CFC BEDB7532FA DE1CDF6663 7E59A67E44 8A5CA75CE7 17986C90AB BA47F4EEFF BA9D39E439""".trim.split("\\s+").map(BigInt(_, 16))

  def check(ok: Boolean, what: String): Unit = { if (!ok) throw new Exception("FAIL: " + what); println("ok   " + what) }
  def bytesToBig(b: Seq[Int]): BigInt = b.foldLeft(BigInt(0))((acc, x) => (acc << 8) | x)
  def bigToBytes(v: BigInt, n: Int): Seq[Int] = (n - 1 to 0 by -1).map(i => ((v >> (8 * i)) & 0xFF).toInt)

  // reference models against the spec
  check(Model.crc8(vectorPloam) == 0xCA, "model CRC-8 of the annex A.5 PLOAM")
  check(Model.crc8(vectorClear.slice(22, 25)) == 0xAE && Model.crc8(vectorClear.slice(30, 37)) == 0xAE &&
    Model.crc8(vectorClear.slice(38, 45)) == 0xF2, "model CRC-8 of PLend and BWmap")
  check((Model.gemHeader(64, 0x100, 1) ^ gemMask) == bytesToBig(vectorClear.slice(46, 51)) &&
    (Model.gemHeader(18, 0x123, 1) ^ gemMask) == bytesToBig(vectorClear.slice(115, 120)), "model GEM headers of annex A.5")
  check(validHeaders.forall(h => Model.gemHeader((h >> 28).toInt, ((h >> 16) & 0xFFF).toInt, ((h >> 13) & 7).toInt) == h),
    "model HEC of the 36 appendix III headers")
  check(Model.descramble(vectorLine) == vectorClear, "model scrambler on annex A.5")
  // annex A.3: a full and a short codeword, parity last
  val fecFull = hex("""00 65 f2 50 51 52 53 54 55 56 57 58 59 5a 5b 5c a2 ec 40 21 f7""") ++ (0 to 0xd9) ++
    hex("d1 ee 8b 7e 8e 33 b7 89 cb e4 c4 64 cb 12 d4 d9")
  val fecShort = (0xda to 0xfe) ++ (0 to 0x44) ++ hex("72 d1 ba 17 30 b5 03 71 70 49 54 35 1c 40 1e 59")
  check(fecFull.length == 255 && fecShort.length == 122, "annex A.3 vectors: 255 and 122 bytes")
  check(Seq(fecFull, fecShort).forall(cw => Model.rsParity(cw.dropRight(16)) == cw.takeRight(16)), "model RS(255,239) parity on annex A.3")

  /** Feeds packets (Port-ID, bytes) one byte per clock. */
  class GemDriver(s: Stream[Fragment[GemTxByte]], cd: ClockDomain) {
    val queue = mutable.Queue[(Int, Seq[Int])]()
    private var index = 0
    s.valid #= false
    cd.onSamplings {
      if (s.valid.toBoolean && s.ready.toBoolean) {
        index += 1
        if (index == queue.head._2.length) { queue.dequeue(); index = 0 }
      }
      s.valid #= queue.nonEmpty
      if (queue.nonEmpty) {
        val (port, data) = queue.head
        s.fragment.portId #= port
        s.fragment.length #= data.length
        s.fragment.data #= data(index)
        s.last #= index == data.length - 1
      }
    }
  }

  class PloamDriver(s: Stream[Bits], cd: ClockDomain) {
    val queue = mutable.Queue[BigInt]()
    s.valid #= false
    cd.onSamplings {
      if (s.valid.toBoolean && s.ready.toBoolean) queue.dequeue()
      s.valid #= queue.nonEmpty
      if (queue.nonEmpty) s.payload #= queue.head
    }
  }

  def writeBwmap(write: Flow[BwmapWrite], commit: Flow[UInt], allocs: Seq[(Int, Int, Int, Int)], cd: ClockDomain): Unit = {
    for (((id, flags, start, stop), i) <- allocs.zipWithIndex) {
      write.valid #= true
      write.index #= i
      write.alloc.allocId #= id
      write.alloc.flags #= flags
      write.alloc.start #= start
      write.alloc.stop #= stop
      cd.waitSampling()
    }
    write.valid #= false
    commit.valid #= true
    commit.payload #= allocs.length
    cd.waitSampling()
    commit.valid #= false
  }

  val vectorAllocs = Seq((0x10, 0x000, 0x1000, 0x1500), (0x150, 0x400, 0x1600, 0x1700))

  // framer against annex A.5: frame 1 carries the example (superframe 0x00051276)
  SimConfig.compile(GtcDsFramer(superframeReset = 0x00051275)).doSim { dut =>
    val cd = dut.clockDomain
    dut.io.fec #= false
    val gem = new GemDriver(dut.io.gem, cd)
    val ploam = new PloamDriver(dut.io.ploam, cd)
    dut.io.bwmapWrite.valid #= false
    dut.io.bwmapCommit.valid #= false
    val frames = mutable.ArrayBuffer[mutable.ArrayBuffer[Int]]()
    var gemErrors = 0
    cd.onSamplings {
      if (dut.io.frameStart.toBoolean) frames += mutable.ArrayBuffer[Int]()
      if (frames.nonEmpty) frames.last += dut.io.line.toInt
      if (dut.io.gemError.toBoolean) gemErrors += 1
    }
    cd.forkStimulus(10)
    cd.waitSamplingWhere(frames.nonEmpty)
    writeBwmap(dut.io.bwmapWrite, dut.io.bwmapCommit, vectorAllocs, cd)
    cd.waitSamplingWhere(frames.length == 2)
    ploam.queue += bytesToBig(vectorPloam)
    gem.queue += ((0x100, vectorPacket1))
    gem.queue += ((0x123, vectorPacket2))
    cd.waitSamplingWhere(frames.length == 3)
    val Seq(frame0, frame1) = frames.take(2).map(_.toSeq).toSeq
    check(frame1.length == frameBytes, "framer frame length 38880")
    val mismatch = frame1.take(138).zip(vectorLine).zipWithIndex.collectFirst { case ((a, b), i) if i != 21 && a != b =>
      f" (byte $i: 0x$a%02X, expected 0x$b%02X)" }
    check(mismatch.isEmpty, "framer matches annex A.5 byte for byte (BIP excluded)" + mismatch.getOrElse(""))
    val clear0 = Model.descramble(frame0)
    val clear1 = Model.descramble(frame1)
    val bip0 = clear0.take(21).reduce(_ ^ _)
    check(bip0 == clear0(21), f"framer BIP of the first frame (0x${clear0(21)}%02X, expected 0x$bip0%02X)")
    check((clear0.drop(22) ++ clear1.take(21)).reduce(_ ^ _) == clear1(21), "framer BIP over clear bytes since the last BIP")
    check(clear1.slice(138, 143) == bigToBytes(gemMask, 5), "framer idle GEM frame after the data")
    check(clear0.slice(8, 20) == bigToBytes(noMessage, 12) && clear0.slice(22, 26) == Seq(0, 0, 0, 0),
      "framer sends No message and an empty BWmap by default")
    check(gemErrors == 0, "framer reports no GEM error")
  }

  // loopback on two bit alignments, and with FEC: sync, PLOAM, BWmap, GEM (fragmentation, PLI cap), BIP and HEC errors
  for ((bitDelay, fec) <- Seq((0, false), (3, false), (3, true))) SimConfig.compile(GtcDsLoopback(bitDelay, fec)).doSim { dut =>
    val name = s"loopback k=$bitDelay${if (fec) " FEC" else ""}"
    val cd = dut.clockDomain
    val f = dut.framer
    val d = dut.deframer
    val gem = new GemDriver(dut.io.gem, cd)
    val ploam = new PloamDriver(dut.io.ploam, cd)
    dut.io.bwmapWrite.valid #= false
    dut.io.bwmapCommit.valid #= false
    dut.io.errorMask #= 0
    val rand = new scala.util.Random(bitDelay)
    val sentPackets = Seq.tabulate(60)(i => (rand.nextInt(4096), Seq.fill(Seq(64, 1518, 5000, 1)(i % 4))(rand.nextInt(256))))
    val sentPloams = Seq.tabulate(5)(i => BigInt(96, rand))
    val gotPackets = mutable.ArrayBuffer[(Int, Seq[Int])]()
    val partial = mutable.Map[Int, mutable.ArrayBuffer[Int]]()
    val gotPloams = mutable.ArrayBuffer[BigInt]()
    val gotAllocs = mutable.ArrayBuffer[(Int, Int, Int, Int)]()
    var badCrc = 0
    var frameStarts = 0
    var gemErrors = 0
    val f0 = if (fec) 4 else 3 // FEC: on at the deframer after two frames with the Ident bit
    val lineFrame = mutable.ArrayBuffer[Int]() // a frame as sent
    cd.onSamplings {
      if (f.io.frameStart.toBoolean) frameStarts += 1
      if (frameStarts == f0 + 2) lineFrame += f.io.line.toInt
      if (f.io.gemError.toBoolean) gemErrors += 1
      if (d.io.ploam.valid.toBoolean) {
        if (!d.io.ploam.crcOk.toBoolean) badCrc += 1 else gotPloams += d.io.ploam.value.toBigInt
      }
      if (d.io.alloc.valid.toBoolean) {
        val a = d.io.alloc.value
        if (!d.io.alloc.crcOk.toBoolean) badCrc += 1
        else gotAllocs += ((a.allocId.toInt, a.flags.toInt, a.start.toInt, a.stop.toInt))
      }
      if (d.io.gem.valid.toBoolean) {
        val port = d.io.gem.portId.toInt
        partial.getOrElseUpdate(port, mutable.ArrayBuffer[Int]()) += d.io.gem.data.toInt
        if (d.io.gem.last.toBoolean && (d.io.gem.pti.toInt & 1) == 1) gotPackets += ((port, partial.remove(port).get.toSeq))
      }
    }
    cd.forkStimulus(10)
    cd.waitSamplingWhere(d.io.synced.toBoolean)
    check(frameStarts <= 3, s"$name: deframer in sync after ${frameStarts} frames")
    cd.waitSamplingWhere(frameStarts == f0)
    writeBwmap(dut.io.bwmapWrite, dut.io.bwmapCommit, vectorAllocs, cd) // for the next frame
    ploam.queue ++= sentPloams
    gem.queue ++= sentPackets
    cd.waitSampling(1000) // the frame before is out of the deframer: errors from here on (FEC: it turned on there)
    val (bip0, plend0, hec0, crc0) = (d.io.bipErrors.toLong, d.io.plendErrors.toLong, d.io.hecErrors.toLong, badCrc)
    def bipErrors = d.io.bipErrors.toLong - bip0
    def hecErrors = d.io.hecErrors.toLong - hec0
    cd.waitSamplingWhere(frameStarts == f0 + 8)
    check(gotPloams.filter(_ != noMessage) == sentPloams, s"$name: PLOAM messages in order")
    check(gotAllocs == vectorAllocs, s"$name: BWmap entries of the committed frame only")
    check(gotPackets == sentPackets, s"$name: ${sentPackets.length} GEM packets, fragmented across frames")
    check(badCrc == crc0 && gemErrors == 0 && bipErrors == 0 && d.io.plendErrors.toLong == plend0 &&
      hecErrors == 0 && d.io.gemCuts.toLong == 0, s"$name: no CRC, GEM, BIP, PLend, HEC or cut error")
    // one flipped bit in an idle GEM header: one BIP bit error, one HEC error, delineation recovers
    cd.waitSamplingWhere(f.io.frameStart.toBoolean)
    cd.waitSampling(1000)
    dut.io.errorMask #= 0x10
    cd.waitSampling()
    dut.io.errorMask #= 0
    cd.waitSamplingWhere(frameStarts == f0 + 10)
    cd.waitSampling(500) // the BIP covering the error: byte 21 of the next frame, after the FEC decoder
    if (fec) check(bipErrors == 0 && hecErrors == 0 && d.io.fecCorrectedBytes.toLong == 1 &&
      d.io.fecUncorrectable.toLong == 0 && d.io.synced.toBoolean, s"$name: injected bit error corrected (FEC ${d.io.fecCorrectedBytes.toLong})")
    else check(bipErrors == 1 && hecErrors == 1 && d.io.synced.toBoolean,
      s"$name: injected bit error counted once by BIP and HEC, still in sync (bip $bipErrors, hec $hecErrors, synced ${d.io.synced.toBoolean})")
    if (fec) {
      val clear = Model.descramble(lineFrame.toSeq)
      check(clear.length == frameBytes && clear(4) >> 7 == 1 &&
        clear.grouped(255).forall(cw => Model.rsParity(cw.dropRight(16)) == cw.takeRight(16)),
        s"$name: frame sent with the Ident FEC bit, every codeword's parity right")
    }
    gem.queue ++= sentPackets.take(4)
    gotPackets.clear()
    cd.waitSamplingWhere(frameStarts == f0 + 12)
    check(gotPackets == sentPackets.take(4), s"$name: GEM traffic after the error")
  }
  println("PASS GtcDsTest")
}
