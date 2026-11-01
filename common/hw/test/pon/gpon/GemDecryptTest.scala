// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.collection.mutable
import GponCodes._
import GtcDsTest.{check, hex, bigToBytes}

/** Deframer -> GemDecrypt, the encryption controls from the test. Test only. */
case class GemDecryptBench() extends Component {
  val deframer = GtcDsDeframer()
  val decrypt = GemDecrypt()
  val io = new Bundle {
    val line = in Bits (8 bits)
    val ports = in Vec (UInt(12 bits), 16)
    val portsUsed = in Bits (16 bits)
    val shadowKey = in Bits (128 bits)
    val keySwitch = slave Flow (UInt(30 bits))
    val keyRequest = in Bool ()
    val output = master Flow (Fragment(GemRxByte()))
    val outputCut = out Bool ()
    val late = out Bool ()
  }
  deframer.io.line := io.line
  deframer.io.simPublic()
  decrypt.io.header << deframer.io.gemHeader
  decrypt.io.input << deframer.io.gem
  decrypt.io.inputCut := deframer.io.gemCut
  decrypt.io.ports := io.ports
  decrypt.io.portsUsed := io.portsUsed
  decrypt.io.shadowKey := io.shadowKey
  decrypt.io.keySwitch << io.keySwitch
  decrypt.io.keyRequest := io.keyRequest
  io.output << decrypt.io.output
  io.outputCut := decrypt.io.outputCut
  io.late := decrypt.io.late
}

/** GemDecrypt after the deframer against the G.984.3 golden vectors (A.2.2, A.2.3 with FEC: the parity skipped, the
  * crypto-counter running through it) and the JVM's AES in counter mode: key switch at the frame set
  * by Key_Switching_Time, Request_Key voiding it, clear ports untouched, back-to-back traffic with no late keystream,
  * and a burst of 3-byte encrypted fragments: those late are dropped, the next ones decrypt again. */
object GemDecryptTest extends App {
  def aes(key: BigInt, block: BigInt): Seq[Int] = {
    def bytes(v: BigInt) = bigToBytes(v, 16).map(_.toByte).toArray
    val cipher = javax.crypto.Cipher.getInstance("AES/ECB/NoPadding")
    cipher.init(javax.crypto.Cipher.ENCRYPT_MODE, new javax.crypto.spec.SecretKeySpec(bytes(key), "AES"))
    cipher.doFinal(bytes(block)).toSeq.map(_ & 0xFF)
  }
  def counterBlock(c: Long): BigInt = ((BigInt(c) << 92) | (BigInt(c) << 46) | c) & ((BigInt(1) << 128) - 1)
  def crypt(key: BigInt, counter: Long, data: Seq[Int]): Seq[Int] =
    data.grouped(16).zipWithIndex.flatMap { case (chunk, k) => chunk.zip(aes(key, counterBlock(counter + k))).map { case (d, s) => d ^ s } }.toSeq

  val goldenKey = BigInt("112233445566778899AABBCCDDEEFF00", 16)
  val goldenFrame = 0x3DCAE120L
  val goldenPlain = hex("""B4 9A 12 D0 73 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F 10 11 12 13 14 15 16 17 18 19 1A
    1B 1C 1D 1E 1F 20 21 22 B6 CA 12 C0 4A AA BB CC DD EE FF B6 5A 12 C1 BB 11 22 33 44 55 66 77 88 99 AA BB CC DD EE FF""")
  val goldenCipher = hex("""B4 9A 12 D0 73 3A FB 97 EE FC BC C1 6B 6C 57 1A A4 FF 7A C3 AD 6C 85 28 5A 57 F8 9E 7A 36 07 CA
    8A CE 45 0A 97 A9 74 5A B6 CA 12 C0 4A 8B 5F 94 E4 8F 34 B6 5A 12 C1 BB 9D F4 F4 15 F6 A4 3C D0 30 0F F6 92 88 EE 54""")
  val goldenAt = 157 // line byte of the first header (the spec counts from 1)
  // the vector's fragments: header offset, payload length
  val goldenFragments = Seq((0, 35), (40, 6), (51, 15))
  check(goldenFragments.forall { case (h, n) =>
    val counter = (goldenFrame << 16) | ((goldenAt + h) >> 2)
    crypt(goldenKey, counter, goldenPlain.slice(h + 5, h + 5 + n)) == goldenCipher.slice(h + 5, h + 5 + n)
  }, "counter-mode model on the A.2.2 golden vector")
  // A.2.3, FEC on: fragments of 30 and 15 bytes, the first across the parity of codeword 1
  val fecAt = 219
  val fecPlain = Seq((0 to 0x1D).toSeq, hex("11 22 33 44 55 66 77 88 99 AA BB CC DD EE FF"))
  val fecCipher = Seq(hex("0F DA 75 62 82 60 A4 8E A0 53 1B 6D CA 53 9B B6 0C 48 B2 74 5A 7E 95 C1 F3 63 BD 63 5C DF"),
    hex("58 F1 4C 22 75 38 D0 32 C8 90 28 6E 71 84 ED"))
  check(crypt(goldenKey, (goldenFrame << 16) | (fecAt >> 2), fecPlain(0)) == fecCipher(0) &&
    crypt(goldenKey, (goldenFrame << 16) | ((fecAt + 51) >> 2), fecPlain(1)) == fecCipher(1), "counter-mode model on the A.2.3 golden vector")

  /** A fragment in a frame: Port-ID, clear payload, encrypted on the line. */
  case class Frag(port: Int, data: Seq[Int], encrypted: Boolean)
  val idle = bigToBytes(gemMask, 5)
  def header(f: Frag): Seq[Int] = bigToBytes(Model.gemHeader(f.data.length, f.port, 1) ^ gemMask, 5)

  /** One downstream frame, clear (before scrambling): allocations and idle GEM up to line byte `at` (< 239), the
    * fragments back to back from there. FEC: the Ident bit and the parity bytes. */
  def frame(superframe: Long, key: BigInt, at: Int, frags: Seq[Frag], fec: Boolean = false): Seq[Int] = {
    val allocs = (1 to 5).find(n => (at - 30 - 8 * n) % 5 == 0).get
    val ploam = bigToBytes(noMessage, 12)
    val plend = Seq(allocs >> 4, (allocs & 0xF) << 4, 0x00)
    val alloc = Seq(0x01, 0x00, 0x00, 0x00, 0x10, 0x00, 0x20)
    val ident = BigInt(superframe) | (if (fec) BigInt(1) << 31 else 0)
    val head = bigToBytes(psync, 4) ++ bigToBytes(ident, 4) ++ ploam ++ Seq(Model.crc8(ploam), 0) ++
      plend ++ Seq(Model.crc8(plend)) ++ plend ++ Seq(Model.crc8(plend)) ++ Seq.fill(allocs)(alloc :+ Model.crc8(alloc)).flatten
    val bytes = mutable.ArrayBuffer[Int]() ++ head ++ Seq.fill((at - head.length) / 5)(idle).flatten
    val dataBytes = if (fec) frameBytes - 16 * 153 else frameBytes
    for (f <- frags) {
      val counter = (superframe << 16) | (Model.linePos(bytes.length, fec) >> 2)
      bytes ++= header(f) ++ (if (f.encrypted) crypt(key, counter, f.data) else f.data)
    }
    require(bytes.length <= dataBytes)
    while (bytes.length + 5 <= dataBytes) bytes ++= idle
    bytes ++= Seq.fill(dataBytes - bytes.length)(0)
    if (fec) Model.fecEncode(bytes.toSeq) else bytes.toSeq
  }
  def fragsOf(bytes: Seq[Int]): Seq[Frag] = goldenFragments.map { case (h, n) => Frag(0x123, bytes.slice(h + 5, h + 5 + n), true) }

  SimConfig.compile(GemDecryptBench()).doSim(seed = 2) { dut =>
    val cd = dut.clockDomain
    val got = mutable.ArrayBuffer[(Int, Seq[Int])]()
    var open = mutable.ArrayBuffer[Int]()
    var lates = 0
    cd.onSamplings {
      if (dut.io.output.valid.toBoolean) {
        open += dut.io.output.data.toInt
        if (dut.io.output.last.toBoolean) { got += ((dut.io.output.portId.toInt, open.toSeq)); open = mutable.ArrayBuffer[Int]() }
      }
      if (dut.io.outputCut.toBoolean) open = mutable.ArrayBuffer[Int]()
      if (dut.io.late.toBoolean) lates += 1
    }
    def setPorts(ports: Seq[Int]): Unit = {
      for (i <- 0 until 16) dut.io.ports(i) #= (if (i < ports.length) ports(i) else 0)
      dut.io.portsUsed #= (1 << ports.length) - 1
    }
    setPorts(Seq(0x123, 0x300))
    dut.io.shadowKey #= goldenKey
    dut.io.keySwitch.valid #= false
    dut.io.keyRequest #= false
    dut.io.line #= 0
    cd.forkStimulus(10)
    cd.waitSampling(10)
    def send(clear: Seq[Int]): Unit = for (b <- Model.descramble(clear)) { dut.io.line #= b; cd.waitSampling() }
    def pulse(f: => Unit, valid: Bool): Unit = fork { f; valid #= true; cd.waitSampling(); valid #= false } // the line runs on

    // sync on 3 frames, the switch armed for the golden frame; the frame before still uses key 0. The idle end of each
    // frame flushes its fragments out.
    val plainOld = Frag(0x123, (0 until 40).map(_ * 7 & 0xFF), true)
    for (s <- goldenFrame - 4 until goldenFrame - 1) send(frame(s, 0, 158, Nil))
    pulse(dut.io.keySwitch.payload #= goldenFrame, dut.io.keySwitch.valid)
    send(frame(goldenFrame - 1, 0, 158, Seq(plainOld)))
    check(got.toSeq == Seq((0x123, plainOld.data)), "frame before the switch: decrypted with the old key")
    val clearFrag = Frag(0x200, (0 until 30).map(_ + 0x40), false)
    check(frame(goldenFrame, goldenKey, goldenAt, fragsOf(goldenPlain)).slice(goldenAt, goldenAt + goldenCipher.length) == goldenCipher,
      "test frame carries the golden cipher text")
    got.clear()
    send(frame(goldenFrame, goldenKey, goldenAt, fragsOf(goldenPlain) :+ clearFrag))
    check(got.toSeq == fragsOf(goldenPlain).map(f => (0x123, f.data)) :+ ((0x200, clearFrag.data)),
      "golden frame: A.2.2 plain text out, clear Port-ID untouched")

    // Request_Key voids an armed switch
    got.clear()
    dut.io.shadowKey #= BigInt("0F0E0D0C0B0A09080706050403020100", 16)
    pulse(dut.io.keySwitch.payload #= goldenFrame + 2, dut.io.keySwitch.valid)
    fork { cd.waitSampling(2); pulse((), dut.io.keyRequest) }
    val f1 = Frag(0x300, (0 until 100).map(_ ^ 0x5A), true)
    send(frame(goldenFrame + 1, goldenKey, 158, Seq(f1)))
    send(frame(goldenFrame + 2, goldenKey, 158, Seq(f1)))
    check(got.toSeq == Seq.fill(2)((0x300, f1.data)), "Request_Key: the armed switch is void, the key stays")

    // back to back: Ethernet sizes on both ports, a 1-byte tail; nothing late
    got.clear()
    val rng = new scala.util.Random(9)
    val traffic = mutable.ArrayBuffer[Frag]()
    var length = 0
    while (length < 30000) {
      val n = if (rng.nextInt(8) == 0) 1 else 64 + rng.nextInt(1455)
      val enc = rng.nextInt(4) != 0
      traffic += Frag(if (enc) 0x123 else 0x201, Seq.fill(n)(rng.nextInt(256)), enc)
      length += n + 5
    }
    send(frame(goldenFrame + 3, goldenKey, 158, traffic.toSeq))
    check(lates == 0 && got.toSeq == traffic.map(f => (f.port, f.data)), s"${traffic.length} fragments back to back: all decrypted, none late")

    // 3-byte encrypted fragments back to back: the keystream falls behind, late ones dropped, then it recovers
    got.clear()
    val tiny = Seq.fill(60)(Frag(0x123, Seq.fill(3)(rng.nextInt(256)), true))
    val after = Seq.fill(5)(Frag(0x123, Seq.fill(200)(rng.nextInt(256)), true))
    send(frame(goldenFrame + 4, goldenKey, 158, tiny ++ Seq.fill(40)(Frag(0x201, Seq.fill(60)(0), false)) ++ after))
    val tinyGot = got.filter(_._2.length == 3).map(_._2).toList
    def inOrder(sub: List[Seq[Int]], all: List[Seq[Int]]): Boolean = (sub, all) match {
      case (Nil, _) => true
      case (_, Nil) => false
      case (x :: xs, y :: ys) => if (x == y) inOrder(xs, ys) else inOrder(sub, ys)
    }
    check(lates > 0 && tinyGot.length + lates == tiny.length && inOrder(tinyGot, tiny.map(_.data).toList),
      s"3-byte burst: ${tinyGot.length} decrypted right, $lates late and dropped")
    check(got.takeRight(5).toSeq == after.map(f => (f.port, f.data)), "after the burst: decrypted again")

    // FEC on: the A.2.3 frame (its superframe again: the counter takes it once two frames agree), then traffic across
    // the parity bytes of every codeword
    for (s <- goldenFrame - 2 until goldenFrame) send(frame(s, goldenKey, 158, Nil, fec = true)) // FEC on after two frames
    got.clear()
    val latesBefore = lates
    val fecFrame = frame(goldenFrame, goldenKey, fecAt, fecPlain.map(Frag(0x123, _, true)), fec = true)
    check(fecFrame.length == frameBytes && fecFrame.slice(fecAt + 5, fecAt + 20) == fecCipher(0).take(15) &&
      fecFrame.slice(fecAt + 36, fecAt + 51) == fecCipher(0).drop(15) && fecFrame.slice(fecAt + 56, fecAt + 71) == fecCipher(1),
      "test frame carries the A.2.3 cipher text around the parity")
    send(fecFrame)
    check(got.toSeq == fecPlain.map((0x123, _)), "A.2.3 frame with FEC: plain text out")
    got.clear()
    val fecTraffic = traffic.take(traffic.length * 3 / 4)
    // 8 byte errors in every third codeword (PSync spared): all corrected
    val damaged = frame(goldenFrame + 1, goldenKey, 158, fecTraffic.toSeq, fec = true).toArray
    val codewords = (0 until frameBytes by 255).filter(_ / 255 % 3 == 1)
    for (c <- codewords; p <- rng.shuffle((c until scala.math.min(c + 255, frameBytes)).toList).take(8)) damaged(p) = damaged(p) ^ (1 + rng.nextInt(255))
    val corrected0 = dut.deframer.io.fecCorrectedBytes.toLong
    send(damaged.toSeq)
    check(lates == latesBefore && got.toSeq == fecTraffic.map(f => (f.port, f.data)),
      s"${fecTraffic.length} fragments back to back with FEC, ${codewords.length * 8} byte errors: all decrypted")
    send(frame(goldenFrame + 2, goldenKey, 158, Nil, fec = true)) // the last codewords out
    check(dut.deframer.io.fecCorrectedBytes.toLong - corrected0 == codewords.length * 8 && dut.deframer.io.fecUncorrectable.toLong == 0,
      s"FEC: ${codewords.length * 8} bytes corrected, none uncorrectable")
  }
  println("PASS GemDecryptTest")
}
