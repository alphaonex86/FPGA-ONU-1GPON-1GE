// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.core.sim._
import scala.collection.mutable
import GponCodes._
import GtcDsTest.check

/** FecDecoder on back-to-back codewords as in a FEC frame (255 bytes, then the short last one), encoded by the model
  * with 0 to 8 byte errors (all corrected), more (counted uncorrectable), or decoding off (passed as received). */
object FecDecoderTest extends App {
  SimConfig.compile(FecDecoder(UInt(20 bits))).doSim(seed = 4) { dut =>
    val io = dut.io
    val cd = dut.clockDomain
    val rng = new scala.util.Random(6)
    case class Word(sent: Seq[Int], received: Seq[Int], errors: Int, enabled: Boolean)
    val words = (0 until 120).map { w =>
      val n = if (w % 8 == 7) frameBytes % 255 else 255
      val data = Seq.fill(n - 16)(rng.nextInt(256))
      val sent = data ++ Model.rsParity(data)
      val errors = if (w % 10 == 9) 9 + rng.nextInt(4) else w % 9
      val at = rng.shuffle((0 until n).toList).take(errors).toSet
      val received = sent.zipWithIndex.map { case (b, i) => if (at(i)) b ^ (1 + rng.nextInt(255)) else b }
      Word(sent, received, errors, w % 13 != 5)
    }
    val stream = words.flatMap(w => w.received.indices.map(i => (w, i)))
    val out = mutable.ArrayBuffer[(Int, Int)]() // tag, byte
    cd.onSamplings { out += ((io.outTag.toInt, io.outData.toInt)) }
    cd.forkStimulus(10)
    for (((w, i), k) <- stream.zipWithIndex) {
      io.data #= w.received(i)
      io.start #= i == 0
      io.end #= i == w.received.length - 1
      io.enable #= w.enabled
      io.tag #= k + 1
      cd.waitSampling()
    }
    io.start #= false
    io.end #= false
    io.tag #= 0
    cd.waitSampling(400)
    val byTag = out.filter(_._1 != 0).toMap
    var offset = 0
    var wrong = 0
    for (w <- words) {
      val got = w.received.indices.map(i => byTag(offset + i + 1))
      val want = if (!w.enabled) w.received else w.sent
      if ((w.errors <= 8 || !w.enabled) && got != want) wrong += 1
      offset += w.received.length
    }
    val decoded = words.filter(_.enabled)
    check(wrong == 0, s"${words.length} codewords: 0-8 errors corrected, decoding off passed as received ($wrong wrong)")
    check(io.codewords.toLong == decoded.length, s"codewords counted (${io.codewords.toLong})")
    check(io.uncorrectable.toLong == decoded.count(_.errors > 8), s"more than 8 errors: uncorrectable (${io.uncorrectable.toLong})")
    check(io.correctedCodewords.toLong == decoded.count(w => w.errors > 0 && w.errors <= 8) &&
      io.correctedBytes.toLong == decoded.filter(_.errors <= 8).map(_.errors).sum,
      s"corrected codewords ${io.correctedCodewords.toLong}, bytes ${io.correctedBytes.toLong}")
  }
  println("PASS FecDecoderTest")
}
