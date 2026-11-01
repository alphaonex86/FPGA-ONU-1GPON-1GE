// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.core.sim._
import scala.collection.mutable
import pon.gpon.GtcDsTest.check

/** 8b/10b code against IEEE 802.3 table values and the code's properties; encoder -> line at a random bit offset ->
  * comma aligner -> decoder, then a line bit error. */
object Code8b10bTest extends App {
  def word(abcdeifghj: String): Int = Integer.parseInt(abcdeifghj.reverse, 2) // bit 0 = a
  val known = Seq((0xBC, true, false, "0011111010"), (0xBC, true, true, "1100000101"), (0x00, false, false, "1001110100"),
    (0x00, false, true, "0110001011"), (0xB5, false, false, "1010101010"), (0x50, false, false, "0110110101"),
    (0xFE, true, false, "0111101000"), (0x3C, true, false, "0011111001"), (0xF1, false, false, "1000110111"),
    (0xEB, false, true, "1101001000"))
  check(known.forall { case (b, k, rd, w) => Code8b10b.encode(b, k, rd).map(_._1) == Some(word(w)) },
    "table values: K28.5, D0.0, D21.5, D16.2, K30.7, K28.1, D17.7 (A7), D11.7 (A7) with both disparities")
  val symbols = (0 until 256).map((_, false)) ++ Code8b10b.validK.map((_, true))
  val codes = for ((b, k) <- symbols; rd <- Seq(false, true)) yield (b, k, rd, Code8b10b.encode(b, k, rd).get)
  def ones(w: Int) = Integer.bitCount(w)
  check(codes.forall { case (_, _, rd, (w, after)) => (ones(w) == 5 && after == rd || ones(w) == (if (rd) 4 else 6) && after != rd) } &&
    codes.map(c => (c._4._1, (c._1, c._2))).groupBy(_._1).forall(_._2.map(_._2).distinct.length == 1),
    s"${codes.map(_._4._1).distinct.length} code words, each of one symbol, disparity 0 or +-2 against the running one")
  val rnd = new scala.util.Random(5)
  val stream = Seq.fill(4000)(symbols(rnd.nextInt(symbols.length)))
  var rd = false
  val bits = stream.flatMap { case (b, k) => val (w, a) = Code8b10b.encode(b, k, rd).get; rd = a; (0 until 10).map(w >> _ & 1) }
  val runs = bits.foldLeft((0, -1, 0)) { case ((longest, last, run), x) => val r = if (x == last) run + 1 else 1; (longest max r, x, r) }._1
  val falseCommas = (0 until bits.length - 7).count(i => i % 10 != 0 && Code8b10b.isComma(bits.slice(i, i + 7).zipWithIndex.map(x => x._1 << x._2).sum) &&
    !stream(i / 10)._2)
  check(runs <= 5 && falseCommas == 0, s"random symbols: run length at most 5 ($runs), no comma off the symbol boundary after data")

  case class Bench() extends Component {
    val io = new Bundle {
      val input = in(Symbol8b10b())
      val line = out Bits (10 bits)
      val raw = in Bits (10 bits)
      val output = out(Symbol8b10b())
      val error = out Bool ()
    }
    val encoder = Encoder8b10b()
    encoder.io.input := io.input
    io.line := encoder.io.code
    val aligner = CommaAligner()
    aligner.io.raw := io.raw
    val decoder = Decoder8b10b()
    decoder.io.code := aligner.io.code
    io.output := decoder.io.output
    io.error := decoder.io.error
  }
  SimConfig.compile(Bench()).doSim { dut =>
    val io = dut.io
    val cd = dut.clockDomain
    val offset = 3 + rnd.nextInt(6)
    val sent = Seq.fill(20)((0xBC, true)) ++ Seq.fill(2000)(symbols(rnd.nextInt(symbols.length)))
    val lineBits = mutable.ArrayBuffer.fill(offset)(0)
    val got = mutable.ArrayBuffer[(Int, Boolean, Boolean)]()
    var flip = -1L
    cd.forkStimulus(10)
    for ((b, k) <- sent ++ Seq.fill(10)((0xBC, true))) {
      io.input.data #= b
      io.input.k #= k
      cd.waitSampling()
      lineBits ++= (0 until 10).map(io.line.toInt >> _ & 1)
      val at = lineBits.length - 10 - offset - 10
      if (at >= 0) {
        if (at / 10 == 1500) flip = at + 4
        io.raw #= (0 until 10).map(i => (lineBits(at + i) ^ (if (at + i == flip) 1 else 0)) << i).sum
      } else io.raw #= 0
      got += ((io.output.data.toInt, io.output.k.toBoolean, io.error.toBoolean))
    }
    val data = sent.drop(20)
    val first = got.map(g => (g._1, g._2)).indexOfSlice(data.take(8))
    val decoded = got.drop(first).take(data.length)
    val aligned = data.indices.takeWhile(i => i < decoded.length && (decoded(i)._1, decoded(i)._2) == data(i)).length
    val errors = decoded.zipWithIndex.filter(_._1._3).map(_._2)
    check(first >= 0 && aligned >= 1400, s"line bit offset $offset: aligned on the commas, $aligned symbols decoded as sent")
    check(errors.nonEmpty && errors.head >= 1460 && errors.head <= 1500 && errors.length <= 3,
      s"one line bit error: flagged (symbol ${errors.headOption.getOrElse(-1)}, ${errors.length} errors)")
  }
  println("PASS Code8b10bTest")
}
