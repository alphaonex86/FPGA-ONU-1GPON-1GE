// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.core.sim._
import scala.collection.mutable
import pon.gpon.GtcDsTest.check

/** Bursts with a corrupted head through the regenerator, on every alignment of the 10-bit words: the burst leaves
  * whole after a constant delay, its head rebuilt as preamble, the laser on from before it to its last one. */
object UsRegenTest extends App {
  val compiled = SimConfig.compile(UsRegen())
  val (head, garble) = (3, 2)
  val delimiter = Seq(1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1) // 0xB5983

  case class Burst(start: Int, clean: Seq[Int]) { val end = start + clean.length } // clean: as the ONU sent it

  for (alignment <- 0 until 10) {
    val rand = new scala.util.Random(alignment)
    val line = mutable.ArrayBuffer.fill(300 + alignment)(0)
    val ideal = mutable.ArrayBuffer[Int]() // the line with every burst's preamble continued backwards
    val bursts = mutable.ArrayBuffer[Burst]()
    def burst(gapAfter: Int): Unit = {
      val clean = Seq.tabulate(48)(i => (i + 1) % 2) ++ delimiter ++ Seq.fill(399)(rand.nextInt(2)) :+ 1
      bursts += Burst(line.length, clean)
      line ++= (1 +: Seq.fill(8)(rand.nextInt(2))) ++ clean.drop(9) // the receiver settles on the first 9 bits
      line ++= Seq.fill(gapAfter)(0)
    }
    burst(40)  // bridged: the laser stays on
    burst(200)
    val stray = line.length
    line ++= 1 +: Seq.fill(199)(0)
    burst(300)
    val out, ben = mutable.ArrayBuffer[Int]()
    compiled.doSim(alignment) { dut =>
      dut.io.enable #= true
      dut.io.head #= head
      dut.io.garble #= garble
      dut.io.bits #= 0
      dut.clockDomain.forkStimulus(10)
      for (w <- 0 until line.length / 10) {
        dut.io.bits #= (0 until 10).map(k => BigInt(line(10 * w + k)) << (9 - k)).sum
        dut.clockDomain.waitSampling()
        val (o, b) = (dut.io.output.toInt, dut.io.ben.toInt)
        out ++= (9 to 0 by -1).map(k => (o >> k) & 1)
        ben ++= Seq.fill(5)((b >> 1) & 1) ++ Seq.fill(5)(b & 1)
      }
    }
    val delay = out.indexOfSlice(bursts(0).clean.drop(48)) - (bursts(0).start + 48)
    check(delay == 110, s"alignment $alignment: delay $delay bits")
    def lit(from: Int, to: Int): Seq[Int] = ben.slice(from + delay, to + delay).toSeq
    for ((b, n) <- bursts.zipWithIndex if n != 1) {
      val on = (b.start - 40 until b.start + 10).find(p => ben(p + delay) == 1).getOrElse(-1)
      check(on >= b.start - 5 * (head + 1) + 1 && on <= b.start - 5 * head, s"alignment $alignment burst $n: laser on ${b.start - on} bits before the burst")
      val want = (on until b.end).map(p => if (p < b.start) (p - b.start + 1) & 1 else b.clean(p - b.start))
      val wrong = want.indices.filter(i => out(on + delay + i) != want(i)).map(_ + on - b.start)
      check(wrong.isEmpty, s"alignment $alignment burst $n: head rebuilt, burst whole (wrong bits at ${wrong.take(12).mkString(" ")})")
      check(lit(on, b.end).forall(_ == 1), s"alignment $alignment burst $n: laser on to the last one")
    }
    val second = bursts(1) // 40 bits after the first: bridged, so its head is not rebuilt
    check(lit(bursts(0).end, second.end).forall(_ == 1) && out.slice(bursts(0).end + delay, second.start + delay).forall(_ == 0) &&
      out.slice(second.start + 9 + delay, second.end + delay) == second.clean.drop(9),
      s"alignment $alignment: a 40-bit gap is bridged, zeros sent, the next burst whole from its settled bits")
    check(lit(bursts(1).end + 5, stray + 150).forall(_ == 0), s"alignment $alignment: laser off within 5 bits, a stray one stays dark")
    check(lit(bursts(2).end + 5, line.length - delay - 10).forall(_ == 0), s"alignment $alignment: laser off after the last burst")
  }
  println("PASS UsRegenTest")
}
