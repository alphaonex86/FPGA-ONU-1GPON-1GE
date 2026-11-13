// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.core.sim._
import scala.collection.mutable
import pon.gpon.GtcDsTest.check

/** A burst sampled 4 times per bit by a clock off the ONU's, with jitter on its edges: the decided bits are the
  * burst's, and the cleaned samples are the line again at a constant delay, every bit 3 to 5 samples long. */
object UsCleanTest extends App {
  val compiled = SimConfig.compile(UsClean())
  val delimiter = Seq(1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1)
  for ((ppm, jitter) <- Seq((0, 0.0), (100, 0.6), (-100, 0.6), (2000, 0.6), (-2000, 0.6)); phase <- Seq(0.0, 1.3, 2.5, 3.9)) {
    val name = s"$ppm ppm, jitter $jitter, phase $phase"
    val rand = new scala.util.Random(ppm + (phase * 10).toInt)
    val bits = Seq.tabulate(48)(i => (i + 1) % 2) ++ delimiter ++ Seq.fill(8000)(rand.nextInt(2)) :+ 1
    val period = 4 * (1 + ppm * 1e-6) // the ONU's bit in samples
    val start = 400 + phase
    val bound = Array.tabulate(bits.length + 1)(k => start + k * period + (rand.nextDouble() * 2 - 1) * jitter)
    val total = (bound.last.toInt + 400) / 40 * 40
    val line = new Array[Int](total)
    var k = 0
    for (n <- 0 until total) {
      while (k < bits.length && bound(k + 1) <= n) k += 1
      line(n) = if (n >= bound(0) && k < bits.length) bits(k) else 0
    }
    val clean, decided = mutable.ArrayBuffer[Int]()
    compiled.doSim(7) { dut =>
      dut.io.samples #= 0
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling(8)
      for (w <- 0 until total / 40) {
        dut.io.samples #= (0 until 40).map(i => BigInt(line(40 * w + i)) << (39 - i)).sum
        dut.clockDomain.waitSampling()
        val (c, b, count) = (dut.io.clean.toBigInt, dut.io.bits.toInt, dut.io.count.toInt)
        clean ++= (39 to 0 by -1).map(i => ((c >> i) & 1).toInt)
        decided ++= (10 until 10 - count by -1).map(i => (b >> i) & 1)
      }
    }
    val settled = 40 // bits left to the tracker
    check(decided.indexOfSlice(bits.drop(settled)) >= 0, s"$name: the burst's bits decided, from bit $settled to the last")
    // the line's edge before bit k is at start + k period, a cleaned edge before sample i at i - 0.5: matched from the last
    val edges = (1 until clean.length).filter(i => clean(i) != clean(i - 1)).map(_ - 0.5)
    val ideal = (settled to bits.length).filter(k => k == bits.length || bits(k) != bits(k - 1)).map(k => start + k * period)
    val delays = edges.takeRight(ideal.length).zip(ideal).map { case (e, t) => e - t }
    val steps = delays.zip(delays.drop(1)).map { case (x, y) => math.abs(y - x) }
    check(edges.length >= ideal.length && delays.max - delays.min <= 3.0 && steps.max <= 1.6,
      f"$name: cleaned line at a constant delay (${delays.min}%.1f to ${delays.max}%.1f samples, steps to ${steps.max}%.1f)")
  }
  println("PASS UsCleanTest")
}
