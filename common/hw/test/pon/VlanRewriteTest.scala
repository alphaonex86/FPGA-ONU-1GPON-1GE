// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.core.sim._
import scala.collection.mutable
import pon.gpon.GtcDsTest.check

/** VlanRewrite against a model: random frames and treatments (push, pop, translate, none), with and without the FCS
  * appended; input gaps and output backpressure, or a continuous input and output: no gap inside an output frame. */
object VlanRewriteTest extends App {
  case class Job(frame: Seq[Int], remove: Boolean, add: Boolean, tag: Long)
  def model(j: Job, fcs: Boolean): Seq[Int] = {
    val tag = (0 until 4).map(k => ((j.tag >> (24 - 8 * k)) & 0xFF).toInt)
    val out = j.frame.take(12) ++ (if (j.add) tag else Nil) ++ j.frame.drop(if (j.remove) 16 else 12)
    if (!fcs) out else { val c = Crc32.model(out); out ++ (0 until 4).map(k => ((c >> (8 * k)) & 0xFF).toInt) }
  }
  for (fcs <- Seq(true, false); stress <- Seq(true, false)) {
    SimConfig.compile(VlanRewrite(fcs)).doSim { dut =>
      val io = dut.io
      val cd = dut.clockDomain
      val rnd = new scala.util.Random(if (fcs) 1 else 2)
      var gaps = 0
      val jobs = Seq.fill(60)(Job(Seq.fill(17 + rnd.nextInt(100))(rnd.nextInt(256)), rnd.nextBoolean(), rnd.nextBoolean(),
        rnd.nextLong() & 0xFFFFFFFFL))
      val queue = mutable.Queue(jobs: _*)
      var index = 0
      val received = mutable.ArrayBuffer[Seq[Int]]()
      val current = mutable.ArrayBuffer[Int]()
      io.input.valid #= false
      io.output.ready #= false
      cd.onSamplings {
        if (io.output.valid.toBoolean && io.output.ready.toBoolean) {
          current += io.output.fragment.toInt
          if (io.output.last.toBoolean) { received += current.toList; current.clear() }
        } else if (current.nonEmpty) gaps += 1
        if (io.input.valid.toBoolean && io.input.ready.toBoolean) {
          index += 1
          if (index == queue.head.frame.length) { queue.dequeue(); index = 0 }
        }
        io.output.ready #= !stress || rnd.nextInt(4) != 0
        io.input.valid #= queue.nonEmpty && (!stress || rnd.nextInt(5) != 0)
        for (j <- queue.headOption) {
          io.input.fragment #= j.frame(index)
          io.input.last #= index == j.frame.length - 1
          io.treatment.remove #= j.remove
          io.treatment.add #= j.add
          io.treatment.tag #= j.tag
        }
      }
      cd.forkStimulus(10)
      cd.waitSamplingWhere(20000)(received.length == jobs.length)
      check(received.toList == jobs.map(model(_, fcs)) && (stress || gaps == 0), s"${jobs.length} frames rewritten (push, pop, " +
        "translate, none)" + (if (fcs) ", FCS appended" else "") + (if (stress) ", gaps and backpressure" else ", no gap inside a frame"))
    }
  }
  println("PASS VlanRewriteTest")
}
