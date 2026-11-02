// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.core.sim._
import scala.collection.mutable
import pon.gpon.GtcDsTest.check

/** The ECP5 fabric: line bits through to the pins on every upstream sampling phase, and the VCXO loop's sign. */
object SnifferBridgeTest extends App {
  val compiled = SimConfig.compile(SnifferBridge())

  /** Runs the bridge: ds period `dsPeriod` against 10 for the VCXO domain; upstream edges at `phase` samples
    * (a whole number: the edge sits on a sample, which reads either bit). Returns the pins' bit streams. */
  def run(phase: Double, dsPeriod: Int, cycles: Int, dsSignal: Boolean, seed: Int)(body: SnifferBridge => Unit = _ => ()) = {
    val rand = new scala.util.Random(seed)
    val dsBits = Array.fill(cycles * 25)(rand.nextInt(2))
    val usBits = Array.tabulate(cycles * 11)(i => if (i < 200) i % 2 else rand.nextInt(2)) // alternating preamble
    def sample(j: Int): Int = {
      val t = (j - phase) / 2
      val bit = if (t == t.floor && rand.nextBoolean()) t.toInt - 1 else t.floor.toInt
      usBits(bit max 0)
    }
    val dsOut, usOut = mutable.ArrayBuffer[Int]()
    var dsWord, usWord = 0
    var odd = mutable.ArrayBuffer[Boolean]()
    compiled.doSim(seed) { dut =>
      dut.io.dsSignal #= dsSignal
      dut.io.dsWord #= 0
      dut.io.usWord #= 0
      dut.dsCd.onSamplings {
        dut.io.dsWord #= (0 until 20).map(k => BigInt(dsBits((20 * dsWord + k) % dsBits.length)) << k).sum
        dsWord += 1
      }
      dut.clockDomain.onSamplings {
        dut.io.usWord #= (0 until 20).map(k => BigInt(sample(20 * usWord + k)) << k).sum
        usWord += 1
        odd += dut.io.usOdd.toBoolean
      }
      dut.outCd.onSamplings {
        val ds = dut.io.dsPins.toBigInt
        val us = dut.io.usPins.toBigInt
        dsOut ++= (31 to 0 by -1).map(k => ((ds >> k) & 1).toInt)
        usOut ++= (15 to 0 by -1).map(k => ((us >> k) & 1).toInt)
      }
      dut.clockDomain.forkStimulus(20) // even periods: forkStimulus halves them
      dut.dsCd.forkStimulus(2 * dsPeriod)
      dut.outCd.forkStimulus(32)
      body(dut)
      dut.clockDomain.waitSampling(cycles)
    }
    (dsBits.toSeq, usBits.toSeq, dsOut.toSeq, usOut.toSeq, odd.toSeq)
  }

  /** `sent` from bit `from` on, `length` bits, found contiguous in `got`. */
  def carried(sent: Seq[Int], got: Seq[Int], from: Int, length: Int): Boolean = {
    val at = got.indexOfSlice(sent.slice(from, from + 64))
    at >= 0 && got.slice(at, at + length) == sent.slice(from, from + length)
  }

  for ((phase, goodOdd) <- Seq((0.0, Some(true)), (0.5, None), (1.0, Some(false)), (1.5, None))) {
    val (dsBits, usBits, dsOut, usOut, odd) = run(phase, 10, 3000, true, (phase * 2).toInt)()
    check(carried(dsBits, dsOut, 2000, 40000), s"phase $phase: downstream bits in line order on the pins")
    check(carried(usBits, usOut, 400, 20000), s"phase $phase: upstream bits from 2x samples, past the preamble")
    goodOdd.foreach(o => check(odd.drop(100).forall(_ == o), s"phase $phase: the ${if (o) "odd" else "even"} sample kept"))
  }

  // ds faster than the VCXO: the FIFO fills, the duty rises; slower: falls; no downstream: holds
  for ((dsPeriod, signal, what) <- Seq((9, true, "rises"), (11, true, "falls"), (9, false, "holds"))) {
    var duty = 0L
    var dutySum, dutyCycles = 0L
    val dutySeq = mutable.ArrayBuffer[Long]()
    val ones = mutable.Map[Long, (Long, Long)]() // duty -> (cycles, vc high)
    run(0.5, dsPeriod, 120000, signal, 7) { dut =>
      dut.clockDomain.onSamplings {
        duty = dut.io.duty.toLong
        if (dutySeq.lastOption != Some(duty)) dutySeq += duty
        dutySum += duty
        dutyCycles += 1
        val (n, h) = ones.getOrElse(duty, (0L, 0L))
        ones(duty) = (n + 1, h + (if (dut.io.vcxo.toBoolean) 1 else 0))
      }
    }
    val mean = dutySum.toDouble / dutyCycles - 0x8000 // the error saw-tooths when slower: judge the mean duty
    val ok = what match { case "rises" => mean > 50 case "falls" => mean < -50 case _ => ones.keySet == Set(0x8000L) }
    check(ok, f"ds period $dsPeriod%d, downstream ${if (signal) "on" else "off"}: duty $what (mean $mean%+.0f LSB, ${dutySeq.map(_.toHexString).mkString(" ")})")
    check(ones.filter(_._2._1 >= 8192).forall { case (d, (n, h)) => math.abs(h.toDouble / n - d / 65536.0) < 0.002 },
      "sigma-delta mean is the duty")
  }
  println("PASS SnifferBridgeTest")
}
