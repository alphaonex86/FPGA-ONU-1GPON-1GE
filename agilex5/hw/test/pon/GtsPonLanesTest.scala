// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.core.sim._
import scala.collection.mutable
import pon.gpon.GtcDsTest.check

/** GtsPonLanes over serial lines at 2488.32 Mb/s, first bit of each word in bit 0, with fibre delays of any bit count:
  * the MAC line bits come out as they went in, delayed by a constant. */
object GtsPonLanesTest extends App {
  SimConfig.compile(GtsPonLanes()).doSim { dut =>
    val ds = dut.clockDomain
    val us = dut.usDomain
    val io = dut.io
    val rnd = new scala.util.Random(7)
    for (x <- Seq(io.oltRxWord, io.onuRxWord, io.oltDsLine, io.onuUsLine, io.onuUsBen)) x #= 0
    ds.fallingEdge(); us.fallingEdge()
    ds.assertReset(); us.assertReset()
    fork {
      var n = 0
      while (true) { ds.clockToggle(); if (n % 2 == 0) us.clockToggle(); n += 1; sleep(2) }
    }
    sleep(41)
    ds.deassertReset(); us.deassertReset()

    def msbFirst(v: Int): Seq[Boolean] = (7 to 0 by -1).map(k => ((v >> k) & 1) == 1)
    def lsbFirst(v: Int): Seq[Boolean] = (0 to 7).map(k => ((v >> k) & 1) == 1)
    def wordOf(bits: Seq[Boolean]): Int = bits.zipWithIndex.map { case (b, k) => if (b) 1 << k else 0 }.sum
    val dsDelay = 13  // line bits
    val usDelay = 37  // line bits: odd, a half upstream bit
    val dsSent, dsGot, usSent, usGot = mutable.ArrayBuffer[Boolean]()
    val oltTx, onuTx, benTx = mutable.ArrayBuffer[Boolean]() // serial lines, line bits
    ds.onSamplings {
      oltTx ++= lsbFirst(io.oltTxWord.toInt)
      onuTx ++= lsbFirst(io.onuTxWord.toInt)
      benTx ++= lsbFirst(io.benTxWord.toInt)
      dsGot ++= msbFirst(io.onuDsLine.toInt)
      val d = rnd.nextInt(256)
      io.oltDsLine #= d
      dsSent ++= msbFirst(d)
      def rx(line: mutable.ArrayBuffer[Boolean], delay: Int) =
        wordOf((0 until 8).map(k => line.lift(line.length - 8 - delay + k).contains(true)))
      io.onuRxWord #= rx(oltTx, dsDelay)
      io.oltRxWord #= rx(onuTx, usDelay)
    }
    us.onSamplings {
      usGot ++= msbFirst(io.oltUsLine.toInt)
      val u = rnd.nextInt(256)
      io.onuUsLine #= u
      io.onuUsBen #= 0xFF
      usSent ++= msbFirst(u)
    }
    ds.waitSampling(4000)

    def lag(sent: Seq[Boolean], got: Seq[Boolean]): Option[Int] =
      (0 until 200).find(l => (500 until sent.length - 300).forall(i => got(i + l) == sent(i)))
    val dsLag = lag(dsSent.toSeq, dsGot.toSeq)
    check(dsLag.nonEmpty, s"downstream line bits through TX, a $dsDelay-bit fibre and RX (lag ${dsLag})")
    val usLag = lag(usSent.toSeq, usGot.toSeq)
    check(usLag.nonEmpty, s"upstream bits sent twice, a $usDelay-bit fibre, every other bit kept (lag ${usLag})")
    check(benTx.drop(100).forall(identity), "burst enable lane on with the line")
    check(onuTx.drop(100).grouped(2).forall(p => p.length < 2 || p(0) == p(1)), "upstream line bits in pairs")
  }
  println("PASS GtsPonLanesTest")
}
