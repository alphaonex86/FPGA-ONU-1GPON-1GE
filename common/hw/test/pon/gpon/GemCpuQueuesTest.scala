// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba3.apb.sim.Apb3Driver
import scala.collection.mutable
import GtcDsTest.check

/** GemCpuQueues with three unrelated clocks and small queues: packets leave whole and contiguous, queues filled
  * exactly, refused heads, cut, truncated and dropped fragments. */
case class GemQueuesBench() extends Component {
  val ds = ClockDomain.external("ds")
  val us = ClockDomain.external("us")
  val queues = GemCpuQueues(ds, us, txWords = 16, rxWords = 16, rxFragments = 4)
  val io = new Bundle {
    val apb = slave(Apb3(Apb3Config(addressWidth = 8, dataWidth = 32)))
    val tx = master Stream (Fragment(GemTxByte()))
    val rx = slave Flow (Fragment(UsGemByte()))
    val rxCut = in Bool ()
    val irq = out Bool ()
  }
  io.apb <> queues.io.apb
  io.tx <> queues.io.tx
  io.rx <> queues.io.rx
  io.rxCut <> queues.io.rxCut
  io.irq := queues.io.irq
}

object GemCpuQueuesTest extends App {
  val TX_HEAD = 0x00
  val TX_DATA = 0x04
  val RX_DESC_0 = 0x08
  val RX_DESC_1 = 0x0C
  val RX_DATA = 0x10
  val ERRORS = 0x18

  SimConfig.compile(GemQueuesBench()).doSim { dut =>
    val io = dut.io
    val cpu = dut.clockDomain
    cpu.forkStimulus(10)
    dut.ds.forkStimulus(3)
    dut.us.forkStimulus(7)
    io.rx.valid #= false
    io.rxCut #= false
    io.tx.ready #= true
    val apb = Apb3Driver(io.apb, cpu)
    cpu.waitSampling(20)

    val sent = mutable.Queue[(Int, Seq[Int])]()
    var gap = false
    var current = mutable.ArrayBuffer[Int]()
    dut.ds.onSamplings {
      if (io.tx.valid.toBoolean) {
        current += io.tx.data.toInt
        if (io.tx.last.toBoolean) { sent += ((io.tx.portId.toInt, current.toSeq)); current = mutable.ArrayBuffer[Int]() }
      } else if (current.nonEmpty) gap = true
    }
    def words(bytes: Seq[Int]): Seq[Long] =
      bytes.grouped(4).map(g => (g ++ Seq.fill(4 - g.length)(0)).foldLeft(0L)((a, b) => (a << 8) | b)).toSeq
    def head(port: Int, length: Int): Unit = apb.write(TX_HEAD, (length << 16) | port)
    def errors(): Int = { val e = apb.read(ERRORS).toInt; apb.write(ERRORS, e); e }

    val p1 = (1 to 10).map(_ * 7 & 0xFF)
    head(0x123, p1.length)
    for (w <- words(p1).take(2)) apb.write(TX_DATA, w)
    cpu.waitSampling(50)
    check(sent.isEmpty && current.isEmpty, "TX: nothing leaves before the whole packet is in")
    head(0x55, 4)
    check(errors() == 1, "TX: a head while the packet is incomplete is refused")
    apb.write(TX_DATA, words(p1)(2))
    cpu.waitSampling(100) // p1 leaves the queue
    val p2 = (0 until 60).map(i => (i * 13 + 1) & 0xFF) // 15 words + head: the 16-word queue exactly
    head(0x0AB, p2.length)
    for (w <- words(p2)) apb.write(TX_DATA, w)
    apb.write(TX_DATA, 0)
    check(errors() == 2, "TX: a data word beyond the length is refused")
    cpu.waitSampling(100)
    check(sent.toSeq == Seq((0x123, p1), (0x0AB, p2)) && !gap, "TX: both packets whole, one byte per clock, queue filled exactly")
    head(0x1, 61 * 4)
    check(errors() == 1, "TX: a packet larger than the queue is refused")

    def fragment(port: Int, alloc: Int, bytes: Seq[Int], end: Boolean, cut: Boolean = false): Unit = {
      for ((b, k) <- bytes.zipWithIndex) {
        io.rx.valid #= true
        io.rx.gem.portId #= port
        io.rx.allocId #= alloc
        io.rx.gem.pti #= (if (end) 1 else 0)
        io.rx.gem.data #= b
        io.rx.last #= !cut && k == bytes.length - 1
        dut.us.waitSampling()
      }
      io.rx.valid #= false
      if (cut) { io.rxCut #= true; dut.us.waitSampling(); io.rxCut #= false }
      dut.us.waitSampling(3)
    }
    case class Got(port: Int, alloc: Int, bytes: Int, end: Boolean, cut: Boolean, truncated: Boolean, data: Seq[Long])
    def receive(): Got = {
      cpu.waitSamplingWhere(1000)((apb.read(RX_DESC_0) >> 31) == 1)
      val d0 = apb.read(RX_DESC_0).toLong
      val d1 = apb.read(RX_DESC_1).toLong
      val n = ((d1 >> 16) & 0x7FF).toInt
      Got((d0 & 0xFFF).toInt, ((d0 >> 16) & 0xFFF).toInt, (d1 & 0xFFF).toInt, ((d1 >> 28) & 1) == 1, ((d1 >> 29) & 1) == 1,
        ((d1 >> 30) & 1) == 1, (0 until n).map(_ => apb.read(RX_DATA).toLong))
    }
    val a = (0 until 10).map(i => (i * 29 + 3) & 0xFF)
    val b = Seq(0xA1, 0xB2, 0xC3)
    val c = (0 until 64).map(i => (i * 5 + 9) & 0xFF) // the 16-word queue exactly
    fragment(5, 7, a, end = true)
    fragment(6, 8, b, end = false, cut = true)
    fragment(9, 10, Seq(0x42), end = false)
    check(receive() == Got(5, 7, 10, true, false, false, words(a)), "RX: fragment, user frame end")
    check(receive() == Got(6, 8, 3, false, true, false, words(b)), "RX: cut fragment, partial word")
    check(receive() == Got(9, 10, 1, false, false, false, words(Seq(0x42))), "RX: one-byte fragment, more to come")
    fragment(11, 12, c, end = true)
    check(receive() == Got(11, 12, 64, true, false, false, words(c)), "RX: data queue filled exactly")
    fragment(13, 14, c ++ c.take(8), end = true)
    check(receive() == Got(13, 14, 72, true, false, true, words(c)), "RX: overflow truncates, flagged in the descriptor")
    check(errors() == 16, "RX: ERRORS.rxTruncated")
    for (k <- 0 until 5) fragment(20 + k, 1, Seq(k), end = true)
    check((0 until 4).map(_ => receive().port) == (20 until 24), "RX: four fragments fill the descriptor queue")
    check(errors() == 8 && (apb.read(RX_DESC_0) >> 31) == 0, "RX: the fifth is dropped, ERRORS.rxDropped")
    apb.read(RX_DATA)
    check(errors() == 4, "RX: RX_DATA without a ready word, ERRORS.rxUnderflow")
  }
  println("PASS GemCpuQueuesTest")
}
