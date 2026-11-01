// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.core.sim._
import pon.gpon.GtcDsTest.check

/** EthBoardCore with its lane looped back at a bit offset: error answers while the lane is down, laser forced off,
  * then autonegotiation and frames from the CPU TX queue back in the RX queue. */
object EthBoardTest extends App {
  val ETH_STATUS = 0x04
  val SFP_CTRL = 0x08
  val STATUS = 0x0C
  val PCS = 0x14
  val TX_HEAD = 0x100
  val TX_DATA = 0x104
  val RX_DESC_0 = 0x108
  val RX_DESC_1 = 0x10C
  val RX_DATA = 0x110

  SimConfig.compile(EthBoardCore(anSpeedup = true)).doSim { dut =>
    val io = dut.io
    val mgmt = dut.clockDomain
    mgmt.forkStimulus(10)
    for (b <- Seq(io.lane.txResetAck, io.lane.rxResetAck, io.lane.txReady, io.lane.rxReady, io.lane.txPllLocked,
           io.lane.rxLockedToData, io.lane.rxLockedToRef, io.sfp.modAbs, io.sfp.los, io.sfp.txFault,
           io.sfp.scl.read, io.sfp.sda.read)) b #= false
    io.rxWord #= 0
    for (f <- Seq(io.olt, io.onu)) f.tx.valid #= false
    io.txClk #= false
    io.rxClk #= false
    fork { // 125 MHz lane, looped back 4 bits late
      var previous = 0
      while (true) {
        sleep(4); io.txClk #= true; io.rxClk #= true
        val word = io.txWord.toInt
        io.rxWord #= ((word << 10 | previous) >> 4) & 0x3FF
        previous = word
        sleep(4); io.txClk #= false; io.rxClk #= false
      }
    }
    val hps = new HpsAxi(io.axi, mgmt)
    import hps._
    mgmt.waitSampling(20)
    check(read(RX_DESC_0)._2 == SLVERR && io.sfp.txDisable.toBoolean, "lane down: queues answer an error, laser forced off")
    io.lane.txReady #= true
    io.lane.rxReady #= true
    mgmt.waitSampling(20)
    check((read(STATUS)._1 & 8) != 0 && (read(ETH_STATUS)._1 & 0xC) == 0xC, "alive once both lane sides are ready")
    write(SFP_CTRL, 0)
    mgmt.waitSamplingWhere(200000)((read(PCS)._1 & 6) == 6)
    check((read(PCS)._1 & 6) == 6 && !io.sfp.txDisable.toBoolean, "PCS synchronized, autonegotiation complete, laser on")

    val rnd = new scala.util.Random(4)
    val frames = Seq(64, 1500, 333).map(n => Seq.fill(n)(rnd.nextInt(256)))
    for (f <- frames) {
      write(TX_HEAD, f.length << 16)
      for (g <- f.grouped(4)) write(TX_DATA, (g ++ Seq.fill(4 - g.length)(0)).foldLeft(0L)((a, b) => (a << 8) | b))
    }
    val got = for (_ <- frames) yield {
      mgmt.waitSamplingWhere(200000)((read(RX_DESC_0)._1 >> 31) == 1)
      val d1 = read(RX_DESC_1)._1
      val bytes = (d1 & 0xFFF).toInt
      val words = (0 until ((d1 >> 16) & 0x7FF).toInt).map(_ => read(RX_DATA)._1)
      (words.flatMap(w => (3 to 0 by -1).map(i => (w >> (8 * i) & 0xFF).toInt)).take(bytes), ((d1 >> 28) & 7).toInt)
    }
    check(got.map(_._1) == frames && got.forall(_._2 == 1), s"${frames.length} frames CPU -> lane -> CPU, whole, end set, not cut")
  }
  println("PASS EthBoardTest")
}
