// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.collection.mutable
import pon.gpon.GtcDsTest.check

/** Two Ethernet ports back to back over 10-bit lines, each direction at its own bit offset: autonegotiation
  * (1000BASE-X), then frames both ways, received whole with a good FCS. */
object EthPortTest extends App {
  case class Pair(offsetAb: Int, offsetBa: Int) extends Component {
    val io = new Bundle {
      val tx = Vec(slave(Stream(Fragment(Bits(8 bits)))), 2)
      val rx = Vec(master(Flow(Fragment(Bits(8 bits)))), 2)
      val rxBad = out Bits (2 bits)
      val anComplete, synced = out Bits (2 bits)
    }
    val ports = Seq.fill(2)(EthPort(ClockDomain.current, ClockDomain.current))
    def line(from: EthPort, offset: Int): Bits = { val previous = RegNext(from.io.rawTx) init (0); (from.io.rawTx ## previous)(offset + 9 downto offset) }
    ports(1).io.rawRx := line(ports(0), offsetAb)
    ports(0).io.rawRx := line(ports(1), offsetBa)
    for ((p, i) <- ports.zipWithIndex) {
      p.io.tx << io.tx(i)
      io.rx(i) << p.io.rx
      io.rxBad(i) := p.io.rxBad
      io.anComplete(i) := p.io.anComplete
      io.synced(i) := p.io.synced
      p.io.sgmii := False
      p.io.anSpeedup := True
    }
  }
  SimConfig.compile(Pair(3, 7)).doSim { dut =>
    val io = dut.io
    val cd = dut.clockDomain
    val rnd = new scala.util.Random(2)
    val queues = Seq.fill(2)(mutable.Queue[Seq[Int]]())
    val received = Seq.fill(2)(mutable.ArrayBuffer[(Seq[Int], Boolean)]())
    val current = Array.fill(2)(mutable.ArrayBuffer[Int]())
    val index = Array(0, 0)
    for (i <- 0 until 2) io.tx(i).valid #= false
    cd.onSamplings {
      for (i <- 0 until 2) {
        val tx = io.tx(i)
        if (tx.valid.toBoolean && tx.ready.toBoolean) {
          index(i) += 1
          if (index(i) == queues(i).head.length) { queues(i).dequeue(); index(i) = 0 }
        }
        tx.valid #= queues(i).nonEmpty
        for (f <- queues(i).headOption) { tx.fragment #= f(index(i)); tx.last #= index(i) == f.length - 1 }
        val rx = io.rx(i)
        if (rx.valid.toBoolean) {
          current(i) += rx.fragment.toInt
          if (rx.last.toBoolean) { received(i) += ((current(i).toSeq, (io.rxBad.toInt >> i & 1) == 1)); current(i).clear() }
        }
      }
    }
    cd.forkStimulus(8)
    cd.waitSampling(10)
    cd.waitSamplingWhere(50000)(io.anComplete.toInt == 3 && io.synced.toInt == 3)
    check(io.anComplete.toInt == 3, "autonegotiation complete on both ports, PCS synchronized")
    val frames = Seq.fill(2)((0 until 20).map(_ => Seq.fill(60 + rnd.nextInt(1400))(rnd.nextInt(256))))
    for (i <- 0 until 2) queues(i) ++= frames(i)
    cd.waitSamplingWhere(200000)(received.forall(_.length == 20))
    for (i <- 0 until 2)
      check(received(1 - i).map(_._1) == frames(i) && received(1 - i).forall(!_._2),
        s"port $i -> ${1 - i}: ${received(1 - i).length} frames of 60-1459 bytes, good FCS")
  }
  println("PASS EthPortTest")
}
