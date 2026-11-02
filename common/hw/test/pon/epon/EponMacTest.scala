// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.epon

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.collection.mutable
import pon.Crc32
import pon.gpon.GtcDsTest.check

/** EponMacTx -> line (slipped by `slip` bits, code groups XORed with `corrupt`) -> EponMacRx. */
case class EponLoop() extends Component {
  val io = new Bundle {
    val input = slave Stream (Fragment(EponTxByte()))
    val slip = in UInt (4 bits)
    val corrupt = in Bits (10 bits)
    val frame = master Flow (Fragment(EponRxByte()))
    val good, synced, badPreamble, start, underrun = out Bool ()
  }
  val tx = EponMacTx()
  tx.io.input << io.input
  io.start := tx.io.start
  io.underrun := tx.io.underrun
  val line = tx.io.code ^ io.corrupt
  val rx = EponMacRx()
  rx.io.raw := ((line ## RegNext(line).init(0)) >> io.slip)(9 downto 0)
  io.frame << rx.io.frame
  io.good := rx.io.good
  io.synced := rx.io.synced
  io.badPreamble := rx.io.badPreamble
  tx.encoder.io.input.simPublic()
}

/** EponMacTx and EponMacRx back to back at every bit slip: frames of random length, LLID and mode received whole with
  * their FCS and `good`; the preamble on the line as the CRC-8 model; IPG; a byte missing in a frame (/V/) and a
  * corrupted code group fail the frame, a corrupted preamble drops it. */
object EponMacTest extends App {
  val rnd = new scala.util.Random(8)
  SimConfig.compile(EponLoop()).doSim { dut =>
    val io = dut.io
    val cd = dut.clockDomain
    cd.forkStimulus(8)
    io.input.valid #= false
    io.slip #= 0
    io.corrupt #= 0
    case class Frame(bytes: Seq[Int], mode: Boolean, llid: Int)
    val received = mutable.ArrayBuffer[(Frame, Boolean)]()
    val current = mutable.ArrayBuffer[Int]()
    var badPreambles, underruns = 0
    var cycle = 0L
    var lastEnd = -100L
    val gaps = mutable.ArrayBuffer[Long]()
    val preambles = mutable.ArrayBuffer[mutable.ArrayBuffer[Int]]()
    var preambleLeft = 0
    val enc = dut.tx.encoder.io.input
    cd.onSamplings {
      cycle += 1
      if (io.frame.valid.toBoolean) {
        current += io.frame.fragment.data.toInt
        if (io.frame.last.toBoolean) {
          received += ((Frame(current.toList, io.frame.fragment.mode.toBoolean, io.frame.fragment.llid.toInt), io.good.toBoolean))
          current.clear()
        }
      }
      if (io.badPreamble.toBoolean) badPreambles += 1
      if (io.underrun.toBoolean) underruns += 1
      if (preambleLeft > 0) { preambles.last += enc.data.toInt; preambleLeft -= 1 }
      if (io.start.toBoolean) { gaps += cycle - lastEnd - 5; preambles += mutable.ArrayBuffer[Int](); preambleLeft = 7 }
      if (io.input.valid.toBoolean && io.input.ready.toBoolean && io.input.last.toBoolean) lastEnd = cycle
    }
    def send(f: Frame, holeAt: Int = -1): Unit = {
      io.input.fragment.mode #= f.mode
      io.input.fragment.llid #= f.llid
      for ((b, i) <- f.bytes.zipWithIndex) {
        if (i == holeAt) { io.input.valid #= false; cd.waitSampling() }
        io.input.valid #= true
        io.input.fragment.data #= b
        io.input.last #= i == f.bytes.length - 1
        cd.waitSamplingWhere(io.input.ready.toBoolean)
      }
      io.input.valid #= false
    }
    def frame() = Frame(Seq.fill(60 + rnd.nextInt(200))(rnd.nextInt(256)), rnd.nextBoolean(), rnd.nextInt(1 << 15))
    def withFcs(f: Frame) = f.copy(bytes = f.bytes ++ (0 until 4).map(k => ((Crc32.model(f.bytes) >> (8 * k)) & 0xFF).toInt))
    def expectedPreamble(f: Frame) = {
      val covered = Seq(0xD5, 0x55, 0x55, (if (f.mode) 0x80 else 0) | f.llid >> 8, f.llid & 0xFF)
      0x55 +: covered :+ Epon.crc8Model(covered) // after /S/
    }

    val sent = mutable.ArrayBuffer[Frame]()
    for (slip <- 0 until 10) {
      io.slip #= slip
      cd.waitSampling(40)
      check(io.synced.toBoolean, s"slip $slip: synchronized on the IDLEs")
      val before = received.length
      val frames = Seq.fill(5)(frame())
      for (f <- frames) { send(f); cd.waitSampling(rnd.nextInt(3)) }
      cd.waitSampling(40)
      sent ++= frames
      check(received.drop(before).toSeq == frames.map(f => (withFcs(f), true)),
        s"slip $slip: 5 frames back to back, LLID, mode, bytes and FCS, good")
    }
    check(preambles.take(sent.length).map(_.toSeq).toSeq == sent.map(expectedPreamble).toSeq, s"preambles on the line as the CRC-8 model (first ${preambles.head.map(_.toHexString).mkString(" ")})")
    check(gaps.drop(1).forall(_ >= Epon.ipg), s"IPG >= ${Epon.ipg} bytes (min ${gaps.drop(1).min})")

    var before = received.length
    send(frame(), holeAt = 30)
    cd.waitSampling(40)
    check(underruns == 1 && received.length == before + 1 && !received.last._2, "a missing byte: /V/, underrun, frame not good")
    before = received.length
    val f = frame()
    fork { cd.waitSamplingWhere(io.start.toBoolean); cd.waitSampling(40); io.corrupt #= 0x3FF; cd.waitSampling(); io.corrupt #= 0 }
    send(f)
    cd.waitSampling(60)
    check(received.length == before + 1 && !received.last._2, "a corrupted code group in the data: frame not good")
    before = received.length
    fork { cd.waitSamplingWhere(io.start.toBoolean); cd.waitSampling(6); io.corrupt #= 0x00C; cd.waitSampling(); io.corrupt #= 0 }
    send(frame())
    cd.waitSampling(60)
    check(received.length == before, s"a corrupted preamble: frame dropped ($badPreambles bad preamble)")
    send(frame())
    cd.waitSampling(60)
    check(received.length == before + 1 && received.last._2, "the next frame received")
  }
  println("PASS EponMacTest")
}
