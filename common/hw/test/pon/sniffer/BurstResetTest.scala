// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.collection.mutable
import pon.gpon.GtcDsTest.check

/** BurstReset with its time counter. Test only. */
case class BurstResetBench() extends Component {
  val dut = BurstReset(depth = 4)
  val io = new Bundle {
    val frameStart = in Bool ()
    val alloc = slave(cloneOf(dut.io.alloc))
    val flush = in Bool ()
    val offset = in UInt (32 bits)
    val lead, width = in UInt (16 bits)
    val reset = out Bool ()
    val lost = out UInt (16 bits)
    val time = out UInt (32 bits)
  }
  val time = Reg(UInt(32 bits)) init (0)
  time := time + 1
  dut.io.frameStart := io.frameStart
  dut.io.alloc << io.alloc
  dut.io.flush := io.flush
  dut.io.time := time
  dut.io.offset := io.offset
  dut.io.lead := io.lead
  dut.io.width := io.width
  io.reset := dut.io.reset
  io.lost := dut.io.lost
  io.time := time
}

/** One pulse per burst, `lead` before its StartTime at the tap, `width` long: contiguous entries share a burst, a new
  * frame starts one; an entry already past is lost and counted; a flush drops the plans. */
object BurstResetTest extends App {
  val (offset, lead, width) = (3000, 20, 8)
  SimConfig.compile(BurstResetBench()).doSim { dut =>
    val io = dut.io
    val cd = dut.clockDomain
    cd.forkStimulus(10)
    io.frameStart #= false
    io.alloc.valid #= false
    io.flush #= false
    io.offset #= offset
    io.lead #= lead
    io.width #= width
    val pulses = mutable.ArrayBuffer[(Long, Int)]() // (time of the first high clock, clocks high)
    val expected = mutable.ArrayBuffer[(Long, Int)]()
    var burst = false // the entry being driven starts a burst
    cd.onSamplings { // an entry's time: the clock the DUT takes it, as a record's
      if (io.alloc.valid.toBoolean && burst) expected += ((io.time.toLong + offset + 2 * io.alloc.start.toInt - lead, width))
      if (io.reset.toBoolean) {
        val t = io.time.toLong
        if (pulses.nonEmpty && pulses.last._1 + pulses.last._2 == t) pulses(pulses.length - 1) = (pulses.last._1, pulses.last._2 + 1)
        else pulses += ((t, 1))
      }
    }
    cd.waitSampling(10)
    def frame(): Unit = { io.frameStart #= true; cd.waitSampling(); io.frameStart #= false; cd.waitSampling(20) }
    def entry(start: Int, stop: Int, starts: Boolean): Unit = {
      io.alloc.valid #= true; io.alloc.allocId #= 1; io.alloc.flags #= 0; io.alloc.start #= start; io.alloc.stop #= stop
      burst = starts
      cd.waitSampling()
      io.alloc.valid #= false
      cd.waitSampling(3)
    }
    frame()
    entry(10, 99, starts = true)
    entry(100, 199, starts = false) // contiguous: the same burst
    entry(400, 500, starts = true)
    frame()
    entry(501, 600, starts = true) // contiguous with the last frame's: a new burst
    cd.waitSampling(offset + 1300)
    check(pulses == expected, s"pulses at the planned times: $pulses, expected $expected")
    check(io.lost.toInt == 0, "nothing lost")

    io.lead #= offset + 100 // planned before now
    frame()
    entry(0, 50, starts = false)
    cd.waitSampling(10)
    check(io.lost.toInt == 1, "an entry already past: lost, counted")

    io.lead #= lead
    val before = pulses.length
    frame()
    entry(10, 20, starts = false)
    io.flush #= true
    cd.waitSampling()
    io.flush #= false
    cd.waitSampling(offset + 100)
    check(pulses.length == before, "a flush drops the planned pulses")
  }
  println("PASS BurstResetTest")
}
