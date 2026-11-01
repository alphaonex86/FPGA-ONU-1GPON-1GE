// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.core.sim._
import pon.gpon.GtcDsTest.check

case class VcxoBench(integralShift: Int) extends Component {
  val tracker = VcxoTracker(integralShift)
  val io = new Bundle {
    val rxClk, localClk = in Bool ()
    val enable = in Bool ()
    val center, kp, ki = in UInt (16 bits)
    val phase = out SInt (16 bits)
    val duty = out UInt (16 bits)
    val pin = out Bool ()
  }
  tracker.io.rxClk := io.rxClk
  tracker.io.localClk := io.localClk
  tracker.io.enable := io.enable
  tracker.io.center := io.center
  tracker.io.kp := io.kp
  tracker.io.ki := io.ki
  io.phase := tracker.io.phase
  io.duty := tracker.io.duty
  val modulator = SigmaDelta()
  modulator.io.duty := tracker.io.duty
  io.pin := modulator.io.pin
}

/** VcxoTracker (DDMTD, helper 6436 ps, the clocks halved as on the board) against a VCXO model (100 ppm slow at the center duty, +-200 ppm over the duty
  * range, a first-order filter, 1 ps rms edge jitter) and a 311.04 MHz recovered clock: the loop pulls the local
  * clock onto it and holds the phase. The ONU's upstream timing follows that phase, so its wander must stay well under
  * the OLT's drift threshold (DOWi re-ranges past 4 bits, TIWi deactivates past 8). Loop time compressed: a fast
  * filter and gains to match, the integral shift of a fast loop. */
object VcxoTrackerTest extends App {
  SimConfig.withTimeScale(1 ps).withTimePrecision(1 ps).compile(VcxoBench(integralShift = 24)).doSim(seed = 7) { dut =>
    val io = dut.io
    val helper = dut.clockDomain
    val rxPeriod = 3215 // 311.04 MHz
    val usBit = rxPeriod / 4.0
    helper.forkStimulus(6436)
    io.enable #= false
    io.center #= 0x8000
    io.kp #= 6000
    io.ki #= 36000
    io.rxClk #= false
    io.localClk #= false
    fork { while (true) { sleep(rxPeriod); io.rxClk #= !io.rxClk.toBoolean } } // halved: one level per clock
    var filtered = 0.5 // duty seen by the VCXO
    var edge = 0.0     // exact time of the next local edge, ps
    var cycles, pinHigh, helperCycles = 0L
    val window = 1 << 18
    val wander = new Array[Double](window) // local rising edge less the recovered clock's, ps
    val rng = new scala.util.Random(1)
    fork {
      while (true) {
        edge += rxPeriod * (1 + 100e-6 - 400e-6 * (filtered - 0.5))
        sleep(math.max(1, math.round(edge + rng.nextGaussian()) - simTime()))
        io.localClk #= !io.localClk.toBoolean
        wander((cycles % window).toInt) = edge - rxPeriod * (cycles + 1)
        cycles += 1
        if ((cycles & 255) == 0) filtered += (io.duty.toLong / 65536.0 - filtered) * 256 / 20000.0
      }
    }
    helper.onSamplings { helperCycles += 1; if (io.pin.toBoolean) pinHigh += 1 }
    helper.waitSampling(4000)
    check(io.duty.toInt == 0x8000 && io.phase.toInt == 0, "off: duty at center, no error")
    io.enable #= true
    helper.waitSampling(4000000)
    val (c0, h0, p0) = (cycles, helperCycles, pinHigh)
    helper.waitSampling(window + 1000)
    val peak = (wander.max - wander.min) / usBit
    val ppm = ((cycles - c0) * rxPeriod.toDouble / ((helperCycles - h0) * 6436.0) - 1) * 1e6
    check(math.abs(ppm) < 5, f"frequency follows the recovered clock ($ppm%.2f ppm)")
    check(peak < 1, f"upstream phase wander $peak%.3f bits peak to peak over $window clocks (DOWi: 4 bits)")
    val mean = (pinHigh - p0).toDouble / (helperCycles - h0)
    check(math.abs(mean - io.duty.toLong / 65536.0) < 0.01, s"sigma-delta pin mean = duty ($mean, ${io.duty.toLong})")
  }
  println("PASS VcxoTrackerTest")
}
