// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.core.sim._
import pon.gpon.GtcDsTest.check

case class VcxoBench(updateShift: Int) extends Component {
  val rx = ClockDomain.external("rx")
  val tracker = VcxoTracker(rx, updateShift)
  val io = new Bundle {
    val enable = in Bool ()
    val center, kp, ki = in UInt (16 bits)
    val phase = out SInt (16 bits)
    val duty = out UInt (16 bits)
    val pin = out Bool ()
  }
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

/** VcxoTracker against a VCXO model (frequency up with the filtered duty, +-1 % over the range) and a recovered
  * clock 0.31 % slower: the loop pulls the local clock onto it and holds the phase. The ONU's upstream timing follows
  * that phase, so its wander must stay under the OLT's drift threshold (DOWi re-ranges, TIWi deactivates). */
object VcxoTrackerTest extends App {
  val updateShift = 8
  SimConfig.withTimeScale(1 ps).withTimePrecision(1 ps).compile(VcxoBench(updateShift)).doSim { dut =>
    val io = dut.io
    val local = dut.clockDomain
    val rxHalf = 1605
    dut.rx.forkStimulus(2 * rxHalf)
    io.enable #= false
    io.center #= 0x8000
    io.kp #= 32800
    io.ki #= 21000
    var filtered = 0.5 // RC on the card
    var cycles, rxCycles = 0L
    var pinHigh = 0L
    local.fallingEdge()
    local.assertReset()
    var edge = 0.0 // exact time of the next local edge, ps: a continuous frequency, not one quantized to 1 ps
    val wander = new Array[Double](1 << 17) // local edge time less the recovered clock's, ps
    fork {
      while (true) {
        val half = 1600 / (1 + 0.02 * (filtered - 0.5))
        for (_ <- 0 until 2) { edge += half; sleep(math.round(edge) - simTime()); local.clockToggle() }
        wander((cycles & ((1 << 17) - 1)).toInt) = edge - 2 * rxHalf * cycles
        cycles += 1
        if (io.pin.toBoolean) pinHigh += 1
        if (cycles % (1 << updateShift) == 0) filtered += 0.3 * (io.duty.toLong / 65536.0 - filtered)
      }
    }
    dut.rx.onSamplings { rxCycles += 1 }
    sleep(20000)
    local.deassertReset()
    local.waitSampling(4 << updateShift)
    check(io.duty.toInt == 0x8000 && io.phase.toInt == 0, "off: duty at center, no error")
    io.enable #= true
    local.waitSampling(1500 << updateShift)
    val (c0, r0, p0) = (cycles, rxCycles, pinHigh)
    var worst = 0
    for (_ <- 0 until 400) { local.waitSampling(1 << updateShift); worst = worst max io.phase.toInt.abs }
    val ratio = (cycles - c0).toDouble / (rxCycles - r0)
    val usBit = 2 * rxHalf / 4.0 // one upstream bit: a quarter of the 311.04 MHz clock
    val peak = (wander.max - wander.min) / usBit
    check(worst <= 3, s"locked: phase error within 3 clocks (worst $worst)")
    check(peak < 4, f"upstream phase wander $peak%.2f bits peak to peak, under DOWi (G.984.3 Table 10-2: 4 bits)")
    check(math.abs(ratio - 1) < 2e-4, s"frequency follows the recovered clock (ratio $ratio)")
    val mean = (pinHigh - p0).toDouble / (cycles - c0)
    check(math.abs(mean - io.duty.toLong / 65536.0) < 0.01, s"sigma-delta pin mean = duty ($mean, ${io.duty.toLong})")
  }
  println("PASS VcxoTrackerTest")
}
