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

/** VcxoTracker against a VCXO model (frequency up with the filtered duty, +-1 % over the range, 1 ps steps) and a
  * recovered clock 0.31 % slower: the loop pulls the local clock onto it and holds the phase. */
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
    fork {
      while (true) {
        val half = math.round(1600 / (1 + 0.02 * (filtered - 0.5)))
        sleep(half); local.clockToggle(); sleep(half); local.clockToggle()
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
    check(worst <= 3, s"locked: phase error within 3 clocks (worst $worst)")
    check(math.abs(ratio - 1) < 2e-4, s"frequency follows the recovered clock (ratio $ratio)")
    val mean = (pinHigh - p0).toDouble / (cycles - c0)
    check(math.abs(mean - io.duty.toLong / 65536.0) < 0.01, s"sigma-delta pin mean = duty ($mean, ${io.duty.toLong})")
  }
  println("PASS VcxoTrackerTest")
}
