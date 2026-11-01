// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.lib._

/** Steers the VCXO so the local clock (the ONU TX clock) follows the recovered downstream clock: the ONU's upstream
  * timing follows their phase, which an OLT holds within 4 bits, a fifth of a 311.04 MHz clock (G.984.3 Table 10-2).
  * Phase detector: dual mixer time difference. Inputs: each clock halved by a toggle flop (a clock output may not
  * reach a data input). The default domain is a helper clock slower than them by a small fraction d; its samples
  * of both rise every 1/d helper cycles, spaced by their phase in steps of d period (helper 155.357 MHz against
  * 155.52: 6.7 ps); frequency capture +-d/2. PI loop on the unwrapped phase, one update per beat:
  * duty = center + (kp * phase >> 12) + (sum(ki * phase) >> integralShift), a higher duty raising the VCXO frequency;
  * `update` pulses once the new duty is out. Off: duty = center and the phase restarts from zero. */
case class VcxoTracker(integralShift: Int = 32, glitchCycles: Int = 128) extends Component {
  val io = new Bundle {
    val rxClk, localClk = in Bool () // the clocks halved: square waves, sampled as data
    val enable = in Bool ()
    val center = in UInt (16 bits)
    val kp, ki = in UInt (16 bits)
    val phase = out SInt (16 bits) // local behind rx, helper cycles, saturated
    val duty = out UInt (16 bits)
    val update = out Bool ()
  }
  val time = Reg(UInt(16 bits)) init (0)
  time := time + 1
  /** Rising edges of a clock's beat: the first one after glitchCycles zeros, so jitter flicker at an edge counts once. */
  class Beat(clk: Bool) extends Area {
    val sample = BufferCC(clk, False)
    val zeros = Reg(UInt(log2Up(glitchCycles + 1) bits)) init (0)
    when(sample)(zeros := 0) elsewhen (zeros =/= glitchCycles)(zeros := zeros + 1)
    val edge = sample && zeros === glitchCycles
    val tag = RegNextWhen(time, edge) init (0)
  }
  val rx = new Beat(io.rxClk)
  val local = new Beat(io.localClk)

  // each local beat: spacing from the last rx beat, its change since the last local beat wrapped into +-period/2
  val measured = RegNext(local.edge) init (False)
  val spacing = RegNextWhen(time - rx.tag, local.edge)
  val period = RegNextWhen(time - local.tag, local.edge)
  val lastSpacing = Reg(UInt(16 bits)) init (0)
  val change = Reg(SInt(18 bits))
  when(measured) { change := (spacing.intoSInt - lastSpacing.intoSInt).resize(18); lastSpacing := spacing }
  val wrapStep = RegNext(measured) init (False)
  val half = (period >> 1).intoSInt.resize(18)
  val wrapped = Reg(SInt(18 bits))
  when(wrapStep) {
    wrapped := (change > half) ? (change - period.intoSInt) | ((change < -half) ? (change + period.intoSInt) | change)
  }
  val accumulate = RegNext(wrapStep) init (False)
  val primed = RegInit(False) // a first beat after enable gives the reference
  val error = Reg(SInt(24 bits)) init (0)
  val sum = Reg(SInt(integralShift + 24 bits)) init (0)
  // products and clamp pipelined: the error only changes once per beat
  val proportional = RegNext(RegNext(error * io.kp.intoSInt) >> 12)
  val integral = RegNext(RegNext(RegNext(error * io.ki.intoSInt)))
  when(accumulate) {
    error := (io.enable && primed) ? (error + wrapped.resize(24)) | S(0, 24 bits)
    sum := (io.enable && primed) ? (sum + integral.resize(integralShift + 24)) | S(0, integralShift + 24 bits)
    primed := io.enable
  }
  val control = RegNext(proportional.resize(34) + (sum >> integralShift).resize(34))
  val target = RegNext(io.center.intoSInt.resize(34) + control)
  val duty = RegNext((target < 0) ? U(0, 16 bits) | ((target > 0xFFFF) ? U(0xFFFF, 16 bits) | target.asUInt.resize(16))) init (0)
  io.duty := duty
  io.phase := error.sat(8)
  io.update := Delay(accumulate, 9, init = False) // past the pipeline
}

/** First-order sigma-delta of `duty` / 65536 on a pin, one step every 16 clocks (RC-filtered outside). */
case class SigmaDelta() extends Component {
  val io = new Bundle {
    val duty = in UInt (16 bits)
    val pin = out Bool ()
  }
  val step = Reg(UInt(4 bits)) init (0)
  step := step + 1
  val accumulator = Reg(UInt(17 bits)) init (0)
  when(step === 0)(accumulator := accumulator(15 downto 0).resize(17) + io.duty)
  io.pin := RegNext(accumulator(16)) init (False)
}
