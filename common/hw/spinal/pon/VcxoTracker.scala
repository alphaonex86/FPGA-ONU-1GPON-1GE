// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.lib._

/** Steers the VCXO so the local clock (default domain: the ONU TX clock) follows the recovered downstream clock
  * (rxDomain), as G.984 wants the upstream locked to the downstream; the GTS RX phase compensation FIFO takes the
  * bounded phase error. Phase detector: an rx clock counter seen across in Gray code minus the local count,
  * unwrapped every 2^updateShift local clocks (frequency error below 127 / 2^updateShift). PI loop, error in clocks:
  * duty = center - (kp * error >> 8) - (sum(ki * error) >> 16), a higher duty raising the VCXO frequency; `update`
  * pulses once the new duty is out. Off: duty = center and the error restarts from zero. */
case class VcxoTracker(rxDomain: ClockDomain, updateShift: Int = 18) extends Component {
  val io = new Bundle {
    val enable = in Bool ()
    val center = in UInt (16 bits)
    val kp, ki = in UInt (16 bits)
    val phase = out SInt (16 bits) // error, clocks, saturated
    val duty = out UInt (16 bits)
    val update = out Bool ()
  }
  val rx = new ClockingArea(rxDomain) {
    val count = Reg(UInt(8 bits)) init (0)
    count := count + 1
    val gray = RegNext(toGray(count)) init (0)
  }
  val seen = fromGray(BufferCC(rx.gray, B(0, 8 bits)))
  val count = Reg(UInt(8 bits)) init (0)
  count := count + 1
  val offset = RegNext((count - seen).asSInt) // wraps as the clocks slip

  val timer = Reg(UInt(updateShift bits)) init (0)
  timer := timer + 1
  val tick = timer.andR
  val lastOffset = Reg(SInt(8 bits)) init (0)
  val error = Reg(SInt(24 bits)) init (0) // unwrapped
  val sum = Reg(SInt(48 bits)) init (0)
  // products and clamp pipelined: the error only changes once per tick
  val proportional = RegNext(RegNext(error * io.kp.intoSInt) >> 8)
  val integral = RegNext(RegNext(RegNext(error * io.ki.intoSInt)))
  when(tick) {
    lastOffset := offset
    error := io.enable ? (error + (offset - lastOffset).resize(24)) | S(0, 24 bits)
    sum := io.enable ? (sum + integral.resize(48)) | S(0, 48 bits)
  }
  val control = RegNext(proportional.resize(34) + (sum >> 16).resize(34))
  val target = RegNext(io.center.intoSInt.resize(34) - control)
  val duty = RegNext((target < 0) ? U(0, 16 bits) | ((target > 0xFFFF) ? U(0xFFFF, 16 bits) | target.asUInt.resize(16))) init (0)
  io.duty := duty
  io.phase := error.sat(8)
  io.update := Delay(tick, 9, init = False) // past the pipeline
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
