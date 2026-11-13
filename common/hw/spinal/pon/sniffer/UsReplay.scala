// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.lib._

/** The upstream line of a fabric too slow for one byte per clock, for the byte-wide SnifferUs: 16 line bits per clock
  * in (first in bit 15), one byte per clock out, half the line rate. The line is zero between bursts: a run (words
  * from a non-zero one to `tail` zero words after the last) is queued, then replayed after `gap` zero bytes, so a run
  * cut short still ends as a burst; zeros go out while nothing waits. `lineTime` is the line time of the byte going
  * out (`time` when its run began, 2 per byte: units of a downstream byte). A run with no room at its start is left
  * out, one that fills the queue loses its end: both counted. */
case class UsReplay(depth: Int = 4096, tail: Int = 5, gap: Int = 10) extends Component {
  val io = new Bundle {
    val word = in Bits (16 bits)
    val time = in UInt (64 bits)
    val line = out Bits (8 bits)
    val lineTime = out UInt (64 bits)
    val overflows = out UInt (32 bits)
  }
  case class Entry() extends Bundle {
    val start = Bool()
    val bits = Bits(16 bits)
  }
  val words = StreamFifo(Entry(), depth)
  val times = StreamFifo(UInt(64 bits), 16)

  val since = Reg(UInt(log2Up(tail + 1) bits)) init (tail) // zero words since the last non-zero one
  val some = io.word =/= 0
  val inRun = some || since =/= tail
  when(some)(since := 0) elsewhen (since =/= tail)(since := since + 1)
  val start = inRun && !(RegNext(inRun) init (False))
  val keeping = Reg(Bool()) init (False)
  val room = words.io.availability > 16 && times.io.push.ready
  val keep = start ? room | (keeping && words.io.push.ready)
  val overflows = Reg(UInt(32 bits)) init (0)
  when(inRun) {
    keeping := keep
    when(start ? !room | (keeping && !keep))(overflows := overflows + 1)
  }
  io.overflows := overflows
  words.io.push.valid := inRun && keep
  words.io.push.start := start
  words.io.push.bits := io.word
  times.io.push.valid := start && room
  times.io.push.payload := io.time

  val head = words.io.pop
  val waited = Reg(UInt(log2Up(gap + 1) bits)) init (0) // zero bytes sent before the run at the head
  val second = Reg(Bool()) init (False)
  val base = Reg(UInt(64 bits)) init (0)
  val sent = Reg(UInt(32 bits)) init (0) // bytes of the run
  val hold = head.start && !second && waited =/= gap
  val line = B(0, 8 bits)
  head.ready := False
  times.io.pop.ready := False
  io.line := RegNext(line) init (0) // registered, with its time: the queue's memory is slow to read
  io.lineTime := RegNext(base + (sent << 1).resized)
  when(head.valid) {
    when(hold) {
      waited := waited + 1
      when(waited === gap - 1) {
        base := times.io.pop.payload
        times.io.pop.ready := True
        sent := 0
      }
    } otherwise {
      line := second ? head.bits(7 downto 0) | head.bits(15 downto 8)
      second := !second
      sent := sent + 1
      when(second) {
        head.ready := True
        waited := 0
      }
    }
  }
}
