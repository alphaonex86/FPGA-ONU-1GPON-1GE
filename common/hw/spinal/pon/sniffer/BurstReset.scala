// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.lib._
import pon.gpon.Allocation

/** Burst reset of the tap's OLT module: a pulse before each burst of the tapped ONU, planned from its BWmap entries
  * (311.04 MHz clocks). A burst starts at a kept entry not contiguous with the frame's previous one (StartTime =
  * previous StopTime + 1 continues it, as GtcUsFramer); its StartTime reaches the tap `offset` + 2 StartTime clocks
  * after the entry (its record's time: the CPU learns offset from the records), the pulse starts `lead` clocks before
  * and lasts `width` (0: none). Plans wait in a queue (the ONU's EqD spans frames); one already past or with the
  * queue full is lost (`lost`). */
case class BurstReset(depth: Int = 16) extends Component {
  val io = new Bundle {
    val frameStart = in Bool ()
    val alloc = slave Flow (Allocation()) // the kept entries, good CRC
    val flush = in Bool ()
    val time = in UInt (32 bits)
    val offset = in UInt (32 bits)
    val lead, width = in UInt (16 bits)
    val reset = out Bool ()
    val lost = out UInt (16 bits)
  }
  val previousStop = Reg(UInt(16 bits))
  val inFrame = RegInit(False) // an entry of this frame seen
  when(io.frameStart)(inFrame := False)
  when(io.alloc.valid) { previousStop := io.alloc.stop; inFrame := True }
  val burst = io.alloc.valid && !(inFrame && io.alloc.start === previousStop + 1)

  // due = time + offset + 2 start - lead - 3 (the pulse's latency), in registered steps (311.04 MHz)
  val base = RegNext(io.time + io.offset)
  val shift = RegNext((io.alloc.start << 1).resize(32) - io.lead - 3)
  val due = RegNext(base + shift)
  val planned = RegNext(RegNext(burst) init (False)) init (False)
  val queue = StreamFifo(UInt(32 bits), depth)
  queue.io.flush := io.flush
  val early = !(due - io.time).msb
  queue.io.push.valid := planned && early
  queue.io.push.payload := due
  val lost = Reg(UInt(16 bits)) init (0)
  when(planned && (!early || !queue.io.push.ready))(lost := lost + 1)
  io.lost := lost

  // the head's time compared registered: a pop waits a clock for the next head's compare
  val head = queue.io.pop
  val reached = RegNext(head.valid && !(io.time - head.payload).msb) init (False)
  val popped = RegNext(head.fire) init (False)
  head.ready := reached && !popped
  val left = Reg(UInt(16 bits)) init (0)
  when(left =/= 0)(left := left - 1)
  when(head.fire)(left := io.width)
  io.reset := RegNext(left =/= 0) init (False)
}
