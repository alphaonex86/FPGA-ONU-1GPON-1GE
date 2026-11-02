// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.lib._

/** What to do with a frame's outer VLAN tag (bytes 12-15): remove it, add one, both (translate) or neither. */
case class VlanTreatment() extends Bundle {
  val remove = Bool() // only for a tagged frame
  val add = Bool()
  val tag = Bits(32 bits) // TPID ## TCI
}

object VlanTreatment {
  /** A treatment entry (remove, add, pcpCopy, sTag, TCI from bit 0) for a frame: its tag (if tagged) and PCP. */
  def resolve(entry: Bits, tagged: Bool, pcp: Bits): VlanTreatment = {
    val t = VlanTreatment()
    t.remove := entry(0) && tagged
    t.add := entry(1)
    t.tag := (entry(3) ? B"16'x88A8" | B"16'x8100") ## (entry(2) ? pcp | entry(19 downto 17)) ## entry(16 downto 4)
    t
  }
}

object VlanRewriteState extends SpinalEnum { val HEAD, TAG, BODY, FCS = newElement() }

/** Rewrites the outer VLAN tag of each frame (push, pop, translate), the treatment read at its first byte; frames of
  * at least 17 bytes (a removed tag must not hold the last byte). appendFcs: the Ethernet FCS of the rewritten frame
  * follows it. A frame leaves once reserve of its bytes and the buffer latency are buffered: an input with holes of at
  * most reserve - 4 bytes in all gives a continuous output frame (the 4-byte hole of a removed tag absorbed too; the
  * GEM framers and the Ethernet MAC need it). */
case class VlanRewrite(appendFcs: Boolean, reserve: Int = 4) extends Component {
  val io = new Bundle {
    val input = slave Stream (Fragment(Bits(8 bits)))
    val treatment = in(VlanTreatment()) // held from the frame's first byte
    val output = master Stream (Fragment(Bits(8 bits)))
    val idle = out Bool () // between frames, the output buffer empty
  }
  val core = Stream(Fragment(Bits(8 bits)))
  import VlanRewriteState._
  val state = RegInit(HEAD)
  val index = Reg(UInt(4 bits)) init (0) // byte within the head or the tag
  val t = Reg(VlanTreatment())
  val crc = Reg(Bits(32 bits))

  core.valid := io.input.valid
  core.fragment := io.input.fragment
  core.last := io.input.last && !Bool(appendFcs)
  io.input.ready := core.ready
  val tagByte = Vec(t.tag.subdivideIn(8 bits).reverse)(index(1 downto 0))
  switch(state) {
    is(HEAD) {
      when(io.input.fire) {
        index := index + 1
        when(index === 0)(t := io.treatment)
        when(index === 11) { index := 0; state := (t.add || t.remove) ? TAG | BODY }
      }
    }
    is(TAG) {
      when(t.add) { // the new tag, in place of the old one if removed
        core.valid := !t.remove || io.input.valid
        core.fragment := tagByte
        core.last := False
        io.input.ready := t.remove && core.ready
      } otherwise { // the old one dropped
        core.valid := False
        io.input.ready := True
      }
      when(t.add ? core.fire | io.input.fire) {
        index := index + 1
        when(index === 3) { index := 0; state := BODY }
      }
    }
    is(BODY) {
      when(io.input.fire && io.input.last)(state := (if (appendFcs) FCS else HEAD))
    }
    is(FCS) {
      core.valid := True
      core.fragment := (~crc).subdivideIn(8 bits)(index(1 downto 0))
      core.last := index === 3
      io.input.ready := False
      when(core.fire) {
        index := index + 1
        when(index === 3) { index := 0; state := HEAD }
      }
    }
  }
  val first = state === HEAD && index === 0
  val fifoLatency = 1
  val buffer = StreamFifo(Fragment(Bits(8 bits)), 2 * reserve, latency = fifoLatency)
  buffer.io.push << core
  val lasts = Reg(UInt(4 bits)) init (0) // frame ends in the buffer
  lasts := lasts + (buffer.io.push.fire && core.last).asUInt - (io.output.fire && io.output.last).asUInt
  val sending = RegInit(False)
  io.output << buffer.io.pop.continueWhen(sending || buffer.io.occupancy >= reserve + fifoLatency || lasts =/= 0) // the hole covered
  when(io.output.fire)(sending := !io.output.last)
  io.idle := first && buffer.io.occupancy === 0
  when(core.fire && state =/= FCS)(crc := Crc32.step(first ? B(32 bits, default -> True) | crc, core.fragment))
}
