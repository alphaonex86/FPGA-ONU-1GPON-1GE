// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.epon

import spinal.core._
import spinal.lib._
import pon.gpon.{GemTxByte, UsGemByte}

/** EPON frames on the GEM streams of GemEthBridge: a frame is one GEM fragment (end-of-frame PTI), its FCS kept both
  * ways; the Port-ID is the LLID's 12 low bits, Port-ID 0xFFF the broadcast LLID (mode 1): LLIDs stay below 0xFFF. */
object EponGem {
  val broadcastPort = 0xFFF

  /** Received frames as GEM fragments (Alloc-ID 0), and the cut of a bad frame (with its last byte). */
  def toGem(frames: Flow[Fragment[EponRxByte]], good: Bool): (Flow[Fragment[UsGemByte]], Bool) = {
    val gem = Flow(Fragment(UsGemByte()))
    gem.valid := frames.valid
    gem.last := frames.last
    gem.allocId := 0
    gem.gem.portId := (frames.llid === Epon.broadcastLlid) ? U(broadcastPort, 12 bits) | frames.llid.resize(12)
    gem.gem.pti := B"001"
    gem.gem.data := frames.data
    (gem, frames.valid && frames.last && !good)
  }

  /** OLT downstream: GEM packets as frames of the LLID of their Port-ID. */
  def toFrames(packets: Stream[Fragment[GemTxByte]]): Stream[Fragment[EponTxByte]] = packets.translateWith {
    val f = Fragment(EponTxByte())
    val broadcast = packets.portId === broadcastPort
    f.data := packets.data
    f.mode := broadcast
    f.llid := broadcast ? U(Epon.broadcastLlid, 15 bits) | packets.portId.resize(15)
    f.fcs := False
    f.last := packets.last
    f
  }

  /** ONU upstream: the bridge's queues, the lowest first, as our frames; the REPORT (TQ, saturated): their backlog in
    * bytes plus 1/8 for the frame overheads (preamble, IPG), plus the exact cost of the frame waiting, so its grant
    * always fits it. */
  def toUpstream(queues: scala.collection.Seq[Stream[Fragment[GemTxByte]]], backlog: Vec[UInt]): (Stream[Fragment[EponUsByte]], UInt) = {
    val packets = StreamArbiterFactory.lowerFirst.fragmentLock.on(queues)
    val frames = packets.translateWith {
      val f = Fragment(EponUsByte())
      f.data := packets.data
      f.length := packets.length.resized
      f.last := packets.last
      f
    }
    val bytes = RegNext(backlog.map(_.resize(19)).reduce(_ + _)) // a clock late: the REPORT is sampled later
    val cost = RegNext(packets.valid ? Mpcp.frameCost(packets.length.resize(11)) | U(0, 16 bits))
    val tq = ((bytes + (bytes >> 3)) >> 1) + cost
    (frames, RegNext((tq > 0xFFFF) ? U(0xFFFF, 16 bits) | tq.resize(16)))
  }
}
