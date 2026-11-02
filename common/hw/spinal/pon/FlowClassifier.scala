// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.lib._

/** How a flow leaves: next hop and own MAC, PPPoE session (added when the frame has none, else kept; none: stripped),
  * outer VLAN treatment (remove, add, pcpCopy, sTag, TCI from bit 0), toward the PON its Port-ID and queue. */
case class FlowEgress() extends Bundle {
  val da, sa = Bits(48 bits)
  val pppoe = Bool()
  val session = UInt(16 bits)
  val vlan = Bits(20 bits)
  val portId = UInt(12 bits)
  val queue = UInt(2 bits)
}

case class EgressWrite() extends Bundle {
  val index = UInt(6 bits)
  val egress = FlowEgress()
}

case class FlowDecision() extends Bundle {
  val hit = Bool()
  val job = RewriteJob() // enabled on a hit
  val vlan = VlanTreatment()
  val portId = UInt(12 bits)
  val queue = UInt(2 bits)
}

object FlowClassifier { val latency = 11 }

/** The flow engine of one direction at its input: parses each frame, looks its flow up at its end (offloadable,
  * TTL > 1), reads the flow's egress (64 entries) and builds its rewrite job with the updated checksums; the decision
  * comes FlowClassifier.latency clocks after the frame's last byte, so a frame may follow right after another.
  * A hit sets its entry's hit bit; a hit query (bucket ## way) answers the bit two clocks later and clears it (a hit
  * at that clock wins, so the bit may stay set). */
case class FlowClassifier(bucketBits: Int) extends Component {
  val io = new Bundle {
    val input = slave Flow (Fragment(Bits(8 bits)))
    val decision = master Flow (FlowDecision())
    val flowWrite = slave Flow (FlowWrite(bucketBits))
    val egressWrite = slave Flow (EgressWrite())
    val hitQuery = slave Flow (UInt(bucketBits + 1 bits))
    val hitAnswer = master Flow (Bool())
  }
  val parser = FrameParser()
  parser.io.input << io.input
  val table = FlowTable(bucketBits)
  table.io.write << io.flowWrite
  val egresses = Mem(FlowEgress(), 64)
  egresses.write(io.egressWrite.index, io.egressWrite.egress, io.egressWrite.valid)

  val ended = RegNext(io.input.valid && io.input.last) init (False) // the parser's header complete
  val p = parser.io.header
  val h = RegNextWhen(p, ended)
  val tagged = p.vlans =/= 0
  table.io.lookup.valid := ended && p.offloadable && p.ttl > 1
  val key = table.io.lookup.payload
  key.dport := p.dport; key.sport := p.sport; key.dst := p.dst; key.src := p.src; key.udp := p.proto === 17
  key.session := p.pppoe ? p.session | U(0); key.pppoe := p.pppoe
  key.vid := tagged ? p.outerVid | U(0); key.tagged := tagged

  val looked = Delay(ended, 5, init = False) // with the table's result
  val result = table.io.result
  val hit = RegNextWhen(result.valid && result.hit, looked)
  val action = RegNextWhen(result.action, looked)
  val e = RegNext(egresses.readSync(result.action.egress, looked)) // registered RAM output
  val hits = Mem(Bool(), 2 << bucketBits) initBigInt (Seq.fill(2 << bucketBits)(BigInt(0)))
  val setHit = looked && result.valid && result.hit
  val query = RegNext(io.hitQuery) init (io.hitQuery.getZero)
  io.hitAnswer.valid := RegNext(query.valid) init (False)
  io.hitAnswer.payload := RegNext(hits.readSync(io.hitQuery.payload, io.hitQuery.valid))
  hits.write(setHit ? result.index | query.payload, setHit, setHit || query.valid)
  // the job from registers held to the decision; the checksums 3 clocks after e
  val udp = h.proto === 17
  val oldIp = action.natSrc ? h.src | h.dst
  val oldPort = (action.natSrc ? h.sport | h.dport).asBits
  def halves(v: Bits) = Seq(v(31 downto 16), v(15 downto 0))
  val ttlProto = h.ttl.asBits ## h.proto.asBits
  val ipChecksum = Checksum.update(h.ipChecksum, ttlProto +: halves(oldIp),
    ((h.ttl - 1).asBits ## h.proto.asBits) +: halves(action.ip))
  val l4Updated = Checksum.update(h.l4Checksum, halves(oldIp) :+ oldPort, halves(action.ip) :+ action.port.asBits)
  val noUdpChecksum = udp && h.l4Checksum === 0

  io.decision.valid := Delay(ended, FlowClassifier.latency - 1, init = False)
  val d = io.decision.payload
  d.hit := hit
  d.vlan := VlanTreatment.resolve(e.vlan, h.vlans =/= 0, h.outerPcp)
  d.portId := e.portId
  d.queue := e.queue
  val j = d.job
  j.enable := hit
  j.da := e.da
  j.sa := e.sa
  j.l2End := (U(12, 5 bits) + (h.vlans << 2)).resized
  j.strip := h.pppoe && !e.pppoe
  j.add := !h.pppoe && e.pppoe
  j.session := e.session
  j.pppLength := h.totalLength + 2
  j.l3Offset := h.l3Offset
  j.natSrc := action.natSrc
  j.ip := action.ip
  j.port := action.port
  j.udp := udp
  j.ipChecksum := ipChecksum
  j.l4Checksum := noUdpChecksum ? B(0, 16 bits) | ((udp && l4Updated === 0) ? B"16'xFFFF" | l4Updated)
}
