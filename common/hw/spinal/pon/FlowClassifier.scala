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
  val control = Bool() // a control protocol frame (FrameHeader.control)
  val job = RewriteJob() // enabled on a hit
  val vlan = VlanTreatment()
  val portId = UInt(12 bits)
  val queue = UInt(2 bits)
}

object FlowClassifier { val latency = 12 }

/** The flow engine of one direction at its input: parses each frame, looks its flow up at its end (offloadable,
  * TTL > 1; IPv4 and IPv6 tables, IPv6 routed only), reads the flow's egress (64 entries) and builds its rewrite job
  * with the updated checksums; the decision
  * comes FlowClassifier.latency clocks after the frame's last byte, so a frame may follow right after another.
  * A hit sets its entry's hit bit; a hit query (IPv6 ## bucket ## way) answers the bit two clocks later and clears it
  * (a hit at that clock wins, so the bit may stay set). */
case class FlowClassifier(bucketBits: Int, bucketBits6: Int) extends Component {
  assert(bucketBits6 <= bucketBits)
  val io = new Bundle {
    val input = slave Flow (Fragment(Bits(8 bits)))
    val decision = master Flow (FlowDecision())
    val flowWrite = slave Flow (FlowWrite(bucketBits))
    val flowWrite6 = slave Flow (FlowWrite(bucketBits6, 128))
    val egressWrite = slave Flow (EgressWrite())
    val hitQuery = slave Flow (UInt(bucketBits + 2 bits))
    val hitAnswer = master Flow (Bool())
  }
  val parser = FrameParser()
  parser.io.input << io.input
  val table = FlowTable(bucketBits)
  table.io.write << io.flowWrite
  val table6 = FlowTable(bucketBits6, 128)
  table6.io.write << io.flowWrite6

  val ended = RegNext(io.input.valid && io.input.last) init (False) // the parser's header complete
  val p = parser.io.header
  val h = RegNextWhen(p, ended)
  val tagged = p.vlans =/= 0
  for ((t, ip) <- Seq(table -> p.ipv4, table6 -> p.ipv6)) {
    t.io.lookup.valid := ended && p.offloadable && ip && p.ttl > 1
    val key = t.io.lookup.payload
    key.dport := p.dport; key.sport := p.sport; key.dst := p.dst.resized; key.src := p.src.resized
    key.udp := p.proto === 17; key.session := p.pppoe ? p.session | U(0); key.pppoe := p.pppoe
    key.vid := tagged ? p.outerVid | U(0); key.tagged := tagged
  }

  val looked = Delay(ended, 6, init = False) // with the table's result
  val egresses = Mem(FlowEgress(), 64) addTag (ReadDuringWriteFree)
  // a CPU write waits out the clock of a read (frames are >= 64 clocks apart)
  val egressWrite = RegNextWhen(io.egressWrite.payload, io.egressWrite.valid)
  val egressPending = RegInit(False)
  when(io.egressWrite.valid)(egressPending := True) elsewhen (!looked)(egressPending := False)
  egresses.write(egressWrite.index, egressWrite.egress, egressPending && !looked)
  val (result, result6) = (table.io.result, table6.io.result)
  val found = result.valid && result.hit || result6.valid && result6.hit
  val hit = RegNextWhen(found, looked)
  val foundAction = result6.valid ? result6.action | result.action
  val action = RegNextWhen(foundAction, looked)
  val e = RegNext(egresses.readSync(foundAction.egress, looked)) // registered RAM output
  val query = RegNext(io.hitQuery) init (io.hitQuery.getZero)
  val answers = for ((r, bits, v6) <- Seq((result, bucketBits, false), (result6, bucketBits6, true))) yield {
    val hits = Mem(Bool(), 2 << bits) initBigInt (Seq.fill(2 << bits)(BigInt(0)))
    val setHit = looked && r.valid && r.hit
    val mine = query.valid && query.payload.msb === Bool(v6)
    hits.write(setHit ? r.index | query.payload.resize(bits + 1), setHit, setHit || mine)
    hits.readSync(io.hitQuery.payload.resize(bits + 1), io.hitQuery.valid)
  }
  io.hitAnswer.valid := RegNext(query.valid) init (False)
  io.hitAnswer.payload := RegNext(query.payload.msb ? answers(1) | answers(0))
  // the job from registers held to the decision; the checksums 3 clocks after e
  val udp = h.proto === 17
  val oldIp = action.natSrc ? h.src(31 downto 0) | h.dst(31 downto 0)
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
  d.control := h.control
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
  j.ipv6 := h.ipv6
  j.l4Checksum := noUdpChecksum ? B(0, 16 bits) | ((udp && l4Updated === 0) ? B"16'xFFFF" | l4Updated)
}
