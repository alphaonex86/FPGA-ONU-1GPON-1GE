// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.lib._

/** How an offloaded flow's frame is rewritten; positions are of the input frame (the parser's). */
case class RewriteJob() extends Bundle {
  val enable = Bool() // else the frame passes unchanged
  val da, sa = Bits(48 bits)
  val l2End = UInt(5 bits) // the EtherType after the VLAN tags
  val strip, add = Bool() // the PPPoE session header
  val session, pppLength = UInt(16 bits)
  val l3Offset = UInt(6 bits)
  val natSrc = Bool()
  val ip = Bits(32 bits)
  val port = UInt(16 bits)
  val udp = Bool()
  val ipChecksum, l4Checksum = Bits(16 bits) // the updated ones
}

object Checksum {
  def add(a: Bits, b: Bits): Bits = { val s = a.asUInt +^ b.asUInt; (s(15 downto 0) + s(16).asUInt).asBits }
  def addModel(a: Int, b: Int): Int = { val s = a + b; (s & 0xFFFF) + (s >> 16) }

  /** RFC 1624 eqn 3: the checksum after the 16-bit words olds became news; a registered adder tree, ready
    * log2(terms) clocks after its inputs. */
  def update(checksum: Bits, olds: Seq[Bits], news: Seq[Bits]): Bits = {
    var terms = (~checksum) +: (olds.map(~_) ++ news)
    while (terms.length > 1) terms = terms.grouped(2).map(p => RegNext(if (p.length == 2) add(p(0), p(1)) else p(0))).toSeq
    ~terms.head
  }
  def updateModel(checksum: Int, olds: Seq[Int], news: Seq[Int]): Int =
    ~((~checksum & 0xFFFF) +: (olds.map(~_ & 0xFFFF) ++ news)).reduce(addModel) & 0xFFFF
}

object FlowRewriteState extends SpinalEnum { val COPY, INSERT, DROP = newElement() }

/** Rewrites an offloaded IPv4 frame on its way out: MACs, PPPoE session header added or stripped, NAPT of the source
  * or destination address and port, TTL - 1, IP and TCP/UDP checksums; the job read at the frame's first byte.
  * A stripped header leaves an 8-byte hole: VlanRewrite(reserve = 12) after it keeps the output frame continuous. */
case class FlowRewrite() extends Component {
  val io = new Bundle {
    val input = slave Stream (Fragment(Bits(8 bits)))
    val job = in(RewriteJob())
    val output = master Stream (Fragment(Bits(8 bits)))
  }
  import FlowRewriteState._
  val state = RegInit(COPY)
  val position = Reg(UInt(7 bits)) init (0) // of the input byte, up to 127
  val count = Reg(UInt(3 bits)) init (0) // inserted or dropped bytes
  val inserted = RegInit(False)
  val t = Reg(RewriteJob())
  val job = (position === 0) ? io.job | t
  when(io.input.fire && position === 0)(t := io.job)

  val l3 = position - job.l3Offset.resize(7)
  val l2 = position - job.l2End.resize(7)
  def be(v: Bits, k: UInt): Bits = v.subdivideIn(8 bits).reverse.read(k.resized) // byte k of a big-endian field
  val b = Bits(8 bits)
  b := io.input.fragment
  when(job.enable) {
    when(position < 6)(b := be(job.da, position))
    when(position >= 6 && position < 12)(b := be(job.sa, position - 6))
    when(job.strip && l2 === 8)(b := 0x08)
    when(job.strip && l2 === 9)(b := 0x00)
    when(job.add && l2 === 0)(b := 0x00)
    when(job.add && l2 === 1)(b := 0x21)
    when(position >= job.l3Offset.resize(7)) {
      when(l3 === 8)(b := (io.input.fragment.asUInt - 1).asBits)
      when(l3 === 10 || l3 === 11)(b := be(job.ipChecksum, l3 - 10))
      when(l3 >= (job.natSrc ? U(12) | U(16)) && l3 < (job.natSrc ? U(16) | U(20)))(b := be(job.ip, l3 - (job.natSrc ? U(12) | U(16))))
      when(l3 >= (job.natSrc ? U(20) | U(22)) && l3 < (job.natSrc ? U(22) | U(24)))(b := be(job.port.asBits, l3 - (job.natSrc ? U(20) | U(22))))
      when(l3 === (job.udp ? U(26) | U(36)) || l3 === (job.udp ? U(27) | U(37)))(b := be(job.l4Checksum, l3 - (job.udp ? U(26) | U(36))))
    }
  }
  val pppoe = Vec(B"8'x88", B"8'x64", B"8'x11", B"8'x00", job.session.asBits(15 downto 8), job.session.asBits(7 downto 0),
    job.pppLength.asBits(15 downto 8), job.pppLength.asBits(7 downto 0))

  io.output.valid := io.input.valid
  io.output.fragment := b
  io.output.last := io.input.last
  io.input.ready := io.output.ready
  val atL2 = job.enable && l2 === 0
  switch(state) {
    is(COPY) {
      when(io.input.valid && atL2 && job.add && !inserted) {
        state := INSERT // before the EtherType
        io.output.valid := True; io.output.fragment := pppoe(0); io.output.last := False; io.input.ready := False
        when(io.output.ready)(count := 1) otherwise (state := COPY)
      }
      when(io.input.valid && atL2 && job.strip) {
        state := DROP
        io.output.valid := False; io.input.ready := True
        count := 1
      }
    }
    is(INSERT) {
      io.output.valid := True; io.output.fragment := pppoe(count); io.output.last := False; io.input.ready := False
      when(io.output.ready) { count := count + 1; when(count === 7) { state := COPY; inserted := True } }
    }
    is(DROP) {
      io.output.valid := False; io.input.ready := True
      when(io.input.valid) { count := count + 1; when(count === 7)(state := COPY) }
    }
  }
  when(io.input.fire) {
    when(position =/= 127)(position := position + 1)
    when(io.input.last) { position := 0; state := COPY; inserted := False }
  }
}
