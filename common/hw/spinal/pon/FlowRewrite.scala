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
  val ipv6 = Bool() // routed only: the hop limit decremented, no NAT
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

/** Rewrites an offloaded frame on its way out: MACs, PPPoE session header added or stripped, IPv4: NAPT of the
  * source or destination address and port, TTL - 1, IP and TCP/UDP checksums; IPv6: hop limit - 1; the job read at
  * the frame's first byte.
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

  /** The input byte's index from the header starting at `offset`, counted from the byte before (311.04 MHz). */
  def index(offset: UInt): (Bool, UInt) = {
    val inside = RegInit(False)
    val at = Reg(UInt(6 bits))
    when(io.input.fire) {
      when(at =/= 63)(at := at + 1)
      when(position + 1 === offset.resize(7)) { inside := True; at := 0 }
      when(io.input.last) { inside := False }
    }
    (inside, at)
  }
  val (inL2, l2) = index(job.l2End)
  val (inL3, l3) = index(job.l3Offset)
  def be(v: Bits, k: UInt): Bits = v.subdivideIn(8 bits).reverse.read(k.resized) // byte k of a big-endian field
  val b = Bits(8 bits)
  b := io.input.fragment
  when(job.enable) {
    when(position < 6)(b := be(job.da, position))
    when(position >= 6 && position < 12)(b := be(job.sa, position - 6))
    when(inL2 && job.strip && l2 === 8)(b := job.ipv6 ? B"8'x86" | B"8'x08")
    when(inL2 && job.strip && l2 === 9)(b := job.ipv6 ? B"8'xDD" | B"8'x00")
    when(inL2 && job.add && l2 === 0)(b := 0x00)
    when(inL2 && job.add && l2 === 1)(b := job.ipv6 ? B"8'x57" | B"8'x21")
    when(inL3 && l3 === (job.ipv6 ? U(7) | U(8)))(b := (io.input.fragment.asUInt - 1).asBits)
    when(inL3 && !job.ipv6) {
      when(l3 === 10 || l3 === 11)(b := be(job.ipChecksum, l3 - 10))
      when(l3 >= (job.natSrc ? U(12) | U(16)) && l3 < (job.natSrc ? U(16) | U(20)))(b := be(job.ip, l3 - (job.natSrc ? U(12) | U(16))))
      when(l3 >= (job.natSrc ? U(20) | U(22)) && l3 < (job.natSrc ? U(22) | U(24)))(b := be(job.port.asBits, l3 - (job.natSrc ? U(20) | U(22))))
      when(l3 === (job.udp ? U(26) | U(36)) || l3 === (job.udp ? U(27) | U(37)))(b := be(job.l4Checksum, l3 - (job.udp ? U(26) | U(36))))
    }
  }
  val pppoe = Vec(B"8'x88", B"8'x64", B"8'x11", B"8'x00", job.session.asBits(15 downto 8), job.session.asBits(7 downto 0),
    job.pppLength.asBits(15 downto 8), job.pppLength.asBits(7 downto 0))

  val out = Stream(Fragment(Bits(8 bits)))
  io.output << out.m2sPipe() // registered output (311.04 MHz)
  out.valid := io.input.valid
  out.fragment := b
  out.last := io.input.last
  io.input.ready := out.ready
  val atL2 = job.enable && inL2 && l2 === 0
  switch(state) {
    is(COPY) {
      when(io.input.valid && atL2 && job.add && !inserted) {
        state := INSERT // before the EtherType
        out.valid := True; out.fragment := pppoe(0); out.last := False; io.input.ready := False
        when(out.ready)(count := 1) otherwise (state := COPY)
      }
      when(io.input.valid && atL2 && job.strip) {
        state := DROP
        out.valid := False; io.input.ready := True
        count := 1
      }
    }
    is(INSERT) {
      out.valid := True; out.fragment := pppoe(count); out.last := False; io.input.ready := False
      when(out.ready) { count := count + 1; when(count === 7) { state := COPY; inserted := True } }
    }
    is(DROP) {
      out.valid := False; io.input.ready := True
      when(io.input.valid) { count := count + 1; when(count === 7)(state := COPY) }
    }
  }
  when(io.input.fire) {
    when(position =/= 127)(position := position + 1)
    when(io.input.last) { position := 0; state := COPY; inserted := False }
  }
}
