// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.lib._

/** What the flow engine needs of a frame's headers. */
case class FrameHeader() extends Bundle {
  val da = Bits(48 bits)
  val vlans = UInt(2 bits)
  val outerVid, innerVid = UInt(12 bits)
  val outerPcp = Bits(3 bits)
  val pppoe = Bool()   // PPPoE session data (0x8864, code 0)
  val session = UInt(16 bits)
  val ipv4, ipv6 = Bool()
  val l3Offset = UInt(6 bits) // the IP header's first byte
  val totalLength = UInt(16 bits) // of the IP packet, IPv6: 40 + payload length
  val ttl, proto = UInt(8 bits) // IPv6: hop limit, next header
  val ipChecksum, l4Checksum = Bits(16 bits)
  val src, dst = Bits(128 bits) // IPv4: bits 31-0
  val sport, dport = UInt(16 bits)
  val tcpFlags = Bits(8 bits)
  val offloadable = Bool() // TCP/UDP over IPv4 without options or fragmentation, or over IPv6 without extension
                           // headers; its L4 header read, TCP without SYN/FIN/RST
  val control = Bool() // ARP, PPPoE discovery and non-IP PPP (LCP, IPCP...), EAPOL, LLDP, slow protocols, IGMP,
                       // DHCP, DHCPv6, ICMPv6 and IPv6 hop-by-hop (MLD)
}

object ParserState extends SpinalEnum { val MAC, TYPE, VLAN, PPPOE, IPV4, IPV6, L4, DONE = newElement() }

/** Parses Ethernet -> 0-2 VLAN tags (0x8100 / 0x88A8) -> PPPoE session -> IPv4 / IPv6 -> TCP / UDP ports, one byte
  * per clock; io.header holds the frame's fields from its end until the next frame's first byte. */
case class FrameParser() extends Component {
  val io = new Bundle {
    val input = slave Flow (Fragment(Bits(8 bits)))
    val header = out(FrameHeader())
  }
  import ParserState._
  val h = Reg(FrameHeader())
  io.header := h
  val state = RegInit(MAC)
  val index = Reg(UInt(6 bits)) init (0) // byte within the current header
  val position = Reg(UInt(6 bits)) init (0) // of the current byte in the frame, up to 63
  val prev = Reg(Bits(8 bits))
  val l4Done = Reg(Bool()) // the UDP or TCP header up to its checksum
  val controlL2 = Reg(Bool()) // a control EtherType or PPP protocol
  val ip4Seen = Reg(Bool()) // an IPv4 header, options or fragment included
  val udp = h.proto === 17
  val b = io.input.fragment
  val word = prev ## b // the 16-bit field ending at this byte

  when(io.input.valid) {
    prev := b
    index := index + 1
    when(position =/= 63)(position := position + 1)
    def next(s: ParserState.C): Unit = { state := s; index := 0 }
    switch(state) {
      is(MAC) {
        when(index === 0) { // the frame's first byte: a fresh header
          h.vlans := 0; h.pppoe := False; h.ipv4 := False; h.ipv6 := False; l4Done := False
          controlL2 := False; ip4Seen := False
        }
        when(index < 6)(h.da := h.da(39 downto 0) ## b)
        when(index === 11)(next(TYPE))
      }
      is(TYPE) {
        when(index === 1) {
          next(DONE)
          when((word === 0x8100 || word === 0x88A8) && h.vlans =/= 2)(next(VLAN))
          when(word === 0x8864)(next(PPPOE))
          when(word === 0x0800)(next(IPV4))
          when(word === 0x86DD)(next(IPV6))
          controlL2 := Seq(0x0806, 0x8863, 0x888E, 0x88CC, 0x8809).map(word === _).orR
        }
      }
      is(VLAN) {
        when(index === 1) { // the TCI; its EtherType follows
          when(h.vlans === 0) { h.outerVid := word(11 downto 0).asUInt; h.outerPcp := word(15 downto 13) }
          when(h.vlans =/= 0)(h.innerVid := word(11 downto 0).asUInt)
          h.vlans := h.vlans + 1
          next(TYPE)
        }
      }
      is(PPPOE) { // ver/type, code, session, length, PPP protocol
        when(index === 1 && b =/= 0)(next(DONE))
        when(index === 3)(h.session := word.asUInt)
        when(index === 7) {
          h.pppoe := True
          controlL2 := word =/= 0x0021 && word =/= 0x0057
          next(DONE)
          when(word === 0x0021)(next(IPV4))
          when(word === 0x0057)(next(IPV6))
        }
      }
      is(IPV4) {
        when(index === 0) { // options: parsed for the protocol, not offloaded
          h.l3Offset := position
          h.ipv4 := b === 0x45
          ip4Seen := b(7 downto 4) === 4
          when(b(7 downto 4) =/= 4)(next(DONE))
        }
        when(index === 3)(h.totalLength := word.asUInt)
        when(index === 7 && (word & 0x3FFF) =/= 0)(h.ipv4 := False) // a fragment
        when(index === 11)(h.ipChecksum := word)
        when(index === 8)(h.ttl := b.asUInt)
        when(index === 9)(h.proto := b.asUInt)
        when(index >= 12 && index < 16)(h.src := h.src(119 downto 0) ## b)
        when(index >= 16 && index < 20)(h.dst := h.dst(119 downto 0) ## b)
        when(index === 19)(next((h.ipv4 && (h.proto === 6 || udp)) ? L4 | DONE))
      }
      is(IPV6) {
        when(index === 0) { h.l3Offset := position; h.ipv6 := b(7 downto 4) === 6; when(b(7 downto 4) =/= 6)(next(DONE)) }
        when(index === 5)(h.totalLength := word.asUInt + 40)
        when(index === 6)(h.proto := b.asUInt)
        when(index === 7)(h.ttl := b.asUInt)
        when(index >= 8 && index < 24)(h.src := h.src(119 downto 0) ## b)
        when(index >= 24 && index < 40)(h.dst := h.dst(119 downto 0) ## b)
        when(index === 39)(next((h.proto === 6 || udp) ? L4 | DONE))
      }
      is(L4) {
        when(index === 1)(h.sport := word.asUInt)
        when(index === 3)(h.dport := word.asUInt)
        when(index === 7 && udp) { h.l4Checksum := word; l4Done := True; next(DONE) }
        when(index === 13)(h.tcpFlags := b)
        when(index === 17) { h.l4Checksum := word; l4Done := True; next(DONE) }
      }
      is(DONE) {}
    }
    when(io.input.last) { state := MAC; index := 0; position := 0 }
  }
  h.offloadable.setAsComb() // the verdict from the fields
  h.offloadable := (h.ipv4 || h.ipv6) && l4Done && (udp || (h.tcpFlags & 0x07) === 0)
  h.control.setAsComb()
  val dhcp = l4Done && udp && (ip4Seen ? (h.dport === 67 || h.dport === 68) | (h.dport === 546 || h.dport === 547))
  h.control := controlL2 || ip4Seen && h.proto === 2 || h.ipv6 && (h.proto === 58 || h.proto === 0) || dhcp
}
