# Datapath acceleration: where we are, what the professional ONU chips do, what to add

Compared at the architecture level with three shipping designs (studied as references, nothing reused): the RTL9602C L34
NAT engine, the RTL9607C / RTL9603CVD flow engine ("luna", the most complete Realtek one) and the Cortina L3FE of the
RTL8277C. Goals (operator 2026-11-07): every acceleration layer can be disabled, everything not accelerated goes to
the CPU, the board gets a real 1000BASE-T port like the ASIC.

## What the three have in common

| Piece | RTL9602C | luna (9607C/9603CVD) | Cortina L3FE | Ours today |
|---|---|---|---|---|
| Flow table | NAPT 4-way + NAPTR, 1K | 2 hash indices, 1K SRAM or 8K-32K in DRAM | 64K in DRAM, 8-way buckets, on-chip cache, key stored as a CRC32 signature | 2 x 1024 IPv4 + 2 x 256 IPv6, M20K |
| Egress interface table | NETIF 16 (MAC, VLAN, MTU) + NEXTHOP + ARP | NETIF (MAC, MTU check, PPPoE keep/add/replace/remove, port masks, deny v4/v6) | L3 interface (PPPoE, IPoE), MAC DA index | 64 egress entries |
| Flow action | NAT IP/port | route/SNAT/DNAT, C-tag + S-tag with priority, TOS, stream (queue), MIB counter | permit, queue, multicast group, new IP/port, DSCP, VLANs, PPPoE, IPv6 NAPT | NAPT, PPPoE add/strip, VLAN, IPv6 hop limit |
| Miss / trap | to CPU | trap reasons with a filter | default (miss) action table, special-packet table (own MAC, ARP, DHCP, ICMP) with CoS and no-drop | miss -> CPU queue; control frames -> rate-limited queue |
| Counters, aging | hit bitmap | 32 flow MIBs | 2-bit age (HW re-arm, SW sweep), 224 flow MIBs | hit bit |
| Policing | - | meters | 32 policer bands, per-GEM policer | - |
| L2 | LUT 1K + CAM, learning | LUT 2K + 64 CAM, own MACs static | station-move control | none (2-port bridge) |
| VLAN rules (ME 171) | classifier 128 + 128 rules, US/DS actions | classifier (9607C), none on 9603CVD | VLAN install per flow | 15 treatments per direction |
| Debug | - | header snapshot of the last frame | hash compute port for software | - |

Every vendor driver also gates each engine behind a switch (HW NAT, HW PPPoE) and falls back to software.

## Proposed additions, in order

1. **Bypass per layer** (goal): one register bit each for flow lookup, NAT rewrite, PPPoE rewrite, VLAN rewrite,
   checksum update, L2 bridge; a disabled layer sends its frames to the CPU (or passes them unchanged where that is
   still correct). A global "everything to the CPU" mode. Cheap, needed before anything else.
2. **Trap table instead of fixed control detection** (goal): per reason (own MAC, ARP, DHCP, ICMP, IGMP/MLD, PPPoE
   discovery and LCP, TTL expired, MTU exceeded, fragment, TCP SYN/FIN/RST, miss, OMCI) an action {to CPU, drop,
   forward}, a CPU queue and a rate. Generalises today's control queue and token bucket.
3. **Debug snapshot**: registers holding the last parsed key, the hash indices, hit/miss and the action taken, per
   direction; software computes the same hash from it. Cheap, essential for ASIC bring-up.
4. **Flow counters and age**: a counter index per flow (packets, bytes), 2-bit age with hardware re-arm on hit,
   software sweep; feeds Linux flowtable stats and conntrack timeouts.
5. **Egress interface table**: MTU check (bigger -> CPU), PPPoE replace, S-tag + C-tag with PCP, DSCP remark,
   stream (T-CONT queue) per flow.
6. **VLAN classifier for ME 171**: a rule table (match on tags, PCP, EtherType, ingress port or GEM; action push /
   pop / translate on two tags, priority, queue) replacing the 15 treatments; ~128 rules per direction.
7. **Policers**: token buckets per GEM port and per flow class (ME 280 traffic descriptors).
8. **Queues**: several priority queues per T-CONT, strict priority then WRR.
9. **L2 learning**: MAC table with aging, static entries for our own MACs, station-move control; needed once the
   board has more than one LAN port.
10. **Multicast**: group id -> replication list, IGMP/MLD snooping in software filling it.
11. **Flow table in DRAM with an on-chip cache** (Cortina, luna): 64K flows without M20K, which is the scarce
    resource here (86-91 % used). Larger change: HPS DDR on the FPGA, external DRAM on the ASIC.

Writes follow the vendors' ordering so the engine never reads a half-written flow: action first, key next, valid
last; on delete, valid first.
