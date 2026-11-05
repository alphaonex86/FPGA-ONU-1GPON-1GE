# FPGA-ONU

An open PON endpoint in one chip: 1 GPON/EPON ONU port (BOSA, to the ISP OLT), 1 SFP
cage (switch SFP, or EPON/GPON OLT module for the OLT role) and 1 Gigabit
Ethernet port, with the MACs, the Ethernet PHY and a RISC-V CPU integrated.
Developed and proven on an FPGA, the final target is an ASIC that holds everything.

## Goals

- GPON ONU MAC: GTC framing, FEC, AES-128 decryption, PLOAM, OMCI, upstream
  burst mode with DBA. EPON MAC (MPCP, OAM) and the OLT role of both.
- 1000BASE-T Ethernet port, PHY integrated in the target ASIC (FPGA phase: a
  copper SFP in an SFP+ cage on our HSMC card for the DE25-Standard; its other
  cage takes an OLT module or a normal SFP).
- SFP cage autodetects the module (EEPROM): 1000BASE-X, EPON OLT or GPON OLT.
- BOSA on board; all optics control (burst, APC, LOS, DDM) in the FPGA/ASIC
  logic; outside only analog: discrete RF transistors, a small ADC/DAC, the APD
  bias boost and passives (FPGA phase: an SY88236L BOSA driver on our HSMC
  card, straight to the DE25-Standard GTS transceivers).
- 32-bit RISC-V CPU (VexiiRiscv, rv32imac, no SIMD, no FPU), high energy
  efficiency, power management (clock gating, sleep states, DVFS); MMU (Sv32)
  so it runs Linux.
- Hardware acceleration (line rate, CPU off the data path): GPON, switching,
  VLAN, PPPoE, IPv4 and IPv6 routing, NAT.
- OpenWrt support: upstream-able riscv32 target, drivers for the GPON MAC and GE port.
- Firmware in an external, socketed, user-programmable flash.
- Behaves on the wire exactly like the stock ONU of each brand the OLT expects
  (ZTE, Huawei, ...): no unknown or unanswered message, no drop out of O5; the
  OLT side likewise mimics the stock OLT.
- ONU and OLT at once, each independent on its own segment; through software,
  the exchanges can be dumped and cloned into per-vendor profiles for everyone.
- Vendor-neutral RTL: vendor primitives only in thin wrappers, so the same tree
  goes from FPGA to ASIC.

## Layout

- `common/`: board-neutral parts: RTL and tests (`hw/`), KiCad generator and
  shared parts library (`pcb/gen`, `pcb/lib`).
- `agilex5/`: common to the Agilex 5 boards (Quartus benches, GTS lane variants).
- `de25-standard/`: Terasic DE25-Standard, our HSMC card (`pcb/hsmc`: two SFP+
  cages, the ONU BOSA and an upstream sniffer receiver; placed and routed, DRC/ERC clean, not yet ordered).
- `de25-nano/`: Terasic DE25-Nano, ECP5 SerDes bridge boards (alternative).

## Status

GPON OLT MAC in simulation (SpinalHDL): GTC downstream framer and deframer,
upstream burst receiver, ONU activation (serial number, ONU-ID, ranging, EqD)
with status-reporting DBA (DBRu), checked against G.984.3 vectors and Scala ONU models; the
OLT MAC top (311.04 / 155.52 MHz domains, CPU registers over APB with a
generated C header) meets timing on the DE25-Standard FPGA.
GPON ONU MAC in simulation: activation (O1-O7, TO1/TO2) with the fixed PLOAM set
in hardware (Acknowledge, Password, Alloc-ID and OMCI Port-ID), every other
PLOAM to the CPU, upstream burst framer timed to the bit from the downstream
frame (35 us response); checked in closed loop against our OLT MAC over a
simulated fibre (ranging, DBA, fibre cut and POPUP, Disable_Serial_Number),
and against the PLOAM sequences of real OLTs (ZTE as recorded by stock ONUs,
Broadcom Maple): every copy answered like the stock ONU, EqD drift updates to
the bit, deactivation churn; meets timing on the DE25-Standard FPGA. Like those
OLTs, our OLT sends each of its PLOAMs 3 times. OMCI over GEM both ways:
downstream fragments of the OMCI Port-ID to the CPU, CPU packets upstream in
the default allocation (OMCC), fragmented to the grant, and data GEM at the
same time in up to 4 data T-CONTs, each stream continuing its packet in its
next allocation. DBRu reports of the real backlog (reporting blocks rounded up,
G.984.3 Table 8-1 code, mode 0 and 1). Downstream AES-128
counter-mode decryption of the encrypted Port-IDs with the key switch at the
OLT's superframe, and downstream FEC: RS(255,239) decoding, up to 8 byte errors
corrected per codeword, with the counters of the OMCI FEC PM; checked against
the G.984.3 golden vectors (A.2.2, A.2.3 with FEC, A.3). The OLT sends
downstream FEC when enabled (CTRL register), every codeword's parity checked
against the model and decoded by our ONU in the closed loop. Upstream FEC in
the ONU when the OLT asks for it (Use_FEC): parity per codeword with the shorter
last one, the content sized to the data bytes, checked by a Scala OLT model; our
OLT does not ask for it (like Maple's default).
Ethernet port in simulation: comma alignment and 8b/10b on the raw 10-bit lane,
taxi's 1000BASE-X/SGMII PCS (autonegotiation) and 1G MAC (`common/hw/ext/taxi`,
submodule: `git submodule update --init`), two ports back to back; on the board
its own lane, SFP cage pins and HPS frame queues (`eth_board_regs.h`), checked
from the CPU registers to the looped-back lane.
L2 bridge GEM <-> Ethernet (`common/hw/spinal/pon/GemEthBridge.scala`,
`bridge_regs.h`): per Port-ID action towards Ethernet (drop, CPU, Ethernet;
whole frames with a good FCS), the outer VLAN ID or the untagged default picks
the Port-ID towards the PON, directly or by PCP (802.1p mapper), and the
Port-ID its T-CONT queue (4 on the ONU, each dropping when full without
blocking the others); VLAN treatments each way (G.988 ME 171 subset: push,
pop, translate, PCP set or kept; `VlanRewrite.scala`), the FCS computed after
the rewrite; store-and-forward queues across the four clocks, drops counted.
Flow offload in the bridge, each way (`FlowClassifier.scala`, mtk_ppe model):
frame parser (0-2 VLAN tags, PPPoE session, IPv4/IPv6 TCP/UDP), flow caches
keyed by the 5-tuple, outer VID and PPPoE session (2-way hash in block RAM,
written by the CPU, a hit bit per flow for the driver's stats: IPv4 2K flows
(1K on the board), IPv6 512, routed only, per direction), 64 egress entries (next hop and own MAC, PPPoE
session, VLAN treatment, Port-ID and queue); a hit is rewritten on its way out
(`FlowRewrite.scala`: MACs, PPPoE add/strip, NAPT, TTL or hop limit,
incremental IP and TCP/UDP checksums), a miss follows the L2 rules or, in flows mode (router: per
Port-ID or VLAN entry), goes to the CPU; on each board side the bridge's LAN and
WAN frame queues carry the misses to the CPU and the CPU's own frames out; control frames (ARP, PPPoE
discovery and LCP, DHCP, ICMPv6, IGMP, EAPOL, LLDP, slow protocols) get their own queues and interrupts
behind a token bucket per side, so a miss flood cannot delay them. On the board the Ethernet port goes to the CPU queues or to the
OLT or the ONU bridge (`eth_board_regs.h` BRIDGE); a frame from the lane reaches
the OLT MAC's downstream GEM, or the ONU MAC's upstream data stream, in
simulation; in the closed OLT <-> ONU loop, Ethernet frames cross both bridges
end to end each way, upstream in two T-CONTs granted by our OLT's DBA.
EPON MAC in simulation (`common/hw/spinal/pon/epon/`): clause 36 PCS (8b/10b,
comma sync at any bit slip) with the clause 65 LLID preamble and its CRC-8;
ONU MPCP (discovery with a random offset in the window, REGISTER_REQ/ACK,
timestamp sync and drift, GATE grants timed to the TQ with laser on/off and
sync time, REPORT, deregistration) and OLT MPCP (discovery windows, LLID
allocation, RTT per ONU, IPACT grants from the REPORTs within min/max, CPU and
timeout deregistration, the optics' receiver reset before each burst), checked
together over a simulated splitter with two
ONUs at different distances: no upstream collision, data per LLID each way.
Both MAC tops (`EponOnu.scala`, `EponOlt.scala`) run from their CPU registers
(`epon_onu_regs.h`, `epon_olt_regs.h`: configuration, registration state and
LLID, OLT events, frame counters, interrupts), checked through them alone;
on the DE25-Standard FPGA at 125 MHz: ONU 1.3K ALMs (211 MHz), OLT 2.5K ALMs
(141 MHz). With the same Ethernet bridges as GPON (`EponGem.scala`: the LLID is
the Port-ID, Port-ID 0xFFF the broadcast LLID; the ONU's REPORT from its queues),
Ethernet frames cross OLT port -> ONU port and back in simulation.
HSMC card GTS lanes (two SFP cages, ONU, burst enable) fit in Quartus at their
line rates from their refclk pins (`agilex5/gts/fit.sh`). One GTS dynamic
reconfiguration controller switches cage 0 and the ONU each on its own at run time
between GPON and 1.25 Gb/s, each profile on its own refclk: the cage 0 lane (GPON
OLT, or EPON OLT, 1000BASE-X or SGMII module) and the ONU BOSA lane with its
burst enable lane (EPON ONU); four combinations (GPON or 1.25 Gb/s per side), so
switching one side leaves the other running (to check on the board). Cage 0 with a normal SFP: the OLT core's own
Ethernet port replaces the EPON OLT MAC on the 1.25 Gb/s profile (`olt_board_regs.h`
ETH, set while the lane is reset; frames to the CPU queues at 0xA00), checked in
simulation through the lane; software picks the mode from the module's EEPROM.
Cage 1 is the Ethernet port's. In the board top the EPON MACs run on those profiles with the same bridges
and CPU queues, their registers on each side's bridge slave (0x800, the DR
controller's at 0x900 on the OLT's); the OLT's received words cross from each
profile's rx clock; the EPON ONU is loop-timed like the GPON one, by its own
VCXO loop on the 156.25 MHz VCXO; checked in simulation in both profiles.
Board top (`agilex5/hw/spinal/pon/PonBoard.scala`), OLT side: OLT MAC on its GTS lane
(2x oversampled upstream), HPS lightweight bridge to the MAC and board registers
(GTS reset/status, SFP cage pins, module I2C; `olt_board_regs.h`, `onu_board_regs.h`) and GEM CPU
queues for OMCI (whole packets downstream, upstream fragments with descriptors;
`gem_queues.h`). ONU side: ONU MAC on the BOSA data and burst enable lanes, its
clock kept on the recovered downstream clock by a VCXO loop (DDMTD phase
detector on a helper PLL, 6.7 ps steps, PI, sigma-delta: 0.03 bit of upstream
phase wander in simulation, the OLT tolerating 4), GEM CPU queues for OMCI,
optics pins with the laser held off until both lanes are up. Sniffer (ONU side, 0x900,
`sniffer_regs.h`; with a passive tap on a stock ONU's fibre, the BOSA receiving the downstream copy and the
card's upstream receiver, on the burst enable lane's receiver, that ONU's bursts): one ONU's PLOAMs, BWmap
entries and GEM fragments (still ciphered) and its descrambled bursts, timestamped, read by the CPU as records;
the laser is held off while it records; checked in simulation.
Both sides checked in simulation, fit together on the DE25-Standard with the
HSMC card pins placed, flow engine, EPON OLT and ONU, cage 0's Ethernet port and the sniffer included: 40.8K ALMs (87 %), 348 of 358 M20K, setup slack +0.014 ns
(`agilex5/gts/fit.sh out/pon_board PonBoard`).
With the HPS: `de25-standard/ghrd/` brings Terasic's DE25-Standard reference design (GHRD, not in this
repository: `GHRD=<its folder> agilex5/gts/fit.sh <folder outside the repository> ghrd`) to Quartus 26.1, removes
its JTAG debug masters and shrinks its on-chip RAM to make room, and wires PonBoard in: its three register
windows on the HPS lightweight bridge (+0x100000 OLT, +0x101000 ONU, +0x102000 Ethernet), its 18 interrupts on
f2h_irq1 bits 0-17, its management clock the GHRD's 100 MHz: 42.9K ALMs (92 %), 352 of 358 M20K,
setup slack +0.005 ns. A fit folder holding an earlier fit made Quartus 26.1.1 crash (internal error 22912) or fail where a fresh
one passed, so fit.sh empties its own folder and refuses any other non-empty one. The HPS DDR settings were upgraded by Quartus: to check on the board.

## License

Hardware and RTL are licensed under the CERN Open Hardware Licence v2 -
Strongly Reciprocal ([CERN-OHL-S-2.0](LICENSE)).
