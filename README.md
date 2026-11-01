# FPGA-ONU

An open PON endpoint in one chip: 1 GPON/EPON ONU port (BOSA, to the ISP OLT), 1 SFP
cage (switch SFP, or EPON/GPON OLT module for the OLT role) and 1 Gigabit
Ethernet port, with the MACs, the Ethernet PHY and a RISC-V CPU integrated.
Developed and proven on an FPGA, the final target is an ASIC that holds everything.

## Goals

- GPON ONU MAC: GTC framing, FEC, AES-128 decryption, PLOAM, OMCI, upstream
  burst mode with DBA. EPON MAC (MPCP, OAM) and the OLT role of both.
- 1000BASE-T Ethernet port, PHY integrated in the target ASIC (FPGA phase: a
  copper SFP in an SFP+ cage on our HSMC card for the DE25-Standard).
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
- `de25-standard/`: Terasic DE25-Standard, our HSMC card (`pcb/hsmc`: placed and
  routed, DRC/ERC clean, not yet ordered).
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
the default allocation (OMCC), fragmented to the grant. Downstream AES-128
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
submodule: `git submodule update --init`), two ports back to back.
HSMC card GTS lanes (OLT, ONU, burst enable, Ethernet) fit in Quartus at their
line rates from their refclk pins (`agilex5/gts/fit.sh`).
Board top (`agilex5/hw/spinal/pon/PonBoard.scala`), OLT side: OLT MAC on its GTS lane
(2x oversampled upstream), HPS lightweight bridge to the MAC and board registers
(GTS reset/status, SFP cage pins, module I2C; `olt_board_regs.h`, `onu_board_regs.h`) and GEM CPU
queues for OMCI (whole packets downstream, upstream fragments with descriptors;
`gem_queues.h`). ONU side: ONU MAC on the BOSA data and burst enable lanes, its
clock kept on the recovered downstream clock by a VCXO loop (DDMTD phase
detector on a helper PLL, 6.7 ps steps, PI, sigma-delta: 0.03 bit of upstream
phase wander in simulation, the OLT tolerating 4), GEM CPU queues for OMCI,
optics pins with the laser held off until both lanes are up.
Both sides checked in simulation, fit together on the DE25-Standard with the
HSMC card pins placed: 11.1K ALMs, setup slack +0.129 ns
(`agilex5/gts/fit.sh out/pon_board PonBoard`).

## License

Hardware and RTL are licensed under the CERN Open Hardware Licence v2 -
Strongly Reciprocal ([CERN-OHL-S-2.0](LICENSE)).
