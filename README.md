# FPGA-ONU

An open PON endpoint in one chip: 1 GPON/EPON ONU port (BOSA, to the ISP OLT), 1 SFP
cage (switch SFP, or EPON/GPON OLT module for the OLT role) and 1 Gigabit
Ethernet port, with the MACs, the Ethernet PHY and a RISC-V CPU integrated.
Developed and proven on an FPGA, the final target is an ASIC that holds everything.

## Goals

- GPON ONU MAC: GTC framing, FEC, AES-128 decryption, PLOAM, OMCI, upstream
  burst mode with DBA. EPON MAC (MPCP, OAM) and the OLT role of both.
- 1000BASE-T Ethernet port, PHY integrated in the target ASIC (FPGA phase: the
  DE25-Nano board PHY through the HPS EMAC, the HPS bridging to the fabric).
- SFP cage autodetects the module (EEPROM): 1000BASE-X, EPON OLT or GPON OLT.
- BOSA on board; all optics control (burst, APC, LOS, DDM) in the FPGA/ASIC
  logic; outside only analog: discrete RF transistors, a small ADC/DAC, the APD
  bias boost and passives (FPGA phase: an SY88236L BOSA driver and an ECP5
  SerDes bridge board on the DE25-Nano GPIO headers).
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

## Status

GPON OLT MAC in simulation (SpinalHDL): GTC downstream framer and deframer,
upstream burst receiver, checked against G.984.3 vectors and a Scala ONU model.

## License

Hardware and RTL are licensed under the CERN Open Hardware Licence v2 -
Strongly Reciprocal ([CERN-OHL-S-2.0](LICENSE)).
