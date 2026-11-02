#!/bin/sh
# SPDX-License-Identifier: CERN-OHL-S-2.0
# Sniffer card bitstream, out/ecp5/sniffer.bit, from the repo root. Needs out/rtl/SnifferBridge.v
# (sbt "runMain pon.Generate"), yosys, nextpnr-ecp5 and ecppack (Debian: yosys nextpnr-ecp5 fpga-trellis).
set -e
O=out/ecp5
B=sniffer/pcb/sniffer/sniffer.kicad_pcb
[ -f out/rtl/SnifferBridge.v ] || { echo "out/rtl/SnifferBridge.v missing: sbt \"runMain pon.Generate\""; exit 1; }
mkdir -p $O
python3 sniffer/ecp5/gen.py $B $O
nice -n 19 yosys -q -l $O/yosys.log -p "read_verilog -I$O sniffer/ecp5/sniffer_ecp5.v out/rtl/SnifferBridge.v; \
  synth_ecp5 -top sniffer_ecp5 -json $O/sniffer.json"
nice -n 19 nextpnr-ecp5 --um-25k --package CABGA381 --speed 6 --json $O/sniffer.json --lpf $O/sniffer.lpf \
  --textcfg $O/sniffer.config --log $O/nextpnr.log
ecppack --compress $O/sniffer.config $O/sniffer.bit
grep -E "Max frequency for clock|ERROR|Warning: .*(constrain|unrouted)" $O/nextpnr.log || true
echo "wrote $O/sniffer.bit"
