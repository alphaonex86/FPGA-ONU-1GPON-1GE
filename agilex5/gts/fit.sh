#!/bin/bash
# SPDX-License-Identifier: CERN-OHL-S-2.0
# Fit a top on the DE25-Standard GTS pins with the variants of gen_ip.sh (DIR/ip), then print the worst setup and hold
# slack over the timing corners (fails if negative). TOP: hsmc_lanes (lane check, default) or OltBoard (Spinal, needs
# sbt or SBT=path). Every other port but clk is virtual: HSMC IO and HPS bridge pins are not placed yet.
# Usage: fit.sh DIR [TOP]
set -euo pipefail
QUARTUS=${QUARTUS:-$HOME/altera_pro/26.1.1/quartus/bin}
die() { echo "ERROR: $*" >&2; exit 1; }
[ $# -ge 1 ] && [ $# -le 2 ] || die "usage: $0 DIR [TOP]"
src=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$src/../.." && pwd)
top=${2:-hsmc_lanes}
mkdir -p "$1"
dir=$(cd "$1" && pwd)
[ "$dir/ip/eth.ip" -nt "$src/gen_ip.sh" ] || "$src/gen_ip.sh" "$dir/ip" # variants older than their script: regenerated
case $top in
    hsmc_lanes) verilog=$src/hsmc_lanes.sv ;;
    OltBoard)
        (cd "$repo" && "${SBT:-sbt}" -batch "runMain pon.OltBoardVerilog") > "$dir/sbt.log" 2>&1 || die "sbt, see $dir/sbt.log"
        verilog=$repo/out/rtl/OltBoard.v ;;
    *) die "unknown top $top" ;;
esac
# DE25-Standard user manual, HSMC GTS table (P pins; N follows)
pins="pon_refclk:AT120 eth_refclk:AP120 ben_tx_p:AU129 onu_tx_p:AR129 eth_tx_p:AN129 olt_tx_p:AL129
      onu_rx_p:AP135 eth_rx_p:AM135 olt_rx_p:AK135"
cd "$dir"
{
    echo 'set_global_assignment -name FAMILY "Agilex 5"'
    echo 'set_global_assignment -name DEVICE A5ED013BB32AE4SCS'
    echo "set_global_assignment -name TOP_LEVEL_ENTITY $top"
    echo "set_global_assignment -name SYSTEMVERILOG_FILE $verilog"
    for ip in srcss olt onu ben_tx eth pon_clocks; do echo "set_global_assignment -name IP_FILE ip/$ip.ip"; done # divider after its source
    echo 'set_global_assignment -name SDC_FILE fit.sdc' # after the IP constraints: their clocks exist
    ports=$(awk "/^module $top \\(/,/\\);/" "$verilog" | sed -n -E 's/^\s*(input|output)\s+wire\s+(\[[^]]+\]\s+)?([A-Za-z0-9_]+).*/\3:\2/p')
    [ -n "$ports" ] || die "no ports of $top in $verilog"
    for port in $ports; do
        name=${port%%:*}
        pin=$(echo $pins | tr ' ' '\n' | sed -n "s/^$name://p")
        if [ -n "$pin" ]; then echo "set_location_assignment PIN_$pin -to $name"
        elif [ "$name" != clk ] && ! echo $pins | grep -qw "${name%_n}_p"; then
            echo "set_instance_assignment -name VIRTUAL_PIN ON -to ${name}$([ -n "${port#*:}" ] && echo '[*]')"
        fi
    done
} > $top.qsf
{
    echo 'create_clock -name pon_refclk -period 6.430 [get_ports pon_refclk]'
    echo 'create_clock -name eth_refclk -period 6.400 [get_ports eth_refclk]'
    echo 'if {[get_collection_size [get_ports -nowarn clk]] > 0} {'
    echo '  create_clock -name clk -period 10 [get_ports clk]' # HPS management clock, 100 MHz
    echo '  set_clock_groups -asynchronous -group clk -group [remove_from_collection [all_clocks] [get_clocks clk]]'
    echo '}'
    echo 'set_false_path -to [get_keepers -nowarn {*buffercc*|buffers_0* *bufferCC*|buffers_0*}]' # Spinal synchronizers
    echo 'derive_clock_uncertainty'
} > fit.sdc
echo "project_open $top; create_timing_netlist; read_sdc; update_timing_netlist
foreach_in_collection c [get_available_operating_conditions] { set_operating_conditions \$c; update_timing_netlist
puts \"SETUP [lindex [report_timing -setup -npaths 1] 1]\"; puts \"HOLD [lindex [report_timing -hold -npaths 1] 1]\" }
report_clocks -file clocks.rpt; delete_timing_netlist; project_close" > slack.tcl
"$QUARTUS/quartus_syn" $top > syn.log 2>&1 || { grep -E "^Error" syn.log | head -20 >&2; die "synthesis, see $dir/syn.log"; }
"$QUARTUS/quartus_fit" $top > fit.log 2>&1 || { grep -E "^Error" fit.log | head -20 >&2; die "fit, see $dir/fit.log"; }
"$QUARTUS/quartus_sta" -t slack.tcl > sta.log 2>&1 || die "timing, see $dir/sta.log"
! grep -m3 "found without an associated clock assignment" sta.log >&2 || die "unconstrained clock, see $dir/sta.log"
setup=$(sed -n 's/^SETUP //p' sta.log | sort -g | head -1)
hold=$(sed -n 's/^HOLD //p' sta.log | sort -g | head -1)
[ -n "$setup" ] && [ -n "$hold" ] || die "no slack in $dir/sta.log"
alm=$(sed -n -E 's/.*Logic utilization \(in ALMs\) *: *([0-9,]+) .*/\1/p' $top.fit.summary | tr -d ,)
echo "$top fit: $alm ALMs, worst setup slack $setup ns, hold $hold ns"
awk -v s="$setup" -v h="$hold" 'BEGIN { exit !(s >= 0 && h >= 0) }' || die "negative slack, see $dir/sta.log"
