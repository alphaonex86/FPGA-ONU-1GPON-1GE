#!/bin/bash
# SPDX-License-Identifier: CERN-OHL-S-2.0
# Fit one Verilog core alone (every port but clk virtual) on an Agilex 5 (DEVICE) with Quartus Pro.
# Usage: quartus_core.sh VERILOG TOP DIR FIT_PERIOD_NS [POWER_PERIOD_NS]
# Writes DIR/summary and prints it: ALMs, RAM blocks, worst-corner Fmax (MHz)[, dynamic mW at POWER_PERIOD_NS].
# CLK2=port:ratio adds a second clock, same source, ratio times the period: the summary then also gives its Fmax and
# the worst setup slack (ns) at the given periods, crossings between the clocks included.
set -euo pipefail

QUARTUS=${QUARTUS:-$HOME/altera_pro/26.1.1/quartus/bin}
DEVICE=${DEVICE:-A5ED013BB32AE4SCS}   # DE25-Standard; DE25-Nano: A5EB013BB23BE4SCS
[ $# -ge 4 ] || { echo "usage: $0 VERILOG TOP DIR FIT_PERIOD_NS [POWER_PERIOD_NS]" >&2; exit 1; }
verilog=$(realpath "$1"); top=$2; dir=$3; fit_period=$4; power_period=${5:-}
clk2=${CLK2:-}; ratio=${clk2#*:}; clk2=${clk2%%:*}

die() { echo "ERROR: $*" >&2; exit 1; }
# pick FILE REGEX: first capture group of REGEX in FILE, fail if absent
pick() { local v; v=$(sed -n -E "s/$2/\1/p" "$1" | head -1); [ -n "$v" ] || die "'$2' not found in $1"; echo "$v"; }

mkdir -p "$dir"
cd "$dir"
{
    echo "set_global_assignment -name FAMILY \"Agilex 5\""
    echo "set_global_assignment -name DEVICE $DEVICE"
    echo "set_global_assignment -name TOP_LEVEL_ENTITY $top"
    echo "set_global_assignment -name VERILOG_FILE $verilog"
    echo "set_global_assignment -name VERILOG_MACRO \"SYNTHESIS=1\"" # hides sim-only initial blocks
    echo "set_global_assignment -name SDC_FILE fit.sdc"
    awk "/^module $top \\(/,/\\);/" "$verilog" |
        sed -n -E 's/^\s*(input|output)\s+wire\s+(\[[^]]+\]\s+)?([A-Za-z0-9_]+).*/\2 \3/p' |
        while read -r a b; do
            name=${b:-$a}; [ "$name" = clk ] || [ "$name" = "$clk2" ] && continue
            [ -n "$b" ] && name="$name[*]"
            echo "set_instance_assignment -name VIRTUAL_PIN ON -to \"$name\""
        done
} > core.qsf
echo "PROJECT_REVISION = \"core\"" > core.qpf
clocks() {   # PERIOD
    echo "create_clock -name clk -period $1 [get_ports clk]"
    [ -z "$clk2" ] || echo "create_clock -name $clk2 -period $(echo "$1 * $ratio" | bc -l) [get_ports $clk2]"
}
clocks "$fit_period" > fit.sdc
# Fmax of every timing corner, appended; the worst is kept
echo "project_open core; create_timing_netlist; read_sdc; update_timing_netlist
foreach_in_collection c [get_available_operating_conditions] {
set_operating_conditions \$c; update_timing_netlist; report_clock_fmax_summary -append -file fmax.rpt
puts \"SLACK [lindex [report_timing -setup -npaths 1] 1]\" }
delete_timing_netlist; project_close" > fmax.tcl
rm -f fmax.rpt

"$QUARTUS/quartus_syn" core -c core > syn.log 2>&1 || die "quartus_syn, see $dir/syn.log"
"$QUARTUS/quartus_fit" core -c core > fit.log 2>&1 || die "quartus_fit, see $dir/fit.log"
"$QUARTUS/quartus_sta" -t fmax.tcl > sta.log 2>&1 || die "quartus_sta, see $dir/sta.log"
alm=$(pick core.fit.summary '.*Logic utilization \(in ALMs\) *: *([0-9,]+) .*' | tr -d ,)
ram=$(pick core.fit.summary '.*Total RAM Blocks *: *([0-9,]+) .*' | tr -d ,)
worst_fmax() { sed -n -E "s/^; *([0-9.]+) MHz *;[^;]*; $1 *;.*/\1/p" fmax.rpt | sort -n | head -1; }
fmax=$(worst_fmax clk)
[ -n "$fmax" ] || die "no Fmax in $dir/fmax.rpt"
summary="$alm $ram $fmax"
if [ -n "$clk2" ]; then
    fmax2=$(worst_fmax "$clk2"); slack=$(sed -n 's/^SLACK //p' sta.log | sort -g | head -1)
    [ -n "$fmax2" ] && [ -n "$slack" ] || die "no $clk2 Fmax or slack in $dir"
    summary="$summary $fmax2 $slack"
fi
if [ -n "$power_period" ]; then
    clocks "$power_period" > power.sdc
    sed -i 's/fit.sdc/power.sdc/' core.qsf
    "$QUARTUS/quartus_pow" core -c core > pow.log 2>&1 || die "quartus_pow, see $dir/pow.log"
    sed -i 's/power.sdc/fit.sdc/' core.qsf
    watts=$(pick core.pow.summary '.*Total Dynamic Power Dissipation *: *([0-9.]+) W.*')
    summary="$summary $(echo "1000 * $watts" | bc -l)"
fi
echo "$summary" | tee summary
