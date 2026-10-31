#!/bin/bash
# SPDX-License-Identifier: CERN-OHL-S-2.0
# Fit one Verilog core alone (every port but clk virtual) on the DE25-Nano FPGA with Quartus Pro.
# Usage: quartus_core.sh VERILOG TOP DIR FIT_PERIOD_NS [POWER_PERIOD_NS]
# Writes DIR/summary and prints it: ALMs, RAM blocks, worst-corner Fmax (MHz)[, dynamic mW at POWER_PERIOD_NS].
set -euo pipefail

QUARTUS=${QUARTUS:-$HOME/altera_pro/26.1.1/quartus/bin}
DEVICE=A5EB013BB23BE4SCS
[ $# -ge 4 ] || { echo "usage: $0 VERILOG TOP DIR FIT_PERIOD_NS [POWER_PERIOD_NS]" >&2; exit 1; }
verilog=$(realpath "$1"); top=$2; dir=$3; fit_period=$4; power_period=${5:-}

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
            name=${b:-$a}; [ "$name" = clk ] && continue
            [ -n "$b" ] && name="$name[*]"
            echo "set_instance_assignment -name VIRTUAL_PIN ON -to \"$name\""
        done
} > core.qsf
echo "PROJECT_REVISION = \"core\"" > core.qpf
echo "create_clock -name clk -period $fit_period [get_ports clk]" > fit.sdc
# Fmax of every timing corner, appended; the worst is kept
echo "project_open core; create_timing_netlist; read_sdc; update_timing_netlist
foreach_in_collection c [get_available_operating_conditions] {
set_operating_conditions \$c; update_timing_netlist; report_clock_fmax_summary -append -file fmax.rpt }
delete_timing_netlist; project_close" > fmax.tcl
rm -f fmax.rpt

"$QUARTUS/quartus_syn" core -c core > syn.log 2>&1 || die "quartus_syn, see $dir/syn.log"
"$QUARTUS/quartus_fit" core -c core > fit.log 2>&1 || die "quartus_fit, see $dir/fit.log"
"$QUARTUS/quartus_sta" -t fmax.tcl > sta.log 2>&1 || die "quartus_sta, see $dir/sta.log"
alm=$(pick core.fit.summary '.*Logic utilization \(in ALMs\) *: *([0-9,]+) .*' | tr -d ,)
ram=$(pick core.fit.summary '.*Total RAM Blocks *: *([0-9,]+) .*' | tr -d ,)
fmax=$(sed -n -E 's/^; *([0-9.]+) MHz *;.*/\1/p' fmax.rpt | sort -n | head -1)
[ -n "$fmax" ] || die "no Fmax in $dir/fmax.rpt"
summary="$alm $ram $fmax"
if [ -n "$power_period" ]; then
    echo "create_clock -name clk -period $power_period [get_ports clk]" > power.sdc
    sed -i 's/fit.sdc/power.sdc/' core.qsf
    "$QUARTUS/quartus_pow" core -c core > pow.log 2>&1 || die "quartus_pow, see $dir/pow.log"
    sed -i 's/power.sdc/fit.sdc/' core.qsf
    watts=$(pick core.pow.summary '.*Total Dynamic Power Dissipation *: *([0-9.]+) W.*')
    summary="$summary $(echo "1000 * $watts" | bc -l)"
fi
echo "$summary" | tee summary
