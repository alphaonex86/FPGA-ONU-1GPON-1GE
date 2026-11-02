#!/bin/bash
# SPDX-License-Identifier: CERN-OHL-S-2.0
# Fit a top on the DE25-Standard GTS pins with the variants of gen_ip.sh (DIR/ip), then print the worst setup and hold
# slack over the timing corners (fails if negative). TOP: hsmc_lanes (lane check, default) or PonBoard (Spinal, needs
# sbt or SBT=path; its PON lanes switch profiles by DR). Every other port but clk is virtual: the HPS side (bridges,
# interrupts) is not placed yet.
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
[ "$dir/ip/dr_ctrl.ip" -nt "$src/gen_ip.sh" ] || "$src/gen_ip.sh" "$dir/ip" # variants older than their script: regenerated
ips="srcss olt onu ben_tx eth pon_clocks helper_pll" # divider after its source
dr_top=$dir/support_logic/dr_top/synth/dr_top.sv # the PON lanes' DR profiles, from quartus_tlg below
case $top in
    hsmc_lanes) verilog=$src/hsmc_lanes.sv ;;
    PonBoard)
        (cd "$repo" && "${SBT:-sbt}" -batch "runMain pon.PonBoardVerilog") > "$dir/sbt.log" 2>&1 || die "sbt, see $dir/sbt.log"
        verilog=$repo/out/rtl/PonBoard.v
        sources="$dr_top $(cat "$repo/out/rtl/PonBoard.sources")" # black box RTL (taxi)
        ips="srcss olt olt_1g25 onu onu_1g25 ben_tx ben_1g25 dr_ctrl eth clock_mux pon_clocks helper_pll" ;;
    *) die "unknown top $top" ;;
esac
# DE25-Standard user manual, HSMC GTS table (P pins; N follows)
pins="pon_refclk:AT120 eth_refclk:AP120 ben_tx_p:AU129 onu_tx_p:AR129 eth_tx_p:AN129 olt_tx_p:AL129
      onu_rx_p:AP135 eth_rx_p:AM135 olt_rx_p:AK135"
# HSMC card IO (common/pcb/gen/boards.py HSMC_IO -> hsmcpins.de25_name -> manual HSMC_D table), 3.3 V by JP5
io_pins="sfp_txDisable:F34 sfp_txFault:H37 sfp_los:Y34 sfp_modAbs:Y15 sfp_scl:AL32 sfp_sda:AK21 sfp_rs0:AL29
         sfp_rs1:AJ1 onuPins_laserDisable:AG36 onuPins_rxSd:Y24 onuPins_scl:AB24 onuPins_sda:T8 vcxoPwm:AG21 vcxo156Pwm:R2
         onuPins_burstCut:T18 onuPins_benIo:AJ2
         ethSfp_txDisable:AK25 ethSfp_txFault:Y8 ethSfp_los:AG13 ethSfp_modAbs:AB4 ethSfp_scl:AH4 ethSfp_sda:AD2
         ethSfp_rs0:T15 ethSfp_rs1:Y4"
clock_pins="clock_50:D8" # board oscillator, 3.3 V; HVIO: to the helper PLL over a global clock
cd "$dir"
{
    echo 'set_global_assignment -name FAMILY "Agilex 5"'
    echo 'set_global_assignment -name DEVICE A5ED013BB32AE4SR1'
    echo "set_global_assignment -name TOP_LEVEL_ENTITY $top"
    echo 'set_global_assignment -name OPTIMIZATION_MODE "HIGH PERFORMANCE EFFORT"' # PonBoard: many 311.04 MHz paths
    echo "set_global_assignment -name SYSTEMVERILOG_FILE $verilog"
    for f in ${sources:-}; do echo "set_global_assignment -name SYSTEMVERILOG_FILE $f"; done
    for ip in $ips; do echo "set_global_assignment -name IP_FILE ip/$ip.ip"; done
    if [ $top != hsmc_lanes ]; then # the DR group, profile:id:channel:combinations (the ONU's
        # burst enable lane switches with it); both sides GPON at startup
        for p in olt:1:0:1,3 olt_1g25:2:0:2,4 onu:3:1:1,2 onu_1g25:4:1:3,4 ben_tx:5:2:1,2 ben_1g25:6:2:3,4; do
            IFS=: read -r name id channel combinations <<< "$p"
            i=${name}_inst
            echo "set_instance_assignment -name DR_IP_INSTANCE -to $i -section_id dr_top $name"
            echo "set_instance_assignment -name DR_GROUP_RECONFIG_ID $id -to $i -section_id dr_top"
            echo "set_instance_assignment -name DR_COMBINATION_IP_INSTANCE_RELATIVE_LOCATION -to $i -section_id dr_top $channel"
            for c in ${combinations/,/ }; do echo "set_instance_assignment -name DR_COMBINATION -section_id dr_top -to $i $c"; done
        done
        echo "set_global_assignment -name DR_GROUP_STARTUP_COMBINATION -section_id dr_top 1"
        echo "set_global_assignment -name SDC_ENTITY_FILE support_logic/dr_top/synth/dr_top.sdc -entity dr_top" # profiles exclusive
    fi
    echo 'set_global_assignment -name SDC_FILE fit.sdc' # after the IP constraints: their clocks exist
    ports=$(awk "/^module $top \\(/,/\\);/" "$verilog" | sed -n -E 's/^\s*(input|output|inout)\s+wire\s+(\[[^]]+\]\s+)?([A-Za-z0-9_]+).*/\3:\2/p')
    [ -n "$ports" ] || die "no ports of $top in $verilog"
    for port in $ports; do
        name=${port%%:*}
        pin=$(echo $pins | tr ' ' '\n' | sed -n "s/^$name://p")
        io=$(echo $io_pins | tr ' ' '\n' | sed -n "s/^$name://p")
        clock=$(echo $clock_pins | tr ' ' '\n' | sed -n "s/^$name://p")
        if [ -n "$pin" ]; then echo "set_location_assignment PIN_$pin -to $name"
        elif [ -n "$clock" ]; then
            echo "set_location_assignment PIN_$clock -to $name"
            echo "set_instance_assignment -name IO_STANDARD \"3.3-V LVCMOS\" -to $name"
            echo "set_instance_assignment -name GLOBAL_SIGNAL GLOBAL_CLOCK -to $name"
        elif [ -n "$io" ]; then
            echo "set_location_assignment PIN_$io -to $name"
            echo "set_instance_assignment -name IO_STANDARD \"3.3-V LVCMOS\" -to $name"
        elif [ "$name" != clk ] && ! echo $pins | grep -qw "${name%_n}_p"; then
            echo "set_instance_assignment -name VIRTUAL_PIN ON -to ${name}$([ -n "${port#*:}" ] && echo '[*]')"
        fi
    done
} > $top.qsf
{
    echo 'create_clock -name pon_refclk -period 6.430 [get_ports pon_refclk]'
    echo 'create_clock -name eth_refclk -period 6.400 [get_ports eth_refclk]'
    echo 'set helper [get_clocks -nowarn {*helper_pll*}]' # its IP constrains it; samples the halved PON clocks
    echo 'if {[get_collection_size $helper] > 0} {'
    echo '  set_clock_groups -asynchronous -group $helper -group [remove_from_collection [all_clocks] $helper]'
    echo '}'
    echo 'if {[get_collection_size [get_ports -nowarn clk]] > 0} {'
    echo '  create_clock -name clk -period 10 [get_ports clk]' # HPS management clock, 100 MHz
    echo '  set_clock_groups -asynchronous -group clk -group [remove_from_collection [all_clocks] [get_clocks clk]]'
    echo '}'
    echo 'set eth_tx [get_clocks -nowarn {*eth*tx_clkout*}]' # Ethernet lane: tx and rx clocks unrelated (taxi, queues CDC)
    echo 'set eth_rx [get_clocks -nowarn {*eth*rx_clkout*}]'
    echo 'if {[get_collection_size $eth_tx] > 0 && [get_collection_size $eth_rx] > 0} {'
    echo '  set_clock_groups -asynchronous -group $eth_tx -group $eth_rx'
    echo '}'
    echo 'set_false_path -to [get_keepers -nowarn {*buffercc*|buffers_0* *bufferCC*|buffers_0*}]' # Spinal synchronizers
    echo 'set dr_rx [get_clocks -nowarn {*olt*_inst_period*|*rx_*}]' # DR: each profile's RX words on its rx_clkout, then FIFOs
    echo 'if {[get_collection_size $dr_rx] > 0} {'
    echo '  set_clock_groups -asynchronous -group $dr_rx'
    echo '}'
    echo 'foreach_in_collection c [get_clocks -nowarn {*_1g25_inst_period*|*_user_clk1_reg*}] {' # a 1.25 Gb/s profile's IP
    echo '  create_generated_clock -name [get_clock_info -name $c] -master_clock [get_clock_info -master_clock $c] \'
    echo '    -source [get_node_info -name [get_clock_info -master_clock_pin $c]] -multiply_by 3125 -divide_by 7776 \'
    echo '    -add [get_node_info -name [get_clock_info -targets $c]]' # took the GPON clock on the shared node, x1: 125 MHz
    echo '}'
    echo 'foreach side {olt onu} {'
    echo '  set gpon [get_clocks -nowarn "${side}Side_clocks|*${side}_inst_period*"]' # EPON logic: reset while the core clock is GPON
    echo '  set epon_logic [get_keepers -nowarn "${side}Core|epon_* ${side}Core|eponCC|*"]'
    echo '  if {[get_collection_size $gpon] == 0 || [get_collection_size $epon_logic] == 0} {'
    echo '    post_message -type error "fit.sdc: no GPON clock or EPON logic for $side"'
    echo '  } else {'
    echo '    set_false_path -from $gpon -to $epon_logic'
    echo '    set_false_path -from $epon_logic -to $gpon'
    echo '  }'
    echo '}'
    echo 'foreach {gpon epon} {olt olt_1g25 onu onu_1g25 ben_tx ben_1g25} {' # every clock of one DR profile (lane, mux, divider) vs the other's
    echo '  set dr_gpon [get_clocks -nowarn "*${gpon}_inst_period*"]'
    echo '  set dr_1g25 [get_clocks -nowarn "*${epon}_inst_period*"]'
    echo '  if {[get_collection_size $dr_gpon] == 0 || [get_collection_size $dr_1g25] == 0} {'
    echo '    post_message -type error "fit.sdc: no clocks for the DR profiles $gpon / $epon"'
    echo '  } else {'
    echo '    set_clock_groups -physically_exclusive -group $dr_gpon -group $dr_1g25'
    echo '  }'
    echo '}'
    echo 'if {[get_collection_size [get_ports -nowarn altera_reserved_tck]] > 0} {' # JTAG debug of the DR controller
    echo '  create_clock -name altera_reserved_tck -period 100 [get_ports altera_reserved_tck]'
    echo '  set_clock_groups -asynchronous -group altera_reserved_tck'
    echo '}'
    echo 'derive_clock_uncertainty'
} > fit.sdc
echo "project_open $top; create_timing_netlist; read_sdc; update_timing_netlist
foreach_in_collection c [get_available_operating_conditions] { set_operating_conditions \$c; update_timing_netlist
puts \"SETUP [lindex [report_timing -setup -npaths 1] 1]\"; puts \"HOLD [lindex [report_timing -hold -npaths 1] 1]\" }
report_clocks -file clocks.rpt; delete_timing_netlist; project_close" > slack.tcl
[ $top = hsmc_lanes ] || "$QUARTUS/quartus_tlg" --tool dr_synth $top > tlg.log 2>&1 || die "DR logic generation, see $dir/tlg.log"
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
