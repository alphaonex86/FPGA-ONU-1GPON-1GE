#!/bin/bash
# SPDX-License-Identifier: CERN-OHL-S-2.0
# Fit a top on the DE25-Standard GTS pins with the variants of gen_ip.sh (DIR/ip), then print the worst setup and hold
# slack over the timing corners (fails if negative). TOP: hsmc_lanes (lane check, default), PonBoard (Spinal, needs
# sbt or SBT=path; its PON lanes switch profiles by DR; every other port but clk virtual) or ghrd (PonBoard in the
# Terasic GHRD with the HPS: GHRD=its directory, brought into DIR by de25-standard/ghrd/prepare.sh).
# Usage: [GHRD=dir] fit.sh DIR [TOP]    (DIR: new, or one of fit.sh, emptied first)
set -euo pipefail
QUARTUS=${QUARTUS:-$HOME/altera_pro/26.1.1/quartus/bin}
die() { echo "ERROR: $*" >&2; exit 1; }
[ $# -ge 1 ] && [ $# -le 2 ] || die "usage: $0 DIR [TOP]"
src=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$src/../.." && pwd)
top=${2:-hsmc_lanes}
# a fit in a DIR holding an earlier fit's Quartus state crashed (22912) or failed (23098) where a fresh one passed:
# every fit starts empty; a DIR of ours (marker) is emptied, any other non-empty one refused
mark=.fit_sh_dir
if [ -e "$1/$mark" ]; then rm -r "$1"
elif [ -d "$1" ] && [ -n "$(ls -A "$1")" ]; then die "$1: not empty and not made by fit.sh"; fi
mkdir -p "$1"
touch "$1/$mark"
dir=$(cd "$1" && pwd)
"$src/gen_ip.sh" "$dir/ip"
ips="srcss sfp0 onu ben_tx sfp1 pon_clocks helper_pll" # divider after its source
case $top in
    hsmc_lanes) verilog=$src/hsmc_lanes.sv ;;
    PonBoard|ghrd)
        (cd "$repo" && "${SBT:-sbt}" -batch "runMain pon.PonBoardVerilog") > "$dir/sbt.log" 2>&1 || die "sbt, see $dir/sbt.log"
        # a copy (with its RAM init files): another fit's sbt may rewrite out/rtl meanwhile
        cp "$repo"/out/rtl/PonBoard.v "$repo"/out/rtl/PonBoard.v_*.bin "$dir/" || die "no PonBoard.v"
        verilog=$dir/PonBoard.v
        sources=$(cat "$repo/out/rtl/PonBoard.sources") # black box RTL (taxi); the DR profiles (dr_top) from quartus_tlg's qip
        ips="srcss sfp0 sfp0_1g25 onu onu_1g25 ben_tx ben_1g25 dr_ctrl sfp1 clock_mux pon_clocks helper_pll" ;;
    *) die "unknown top $top" ;;
esac
project=$top
[ $top != ghrd ] || { project=golden_top; [ -n "${GHRD:-}" ] || die "ghrd: set GHRD to the Terasic GHRD directory"; }
# DE25-Standard user manual, HSMC GTS table (P pins; N follows)
pins="pon_refclk:AT120 eth_refclk:AP120 ben_tx_p:AU129 onu_tx_p:AR129 sfp1_tx_p:AN129 sfp0_tx_p:AL129
      ben_rx_p:AT135 onu_rx_p:AP135 sfp1_rx_p:AM135 sfp0_rx_p:AK135"
# HSMC card IO (common/pcb/gen/boards.py HSMC_IO -> hsmcpins.de25_name -> manual HSMC_D table), 3.3 V by JP5
io_pins="sfp0_txDisable:F34 sfp0_txFault:H37 sfp0_los:Y34 sfp0_modAbs:Y15 sfp0_scl:AL32 sfp0_sda:AK21 sfp0_rs0:AL29
         sfp0_rs1:AJ1 onuPins_laserDisable:AG36 onuPins_rxSd:Y24 onuPins_scl:AB24 onuPins_sda:T8 vcxoPwm:AG21 vcxo156Pwm:R2
         onuPins_burstCut:T18 onuPins_benIo:AJ2 onuPins_usSd:Y18
         sfp1_txDisable:AK25 sfp1_txFault:Y8 sfp1_los:AG13 sfp1_modAbs:AB4 sfp1_scl:AH4 sfp1_sda:AD2
         sfp1_rs0:T15 sfp1_rs1:Y4"
clock_pins="clock_50:D8" # board oscillator CLOCK1_50, bank 6C at 1.8 V (manual); to the helper PLL over a global clock
cd "$dir"
p="" # PonBoard's hierarchy prefix in fit.sdc
if [ $top = ghrd ]; then # its golden_top.qsf gets our assignments below
    "$repo/de25-standard/ghrd/prepare.sh" "$GHRD" "$dir"
    python3 -I "$repo/de25-standard/ghrd/ghrd_top.py" golden_top.v "$verilog" $(echo $pins $io_pins | tr ' ' '\n' | cut -d: -f1) \
        > ghrd_top.log || die "golden_top patch, see $dir/ghrd_top.log"
    p="pon|"
else
    : > $project.qsf
fi
{
    if [ $top != ghrd ]; then
        echo 'set_global_assignment -name FAMILY "Agilex 5"'
        echo 'set_global_assignment -name DEVICE A5ED013BB32AE4SR1'
        echo "set_global_assignment -name TOP_LEVEL_ENTITY $top"
    fi
    echo 'set_global_assignment -name OPTIMIZATION_MODE "HIGH PERFORMANCE EFFORT"' # PonBoard: many 311.04 MHz paths
    echo "set_global_assignment -name SYSTEMVERILOG_FILE $verilog"
    for f in ${sources:-}; do echo "set_global_assignment -name SYSTEMVERILOG_FILE $f"; done
    for ip in $ips; do echo "set_global_assignment -name IP_FILE ip/$ip.ip"; done
    if [ $top != hsmc_lanes ]; then # the DR group, profile:id:channel:combinations: (OLT, ONU) 1 GPON/GPON (startup),
        # 2 1.25G/GPON, 3 GPON/1.25G, 4 1.25G/1.25G, so each side switches alone (the ONU's burst enable lane with it)
        for profile in sfp0:1:0:1,3 sfp0_1g25:2:0:2,4 onu:3:1:1,2 onu_1g25:4:1:3,4 ben_tx:5:2:1,2 ben_1g25:6:2:3,4; do
            IFS=: read -r name id channel combinations <<< "$profile"
            i=${name}_inst
            echo "set_instance_assignment -name DR_IP_INSTANCE -to $i -section_id dr_top $name"
            echo "set_instance_assignment -name DR_GROUP_RECONFIG_ID $id -to $i -section_id dr_top"
            echo "set_instance_assignment -name DR_COMBINATION_IP_INSTANCE_RELATIVE_LOCATION -to $i -section_id dr_top $channel"
            for c in ${combinations/,/ }; do echo "set_instance_assignment -name DR_COMBINATION -section_id dr_top -to $i $c"; done
        done
        echo "set_global_assignment -name DR_GROUP_STARTUP_COMBINATION -section_id dr_top 1"
    fi
    echo 'set_global_assignment -name SDC_FILE fit.sdc' # after the IP constraints: their clocks exist
    ports=$(awk "/^module ${top/ghrd/PonBoard} \\(/,/\\);/" "$verilog" | sed -n -E 's/^\s*(input|output|inout)\s+wire\s+(\[[^]]+\]\s+)?([A-Za-z0-9_]+).*/\3:\2/p')
    [ -n "$ports" ] || die "no ports of $top in $verilog"
    for port in $ports; do
        name=${port%%:*}
        pin=$(echo $pins | tr ' ' '\n' | sed -n "s/^$name://p")
        io=$(echo $io_pins | tr ' ' '\n' | sed -n "s/^$name://p")
        clock=$(echo $clock_pins | tr ' ' '\n' | sed -n "s/^$name://p")
        if [ -n "$pin" ]; then echo "set_location_assignment PIN_$pin -to $name"
        elif [ -n "$clock" ] && [ $top = ghrd ]; then # golden_top's CLOCK1_50
            echo "set_instance_assignment -name GLOBAL_SIGNAL GLOBAL_CLOCK -to CLOCK1_50"
        elif [ -n "$clock" ]; then
            echo "set_location_assignment PIN_$clock -to $name"
            echo "set_instance_assignment -name IO_STANDARD \"1.8-V LVCMOS\" -to $name"
            echo "set_instance_assignment -name GLOBAL_SIGNAL GLOBAL_CLOCK -to $name"
        elif [ -n "$io" ]; then
            echo "set_location_assignment PIN_$io -to $name"
            echo "set_instance_assignment -name IO_STANDARD \"3.3-V LVCMOS\" -to $name"
        elif [ $top != ghrd ] && [ "$name" != clk ] && ! echo $pins | grep -qw "${name%_n}_p"; then
            echo "set_instance_assignment -name VIRTUAL_PIN ON -to ${name}$([ -n "${port#*:}" ] && echo '[*]')"
        fi
    done
} >> $project.qsf
{
    echo "set p {$p}"
    echo 'create_clock -name pon_refclk -period 6.430 [get_ports pon_refclk]'
    echo 'create_clock -name eth_refclk -period 6.400 [get_ports eth_refclk]'
    echo 'set helper [get_clocks -nowarn {*helper_pll*}]' # its IP constrains it; samples the halved PON clocks
    echo 'if {[get_collection_size $helper] > 0} {'
    echo '  set_clock_groups -asynchronous -group $helper -group [remove_from_collection [all_clocks] $helper]'
    echo '}'
    if [ $top = ghrd ]; then # PonBoard's management clock: the GHRD's 100 MHz system clock
        echo 'set mgmt [get_clocks -nowarn {pll_inst|*outclk0}]'
        echo 'set ours [get_clocks -nowarn {pon_refclk eth_refclk pon|*}]'
        echo 'if {[get_collection_size $mgmt] == 0 || [get_collection_size $ours] == 0} {'
        echo '  post_message -type error "fit.sdc: no GHRD system clock or no PonBoard clock"'
        echo '} else {'
        echo '  set_clock_groups -asynchronous -group $mgmt -group $ours'
        echo '}'
        echo 'create_clock -name CLOCK0_50 -period 20 [get_ports CLOCK0_50]' # Terasic's board management I2C
        echo 'set_clock_groups -asynchronous -group CLOCK0_50'
    else
        echo 'if {[get_collection_size [get_ports -nowarn clk]] > 0} {'
        echo '  create_clock -name clk -period 10 [get_ports clk]' # HPS management clock, 100 MHz
        echo '  set_clock_groups -asynchronous -group clk -group [remove_from_collection [all_clocks] [get_clocks clk]]'
        echo '}'
    fi
    echo 'set eth_tx [get_clocks -nowarn {*sfp1*tx_clkout*}]' # Ethernet lane: tx and rx clocks unrelated (taxi, queues CDC)
    echo 'set eth_rx [get_clocks -nowarn {*sfp1*rx_clkout*}]'
    echo 'if {[get_collection_size $eth_tx] > 0 && [get_collection_size $eth_rx] > 0} {'
    echo '  set_clock_groups -asynchronous -group $eth_tx -group $eth_rx'
    echo '}'
    echo 'set_false_path -to [get_keepers -nowarn {*buffercc*|buffers_0* *bufferCC*|buffers_0*}]' # Spinal synchronizers
    echo 'set dr_rx [get_clocks -nowarn {*sfp0*_inst_period*|*rx_*}]' # DR: each profile's RX words on its rx_clkout, then FIFOs
    echo 'if {[get_collection_size $dr_rx] > 0} {'
    echo '  set_clock_groups -asynchronous -group $dr_rx'
    echo '}'
    echo 'foreach_in_collection c [get_clocks -nowarn {*_1g25_inst_period*|*_user_clk1_reg*}] {' # a 1.25 Gb/s profile's IP
    echo '  create_generated_clock -name [get_clock_info -name $c] -master_clock [get_clock_info -master_clock $c] \'
    echo '    -source [get_node_info -name [get_clock_info -master_clock_pin $c]] -multiply_by 3125 -divide_by 7776 \'
    echo '    -add [get_node_info -name [get_clock_info -targets $c]]' # took the GPON clock on the shared node, x1: 125 MHz
    echo '}'
    echo 'foreach {side lane} {olt sfp0 onu onu} {'
    echo '  set gpon [get_clocks -nowarn "${p}${side}Side_clocks|*${lane}_inst_period*"]' # 1.25 Gb/s logic: reset while the core clock is GPON
    echo '  set epon_logic [get_keepers -nowarn "${p}${side}Core|epon_* ${p}${side}Core|eponCC|*"]'
    echo '  if {[get_collection_size $gpon] == 0 || [get_collection_size $epon_logic] == 0} {'
    echo '    post_message -type error "fit.sdc: no GPON clock or EPON logic for $side"'
    echo '  } else {'
    echo '    set_false_path -from $gpon -to $epon_logic'
    echo '    set_false_path -from $epon_logic -to $gpon'
    echo '  }'
    echo '}'
    echo 'foreach {gpon epon} {sfp0 sfp0_1g25 onu onu_1g25 ben_tx ben_1g25} {' # every clock of one DR profile (lane, mux, divider) vs the other's
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
echo "project_open $project; create_timing_netlist; read_sdc; update_timing_netlist
foreach_in_collection c [get_available_operating_conditions] { set_operating_conditions \$c; update_timing_netlist
puts \"SETUP [lindex [report_timing -setup -npaths 1] 1]\"; puts \"HOLD [lindex [report_timing -hold -npaths 1] 1]\" }
report_clocks -file clocks.rpt; delete_timing_netlist; project_close" > slack.tcl
[ $top = hsmc_lanes ] || "$QUARTUS/quartus_tlg" --tool dr_synth $project > tlg.log 2>&1 || die "DR logic generation, see $dir/tlg.log"
"$QUARTUS/quartus_syn" $project > syn.log 2>&1 || { grep -E "^Error" syn.log | head -20 >&2; die "synthesis, see $dir/syn.log"; }
"$QUARTUS/quartus_fit" $project > fit.log 2>&1 || { grep -E "^Error" fit.log | head -20 >&2; die "fit, see $dir/fit.log"; }
! grep -m3 "Critical Warning (22304)" fit.log >&2 || die "an SDC file was not applied, see $dir/fit.log"
"$QUARTUS/quartus_sta" -t slack.tcl > sta.log 2>&1 || die "timing, see $dir/sta.log"
! grep -m3 "found without an associated clock assignment" sta.log >&2 || die "unconstrained clock, see $dir/sta.log"
setup=$(sed -n 's/^SETUP //p' sta.log | sort -g | head -1)
hold=$(sed -n 's/^HOLD //p' sta.log | sort -g | head -1)
[ -n "$setup" ] && [ -n "$hold" ] || die "no slack in $dir/sta.log"
summary=$project.fit.summary
[ $top = ghrd ] && summary=output_files/$summary # the GHRD's PROJECT_OUTPUT_DIRECTORY
alm=$(sed -n -E 's/.*Logic utilization \(in ALMs\) *: *([0-9,]+) .*/\1/p' $summary | tr -d ,)
echo "$top fit: $alm ALMs, worst setup slack $setup ns, hold $hold ns"
awk -v s="$setup" -v h="$hold" 'BEGIN { exit !(s >= 0 && h >= 0) }' || die "negative slack, see $dir/sta.log"
