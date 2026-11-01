#!/bin/bash
# SPDX-License-Identifier: CERN-OHL-S-2.0
# Fit hsmc_lanes.sv on the DE25-Standard pins: proves each GTS lane reaches its rate from its refclk pin.
# Usage: fit.sh DIR   (IP variants from gen_ip.sh in DIR/ip)
set -euo pipefail
QUARTUS=${QUARTUS:-$HOME/altera_pro/26.1.1/quartus/bin}
[ $# -eq 1 ] || { echo "usage: $0 DIR" >&2; exit 1; }
src=$(cd "$(dirname "$0")" && pwd)
[ -f "$1/ip/eth.ip" ] && [ -f "$1/ip/srcss.ip" ] || "$src/gen_ip.sh" "$1/ip"
cd "$1"
{
    echo 'set_global_assignment -name FAMILY "Agilex 5"'
    echo 'set_global_assignment -name DEVICE A5ED013BB32AE4SCS'
    echo 'set_global_assignment -name TOP_LEVEL_ENTITY hsmc_lanes'
    echo "set_global_assignment -name SYSTEMVERILOG_FILE $src/hsmc_lanes.sv"
    for ip in srcss olt onu ben_tx eth; do echo "set_global_assignment -name IP_FILE ip/$ip.ip"; done
    # DE25-Standard user manual, HSMC GTS table
    echo 'set_location_assignment PIN_AT120 -to pon_refclk'
    echo 'set_location_assignment PIN_AP120 -to eth_refclk'
    for k in 0 1 2 3; do echo "set_location_assignment PIN_$(echo AU AR AN AL | cut -d' ' -f$((k + 1)))129 -to tx_p[$k]"; done
    for k in 1 2 3; do echo "set_location_assignment PIN_$(echo AT AP AM AK | cut -d' ' -f$((k + 1)))135 -to rx_p[$k]"; done
    echo 'set_instance_assignment -name VIRTUAL_PIN ON -to reset'
    echo 'set_instance_assignment -name VIRTUAL_PIN ON -to status[*]'
} > hsmc_lanes.qsf
"$QUARTUS/quartus_syn" hsmc_lanes > syn.log 2>&1 || { grep -E "^Error" syn.log | head -20 >&2; echo "ERROR: synthesis, see $1/syn.log" >&2; exit 1; }
"$QUARTUS/quartus_fit" hsmc_lanes > fit.log 2>&1 || { grep -E "^Error" fit.log | head -20 >&2; echo "ERROR: fit, see $1/fit.log" >&2; exit 1; }
echo "fit ok: every lane placed on its pin with its refclk"
