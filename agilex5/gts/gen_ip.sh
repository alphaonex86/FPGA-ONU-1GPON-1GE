#!/bin/bash
# SPDX-License-Identifier: CERN-OHL-S-2.0
# GTS Direct PHY variants of the HSMC card lanes (PMA clocking, raw parallel data) into DIR.
# Usage: gen_ip.sh DIR
set -euo pipefail
QSYS=${QSYS:-$HOME/altera_pro/26.1.1/qsys/bin}
PART=${PART:-A5ED013BB32AE4SCS}
[ $# -eq 1 ] || { echo "usage: $0 DIR" >&2; exit 1; }
mkdir -p "$1"; cd "$1"
PON=155.520000   # VCXO pin: every PON lane (CDRs need a PON-family refclk, the VCXO tracks the ISP OLT)
ETH=156.250000   # XO pin: Ethernet (1.25 Gb/s cannot come from 155.52 MHz, TX or CDR)
# single width: one word per core clock (311.04 MHz PON, 125 MHz Ethernet), as the MACs take it
COMMON="clocking_mode=xcvr pmaif_tx_fifo_mode_s=register pmaif_rx_fifo_mode_s=register
        pldif_tx_clkout_sel=TX_WORD_CLK pldif_rx_clkout_sel=RX_WORD_CLK pldif_tx_clkout_div=1 pldif_rx_clkout_div=1
        pldif_tx_double_width_transfer_enable=0 pldif_rx_double_width_transfer_enable=0 rx_adaptation_mode=manual"
SWAP=rx_invert_pin=ENABLE   # RX pairs P/N swapped on the HSMC card (HSMC_SWAP): inverted back in the PMA
deploy() {   # COMPONENT NAME PARAM=VALUE...
    local component=$1 name=$2; shift 2
    local args=()
    for p in "$@"; do args+=("--component-parameter=$p"); done
    "$QSYS/ip-deploy" --family="Agilex 5" --part="$PART" --component-name="$component" --output-name="$name" \
        "${args[@]}" > "$name.log" 2>&1 || true
    if grep -q "Error" "$name.log" || [ ! -f "$name.ip" ]; then grep "Error" "$name.log" | cut -c1-300 >&2; echo "ERROR: $name" >&2; exit 1; fi
    "$QSYS/qsys-generate" "$name.ip" --synthesis=VERILOG --family="Agilex 5" --part="$PART" >> "$name.log" 2>&1 ||
        { echo "ERROR: $name generation, see $1/$name.log" >&2; exit 1; }
    echo "$name ok"
}
gen() { local name=$1; shift; deploy intel_directphy_gts "$name" $COMMON "$@"; }
deploy intel_srcss_gts srcss NUM_LANES_SHORELINE=4   # one reset request/grant per lane
# PON core clocks 311.04 / 155.52 MHz from the OLT tx_clkout: the clock network divider keeps them aligned and timed
# together (the GTS tx_clkout2 is asynchronous to tx_clkout in the IP constraints)
deploy intelclkctrl pon_clocks CLOCK_DIVIDER=1 CLOCK_DIVIDER_OUTPUTS=2
# VCXO loop helper clock (VcxoTracker DDMTD): 50 MHz x 87 / 28 = 155.357 MHz, 1.05e-3 below the halved 311.04
deploy altera_iopll helper_pll gui_reference_clock_frequency=50.0 gui_number_of_clocks=1 gui_output_clock_frequency0=155.357143 \
    gui_operation_mode=direct gui_use_locked=true
# Every PON lane is duplex at 2488.32: the 1244.16 Gb/s direction is 2x oversampled (TX bits doubled, RX every
# other bit), same waveform on the fiber, no dual simplex group (Quartus refuses TX and RX variants on one channel).
# The OLT lane's tx_clkout, through pon_clocks, clocks every PON lane's core.
gen olt duplex_mode=duplex pma_data_rate=2488.32 pma_width=8 tx_pll_refclk_freq_mhz=$PON rx_pll_refclk_freq_mhz=$PON enable_port_rx_set_locktoref=1 $SWAP
gen onu duplex_mode=duplex pma_data_rate=2488.32 pma_width=8 tx_pll_refclk_freq_mhz=$PON rx_pll_refclk_freq_mhz=$PON $SWAP
gen ben_tx duplex_mode=tx pma_data_rate=2488.32 pma_width=8 tx_pll_refclk_freq_mhz=$PON rx_pll_refclk_freq_mhz=$PON
gen eth duplex_mode=duplex pma_data_rate=1250 pma_width=10 tx_pll_refclk_freq_mhz=$ETH rx_pll_refclk_freq_mhz=$ETH $SWAP
