#!/bin/sh
# SPDX-License-Identifier: CERN-OHL-S-2.0
# Usage: uio_overlay.sh FIT_DIR F2H_IRQ1_SPI -> FIT_DIR/sniffer-uio.dtbo (FIT_DIR: a "fit.sh DIR ghrd" folder)
set -eu
[ $# -eq 2 ] || { echo "usage: $0 FIT_DIR F2H_IRQ1_SPI" >&2; exit 2; }
dir=$1
[ -f "$dir/pon_irqs.h" ] || { echo "ERROR: $dir/pon_irqs.h missing: run fit.sh $dir ghrd first" >&2; exit 1; }
here=$(cd "$(dirname "$0")" && pwd)
cpp -nostdinc -undef -P -x assembler-with-cpp -I "$dir" -DF2H_IRQ1_SPI="$2" "$here/sniffer-uio.dtso" > "$dir/sniffer-uio.dts"
# the base tree (soc@0: 1 address cell, 1 size cell, GIC parent) is unknown here
dtc -@ -Wno-reg_format -Wno-avoid_default_addr_size -Wno-interrupts_property -I dts -O dtb -o "$dir/sniffer-uio.dtbo" "$dir/sniffer-uio.dts"
echo "$dir/sniffer-uio.dtbo"
