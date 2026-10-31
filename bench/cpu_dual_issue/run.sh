#!/bin/bash
# SPDX-License-Identifier: CERN-OHL-S-2.0
# VexiiRiscv single vs dual issue: CoreMark/Dhrystone per MHz (Verilator sim),
# ALM/M20K/Fmax and vectorless core dynamic power (Quartus Pro, DE25-Nano FPGA).
set -euo pipefail

REF=${REF:-/mnt/data/perso/FPGA/projects/FPGA-ONU-ref}
OUT=${OUT:-$(cd "$(dirname "$0")" && pwd)/out}
FIT_PERIOD_NS=3.0     # same push for both variants; Fmax comes from the timing report
POWER_PERIOD_NS=10.0  # power compared at 100 MHz
MEMORY_LATENCY=15     # cycles, ~LPDDR4 at ~150 MHz
CORE="--xlen=32 --with-mul --with-div --with-rva --with-rvc --with-supervisor --with-user
 --allow-bypass-from=0 --with-fetch-l1 --with-lsu-l1 --fetch-l1-ways=2 --lsu-l1-ways=2
 --with-btb --with-gshare --with-ras --performance-counters=0"
declare -A VARIANT=([single]="" [dual]="--dual-issue")

VEXII=$REF/VexiiRiscv
SBT=$REF/tools/sbt/bin/sbt
ELF=$VEXII/ext/NaxSoftware/baremetal

die() { echo "ERROR: $*" >&2; exit 1; }
# pick FILE REGEX: first capture group of REGEX in FILE, fail if absent
pick() { local v; v=$(sed -n -E "s/$2/\1/p" "$1" | head -1); [ -n "$v" ] || die "'$2' not found in $1"; echo "$v"; }
sbt_run() { (cd "$VEXII" && "$SBT" -batch "Test/runMain $1") > "$2" 2>&1 || die "sbt failed, see $2"; }

simulate() { # variant bench elf
    local log=$OUT/$1/sim_$2.log
    sbt_run "vexiiriscv.tester.TestBench $CORE ${VARIANT[$1]} --memory-latency=$MEMORY_LATENCY \
--no-rvls-check --print-stats --name $1_$2 --load-elf $3" "$log"
    grep -q "^\[success\]" "$log" || die "simulation $1/$2 did not pass, see $log"
}

generate() { # variant
    rm -f "$VEXII/VexiiRiscv.v"
    sbt_run "vexiiriscv.Generate $CORE ${VARIANT[$1]}" "$OUT/$1/generate.log"
    mv "$VEXII/VexiiRiscv.v" "$OUT/$1/VexiiRiscv.v" || die "generator wrote no VexiiRiscv.v"
}

quartus() { # variant
    "$(dirname "$0")/../quartus_core.sh" "$OUT/$1/VexiiRiscv.v" VexiiRiscv "$OUT/$1/quartus" \
        "$FIT_PERIOD_NS" "$POWER_PERIOD_NS" > /dev/null
}

for v in single dual; do
    mkdir -p "$OUT/$v"
    echo "== $v: simulate"
    simulate "$v" dhrystone "$ELF/dhrystone_vexii/build/rv32imac/dhrystone_vexii.elf"
    simulate "$v" coremark "$ELF/coremark_vexii/build/rv32imac/coremark_vexii.elf"
    echo "== $v: generate + Quartus"
    generate "$v"
    quartus "$v"
done

printf "\n%-7s %8s %9s %6s %7s %5s %8s %9s %7s %11s\n" variant DMIPS/MHz CM/MHz IPC ALM M20K Fmax CM@Fmax "mW@100M" "CM/mW@100M"
for v in single dual; do
    d=$OUT/$v
    dmips=$(pick "$d/sim_dhrystone.log" '.*DMIPS per Mhz: *([0-9.]+).*')
    cm=$(pick "$d/sim_coremark.log" '.* ([0-9.]+) Coremark\/MHz.*')
    ipc=$(pick "$d/sim_coremark.log" '.*IPC *: *[0-9]+ \/ *[0-9]+ *([0-9.]+)%.*')
    read -r alm m20k fmax mw < "$d/quartus/summary"
    printf "%-7s %8s %9s %5s%% %7s %5s %8s %9.0f %7.0f %11.3f\n" "$v" "$dmips" "$cm" "$ipc" "$alm" "$m20k" "$fmax" \
        "$(echo "$cm * $fmax" | bc -l)" "$mw" "$(echo "$cm * 1000 / ($POWER_PERIOD_NS * $mw)" | bc -l)"
done
