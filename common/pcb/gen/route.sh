#!/bin/sh
# SPDX-License-Identifier: CERN-OHL-S-2.0
# Route the generated boards: freerouting, then KRT on what is left, then finish.py (outer pours, DRC).
# Usage: common/pcb/gen/route.sh [olt] [onu]     (routers: the FPGAcommon/hw wrappers, HW=<dir> to override)
set -eu
GEN=$(cd "$(dirname "$0")" && pwd)
HW=${HW:-$GEN/../../../../FPGAcommon/hw}
for b in ${@:-olt onu}; do
    dir=$(python3 "$GEN/paths.py" "$b")
    work=$(mktemp -d)
    cp "$dir/$b.kicad_pcb" "$dir/$b.kicad_pro" "$work/"
    (cd "$work" && nice -n 19 python3 "$HW/autoroute.py" "$b.kicad_pcb" --passes ${PASSES:-20} --iterations ${ITERATIONS:-4} \
        --work "$work/fr" > "$work/fr.log" 2>&1) || true
    grep -E 'result|wrote|not accepting' "$work/fr.log" || echo "$b: freerouting printed no result"
    if grep -q 'not accepting' "$work/fr.log"; then
        rejected=$(ls "$work"/fr/it*.kicad_pcb | sort -V | tail -1)
        python3 "$GEN/salvage.py" "$rejected" "$work/$b.kicad_pcb"
    fi
    (cd "$work" && nice -n 19 python3 "$HW/krt_route.py" "$b.kicad_pcb" --layers F.Cu B.Cu --track-width 0.1 --rip --open-bga --unfill --apply | grep -E 'result|landed|NOT an') || echo "$b: KRT printed no result"
    rc=0; python3 "$GEN/finish.py" "$work/$b.kicad_pcb" || rc=$?
    cp "$work/$b.kicad_pcb" "$dir/$b.kicad_pcb"
    rm -r "$work"
    [ $rc -eq 0 ] || echo "$b: not clean, see the DRC in KiCad"
done
