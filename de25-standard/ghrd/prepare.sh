#!/bin/bash
# SPDX-License-Identifier: CERN-OHL-S-2.0
# Bring the Terasic DE25-Standard GHRD (HPS reference design, Terasic license: stays outside this repo) into DIR for
# fit.sh's ghrd top. Once (or when this script or qsys_top.tcl change): copy it, upgrade its IP to this Quartus (it
# is a 25.1 design; two values out of the new ranges set as in Altera's Agilex 5 GHRD 26.x), trim it for PonBoard
# and add its lightweight windows (qsys_top.tcl), generate qsys_top and keep the resulting qsf. Every time: a fresh
# golden_top.v and that qsf for fit.sh to patch. Usage: prepare.sh GHRD_DIR DIR
set -euo pipefail
QUARTUS=${QUARTUS:-$HOME/altera_pro/26.1.1/quartus/bin}
QSYS=${QSYS:-$QUARTUS/../../qsys/bin}
die() { echo "ERROR: $*" >&2; exit 1; }
[ $# -eq 2 ] || die "usage: $0 GHRD_DIR DIR"
here=$(cd "$(dirname "$0")" && pwd)
ghrd=$(cd "$1" && pwd)
dir=$(cd "$2" && pwd)
[ -f "$ghrd/golden_top.qpf" ] && [ -f "$ghrd/qsys_top.qsys" ] || die "$ghrd is not the DE25-Standard GHRD"
cd "$dir"
cp "$ghrd/golden_top.v" .
prepared=golden_top.qsf.prepared # with the IP files the upgrade and qsys_top.tcl added
if ! [ "$prepared" -nt "$here/qsys_top.tcl" ] || ! [ "$prepared" -nt "$here/prepare.sh" ]; then
rsync -a --exclude output_files --exclude golden_top.v "$ghrd/" ./
python3 -I - <<'EOF' || die "GHRD IP parameters not as expected"
import re
def set_value(path, parameter, old, new):
    text = open(path, newline="").read()
    pattern = r'(parameterId="%s"[^\n]*\n(?:[^\n]*\n){0,3}?\s*<ipxact:value>)%s(</ipxact:value>)' % (parameter, old)
    text, n = re.subn(pattern, r"\g<1>%s\g<2>" % new, text)
    if n != 1:
        raise SystemExit(f"{path}: {parameter} = {old} found {n} times")
    open(path, "w", newline="").write(text)
set_value("hps_subsys/ip/qsys_top/emif_io96b_hps.ip", "CTRL_PERFORMANCE_PROFILE", "default", "SEQ")
for k in range(3):
    set_value("hps_subsys/ip/hps_subsys/agilex_hps.ip", f"Periph_clk_emac{k}_sel", "50", "250")
EOF
"$QUARTUS/quartus_sh" --ip_upgrade -mode all golden_top > ghrd_upgrade.log 2>&1 ||
    die "IP upgrade, see $dir/ghrd_upgrade.log"
"$QSYS/qsys-script" --script="$here/qsys_top.tcl" --quartus-project=golden_top.qpf > ghrd_qsys.log 2>&1 ||
    die "lightweight windows, see $dir/ghrd_qsys.log"
"$QSYS/qsys-generate" qsys_top.qsys --synthesis=VERILOG --quartus-project=golden_top.qpf > ghrd_generate.log 2>&1 ||
    die "qsys_top generation, see $dir/ghrd_generate.log"
cp golden_top.qsf "$prepared"
fi
cp "$prepared" golden_top.qsf
