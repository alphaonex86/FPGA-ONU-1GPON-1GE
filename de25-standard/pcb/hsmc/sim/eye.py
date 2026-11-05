# SPDX-License-Identifier: CERN-OHL-S-2.0
"""Run laser_driver.cir in ngspice (from this dir) and print the eye at mid-UI: python3 eye.py laser_driver.cir [Rsn=22 ...]"""
import numpy as np, subprocess, sys, re
# usage: eye.py <netlist> [k=v ...]: replaces 'NAME value' lines' values, runs, reports eye + burst edges
src = open(sys.argv[1]).read()
for kv in sys.argv[2:]:
    k, val = kv.split('=')
    src = re.sub(rf'^({k} \S+ \S+ ).*$', rf'\g<1>{val}', src, flags=re.M)
open('run.cir', 'w').write(src)
subprocess.run(['nice', '-n', '19', 'ngspice', '-b', 'run.cir'], capture_output=True)
d = np.loadtxt('laser.txt'); t, il, ben = d[:, 0], d[:, 1] * 1e3, d[:, 3]
ui = 1 / 1.24416e9
pw = [l.split() for l in open('prbs.pwl')]
pt, pv = np.array([float(a) for a, b in pw]), np.array([float(b) for a, b in pw])
ones, zeros = [], []
for k in range(int(150e-9 / ui), int(1.15e-6 / ui)):
    tc = (k + 0.5) * ui + 0.3e-9        # driver latency (delay scan)
    bit = np.interp((k + 0.5) * ui, pt, pv) > 0
    (ones if bit else zeros).append(np.interp(tc, t, il))
on = (t > 150e-9) & (t < 1.15e-6)
off_ = (t > 1.215e-6) & (t < 3e-6)
print(f"one min {min(ones):5.1f} mean {np.mean(ones):5.1f} | zero max {max(zeros):5.1f} mean {np.mean(zeros):5.1f} | "
      f"peak {il[on].max():5.1f} trough {il[on].min():5.1f} | after burst max {il[off_].max():5.2f} mA")
