#!/usr/bin/env python3
# SPDX-License-Identifier: CERN-OHL-S-2.0
# Closed-loop model of VcxoLoop (common/hw/spinal/pon/sniffer/SnifferBridge.scala): update every 2^14 clocks of
# 124.416 MHz, FIFO phase detector saturating at +-8 words (1-word quantization), duty -> two RC poles at 16 Hz ->
# VCXO 100 ppm over the duty range (Kv also /3 and x3).
import math
F = 124.416e6; T = 2**14 / F; KV = 100e-6
def run(f0_ppm, kp=32, ki=512, kv=KV, seconds=120):
    sub = 64; dt = T / sub; a = 1 - math.exp(-dt * 2 * math.pi * 16)
    integral = 0x8000 << 16; duty = 0x8000; v1 = v2 = 0.5; phase = 0.0; locked_at = None; worst = 0
    for n in range(int(seconds / T)):
        for _ in range(sub):
            v1 += a * (duty / 65536 - v1); v2 += a * (v1 - v2)
            f_ref = f0_ppm * 1e-6 + kv * (v2 - 0.5)          # ref relative to the downstream
            phase = max(-8, min(8, phase - f_ref * F * dt))  # ds faster than ref: fill grows
        e = max(-8, min(8, round(phase)))
        integral = max(0, min(0xFFFF << 16, integral + e * ki))
        duty = max(0, min(0xFFFF, (integral >> 16) + kp * e))
        t = n * T
        if abs(phase) < 2:
            if locked_at is None: locked_at = t
        elif t > 1 and locked_at is not None and t - locked_at < 5: locked_at = None
        if locked_at is not None and t > locked_at + 5: worst = max(worst, abs(phase))
    return locked_at, worst, duty
for f0 in (-50, -10, 10, 50):
    for kv in (KV, KV / 3, KV * 3):
        lock, worst, duty = run(f0, kv=kv)
        print(f"start {f0:+d} ppm, Kv {kv*1e6:.0f} ppm/range: lock {('%.1f s' % lock) if lock else 'NEVER'}, "
              f"|phase| after lock <= {worst:.2f} words, duty 0x{duty:04X}")
