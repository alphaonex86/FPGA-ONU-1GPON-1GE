# SPDX-License-Identifier: CERN-OHL-S-2.0
"""Place the boards (pcbgen.Layout), fan out the BGA, lay the planes, save."""
import os, sys
import pcbnew
sys.path.insert(0, os.path.dirname(__file__))
from pcbgen import Layout, BGA_C, ball_xy, P, V, plane_vias, PLANE_NETS
from paths import board_dir
from boards import onu, olt, hsmc

PASSIVE = ('R', 'C', 'FB', 'L', 'RN', 'TP')
def is_passive(ref):
    return ref.rstrip('0123456789') in PASSIVE

def refs(lay, symname):
    return [r for r, p in lay.parts.items() if p.lib_id.endswith(':' + symname)]

def bga_xy(ball):
    bx, by = ball_xy(ball)
    return BGA_C[0] + bx, BGA_C[1] + by

def common(lay, h):
    u = refs(lay, 'LFE5UM-25F-BG381')[0]
    lay.bga = u
    lay.header(56.13, 3.0)
    lay.place(u, *BGA_C)
    x0, y0, x1, y1 = BGA_C[0] - 9.6, BGA_C[1] - 9.6, BGA_C[0] + 9.6, BGA_C[1] + 9.6
    lay.keepout['B'].append((x0, y0, x1, y1))           # dog-bone via field
    for k, (x, y) in enumerate(((4, h - 4), (60, h - 4))):
        lay.place(refs(lay, 'MountingHole')[k], x, y)
    # regulators on the bottom, east of the BGA; +1V1 leaves the BGA on In2 along y = BGA centre
    b1, b2 = refs(lay, 'TLV62569DBV')
    for u, y in ((b1, 23.0), (b2, 32.5)):
        lay.place(u, 47.5, y, 180, True)
        buck_cell(lay, u)
    lay.place(refs(lay, 'TLV75525PDBV')[0], 54.0, 31.0, 0, True)
    lay.place(refs(lay, 'Conn_01x08')[0], 3.0, 10.0)    # OLT-ONU link, under the header's SPI/I2C pins
    lay.place(refs(lay, 'Conn_01x06')[0], 8.0, 38.0)    # JTAG

def buck_cell(lay, u):
    """TLV62569 (rotated 180, bottom) hot loop: L at SW, Cout at L, Cin at VIN, divider at FB.
    Offsets are for rotation 0, so they are negated."""
    ux, uy = P(lay.fps[u].GetPosition())
    at = lambda dx, dy: (ux - dx, uy - dy)
    east = (-1, 0)
    conns = lambda r: lay.parts[r].conns
    mine = [r for r, p in lay.parts.items() if p.block == lay.parts[u].block and r != u]
    sw, fb = conns(u)['3'], conns(u)['5']
    ind = next(r for r in mine if r.startswith('L'))
    out = conns(ind)['2']
    cout = [r for r in mine if r.startswith('C') and conns(r)['1'] == out]
    cin = next(r for r in mine if r.startswith('C') and conns(r)['1'] == '+5V')
    r_top = next(r for r in mine if r.startswith('R') and conns(r)['1'] == out)
    r_bot = next(r for r in mine if r.startswith('R') and conns(r)['1'] == fb)
    # courtyards just clear (SOT-23-5 2.10 x 1.75, L 1.90 x 1.54, 0805 1.75 x 1.02 half sizes)
    lay.place_pad(ind, '1', *at(-1.14, -3.3), east)
    lay.place_pad(cout[0], '1', *at(2.5, -3.3), east)
    lay.place_pad(cout[1], '1', *at(2.5, -5.4), east)
    lay.place_pad(cin, '1', *at(2.9, -0.95), east)
    lay.place_pad(r_top, '2', *at(2.6, 0.95), east)
    lay.place_pad(r_bot, '1', *at(2.6, 2.1), east)
    lay.track(sw, [at(-1.14, -0.95), at(-1.14, -3.3)], 0.5, pcbnew.B_Cu)
    lay.track(out, [lay.pad_xy(ind, '2'), at(2.5, -3.3), at(2.5, -5.4)], 0.5, pcbnew.B_Cu)
    lay.track('+5V', [at(1.14, -0.95), at(2.9, -0.95)], 0.5, pcbnew.B_Cu)
    lay.track(fb, [at(1.14, 0.95), at(2.6, 0.95), at(2.6, 2.1)], 0.2, pcbnew.B_Cu)

def passives(lay, header_rn_y):
    parts = lay.parts
    placed = set(lay.placed_refs())
    # source series packs between the header and the BGA, under their balls
    for r in sorted(r for r in parts if r.startswith('RN')):
        balls = [lay.ball_of(n) for n in parts[r].conns.values() if n.endswith('_S')]
        xs = [bga_xy(b)[0] for b in balls if b]
        lay.place_near(r, sum(xs) / len(xs), header_rn_y, rot=90)
    rail_balls = {}
    if lay.bga:
        lay.keepout['F'].append((10, 6.5, 54, BGA_C[1] - 9.8))  # bus fan-in corridor, top layer
        # ECP5 rail decoupling: one cap per ball, cycling, on the bottom ring
        for b, n in parts[lay.bga].conns.items():
            if n.startswith('+'):
                rail_balls.setdefault(n, []).append(b)
    used = {}
    pending = sorted((r for r in parts if r not in placed and is_passive(r) and not r.startswith('RN')),
                     key=lambda r: (r.rstrip('0123456789'), int(r.lstrip('ABCDEFGHIJKLMNOPQRSTUVWXYZ'))))
    while pending:
        later = []
        for r in pending:
            nets = [n for n in parts[r].conns.values() if n != 'GND']
            if parts[r].block.startswith('power:'):  # at the regulator pin (or inductor end) it serves
                done = set(lay.placed_refs())
                hosts = [u for u, p in parts.items() if p.block == parts[r].block and u[0] in 'UL' and u != r and u in done]
                pts = [lay.pad_xy(u, pin) for u in hosts for pin, n in parts[u].conns.items() if n in nets]
                if pts:
                    lay.place_near(r, *pts[0], bottom=True)
                else:
                    later.append(r)
                continue
            if nets == ['+1V1'] and lay.cap_for_bga(r):  # on the wide +1V1 island east of the BGA
                lay.place_near(r, BGA_C[0] + 11.0, BGA_C[1], bottom=True, within=ISLAND)
                continue
            if r.startswith('C') and len(nets) == 1 and nets[0] in rail_balls and lay.cap_for_bga(r):
                balls = rail_balls[nets[0]]
                k = used.get(nets[0], 0); used[nets[0]] = k + 1
                lay.place_near(r, *bga_xy(balls[k % len(balls)]), bottom=True)
                continue
            pts = []
            if not lay.bga and len(nets) == 1 and nets[0].startswith('+'):   # decoupling: at the pins of its own block
                pts = [lay.pad_xy(u, pin) for u, p in parts.items() if p.block == parts[r].block and not is_passive(u)
                       and u in placed for pin, n in p.conns.items() if n == nets[0]]
            plane_last = (lambda n: (n in PLANE_NETS, n.startswith('+'))) if not lay.bga else (lambda n: n.startswith('+'))
            for n in sorted(nets, key=plane_last) if not pts else []:
                pts = [xy for ref, xy in lay.pads_of(n) if not is_passive(ref)]
                if pts:
                    break
            if not pts:
                pts = [xy for n in nets for ref, xy in lay.pads_of(n) if n not in ('+3V3', '+5V')]
            if not pts:
                later.append(r)
                continue
            anchor = (sum(p[0] for p in pts) / len(pts), sum(p[1] for p in pts) / len(pts))
            near_bga = any(ref == lay.bga for n in nets if not n.startswith('+') for ref, xy in lay.pads_of(n))
            # BGA pull resistors on top: the bottom edge of the BGA belongs to the decaps
            bottom = lay.bga and ((near_bga and not r.startswith('R')) or lay.on_bottom_net(nets))   # no BGA: all on top
            lay.place_near(r, *anchor, bottom=bottom)
        if later == pending:
            raise RuntimeError(f'no anchor for {later}')
        pending = later

def olt_layout():
    bd = olt()
    lay = Layout(bd, 64, 87.5)
    common(lay, lay.h)
    lay.place(refs(lay, 'SFP_HOST')[0], 29.0, 38.0, 90)
    y155, y125 = refs(lay, 'XO_LVDS_3225')
    lay.place(y155, 42.0, 37.5, 180)    # outputs west to the REFCLK balls, OE east to the expander
    lay.place(y125, 42.0, 41.5, 180)
    lay.place(refs(lay, 'PCA9555PW')[0], 53.0, 47.0)    # central to the SFP, XO, buffers and PROGRAMN it drives
    lay.place(refs(lay, 'PCA9540B')[0], 56.0, 40.0)
    for k, u in enumerate(refs(lay, 'SN74LVC1G125DBV')):
        lay.place(u, 42.0, 47.0 + 4 * k)
    return lay

def onu_layout():
    bd = onu()
    lay = Layout(bd, 64, 66)
    common(lay, lay.h)
    lay.place(refs(lay, 'VCXO_LVDS_3225')[0], 42.0, 37.5)
    lay.place(refs(lay, 'BOSA_GPON')[0], 48.0, 49.5)          # fiber out over the east edge
    lay.place(refs(lay, 'SY88236L')[0], 39.5, 49.5)           # MOD/BIAS/MD side facing the TX can
    lay.place(refs(lay, 'SN65LVDT2DBV')[0], 36.0, 56.0)       # at BEN+ (south-west corner)
    lay.place(refs(lay, 'TPS61391')[0], 60.5, 33.0)           # north-east of the RX can, bottom free (thermal vias)
    lay.place(refs(lay, 'D_Schottky')[0], 60.5, 36.6)
    lay.place(refs(lay, 'AD5593R')[0], 30.0, 58.0)
    return lay


HSMC_W, HSMC_H = 80.0, 78.0

def hsmc_layout():
    """The card hangs east of the DE25-Standard: connector on the bottom along the west edge (bank 1 north, odd row
    west, as on the open Zipper HSMC card), SFP+ cages and the BOSA fiber out of the east edge, bottom empty over the host."""
    lay = Layout(hsmc(), HSMC_W, HSMC_H)
    lay.bga = None
    j = refs(lay, 'HSMC_CARD')[0]
    fp = lay.fps[j]
    lay.pcb.Add(fp)
    fp.SetLayerAndFlip(pcbnew.B_Cu)
    for rot in (0, 90, 180, 270):
        fp.SetOrientationDegrees(rot)
        fp.SetPosition(V(5.8, 39.0))
        p1, p2, p159 = (lay.pad_xy(j, n) for n in ('1', '2', '159'))
        if p1[1] < p159[1] - 50 and p1[0] < p2[0]:
            break
    else:
        raise RuntimeError('HSMC orientation')
    lay.rects['B'].append(lay.rect(fp))
    lay.keepout['B'].append((0, 0, 16, HSMC_H))
    olt, eth = refs(lay, 'SFP_HOST')
    lay.place(olt, HSMC_W - 41.6, 10.0, 180)    # front at the east edge (footprint PCB edge mark at x = -41.6)
    lay.place(eth, HSMC_W - 41.6, 28.6, 180)
    bx, by = HSMC_W - 16.0, 62.0                  # BOSA: fiber over the east edge
    lay.place(refs(lay, 'BOSA_GPON')[0], bx, by)
    lay.place(refs(lay, 'SY88236L')[0], bx - 8.5, by)
    ux, uy = bx - 8.5, by   # RX AC-coupling pair straight north of U1 pins 31/30
    put_pair(lay, [(part_on(lay, 'ONU_RD_P', 'C'), ux - 1.6, uy - 4.3), (part_on(lay, 'ONU_RD_N', 'C'), ux - 0.4, uy - 4.3)], 90)
    lay.place(refs(lay, 'SN65LVDT2DBV')[0], bx - 20.0, by)   # away from U1: its passives need the room
    lay.place(refs(lay, 'TPS61391')[0], bx + 8.5, 48.0)
    lay.place(refs(lay, 'D_Schottky')[0], bx + 8.5, 51.6)
    lay.place(refs(lay, 'AD5593R')[0], 32.0, 70.0)
    lay.place(refs(lay, 'VCXO_LVDS_3225')[0], 14.0, 9.0)    # at XCVR_RX7 (pins 2/4)
    lay.place(refs(lay, 'XO_LVDS_3225')[0], 14.0, 15.0)     # at XCVR_RX6 (pins 6/8)
    lay.place_near(refs(lay, '74LVC1G17')[0], bx - 24.0, by + 10.0)
    lay.place_near(refs(lay, '74LVC1G32')[0], bx - 28.0, by + 10.0)
    lay.place_near(refs(lay, 'D_Schottky')[1], bx - 26.0, by + 13.0)   # burst cutoff discharge diode
    for k, y in enumerate((41.5, 74.5)):
        lay.place(refs(lay, 'MountingHole')[k], HSMC_W - 4.5, y)
    return lay

def hsmc_planes(lay):
    w, h, e = lay.w, lay.h, 0.3
    full = [(e, e), (w - e, e), (w - e, h - e), (e, h - e)]
    lay.zone('GND', pcbnew.In1_Cu, full)
    lay.zone('+3V3', pcbnew.In2_Cu, full)

SERDES_W = 0.35   # ~50 ohm on JLC 7628 prepreg (0.21 mm), P/N at 0.8 mm pitch: loosely coupled 100 ohm diff
NECK_W = 0.1      # between balls

def put_pair(lay, refs_xy, rot):
    """Place 0402 parts of a pair exactly: [(ref, x, y)]."""
    for ref, x, y in refs_xy:
        fp = lay.fps[ref]
        if fp.GetBoard() is not None:
            lay.pcb.Remove(fp)
        lay.place(ref, x, y, rot)

def part_on(lay, net, prefix):
    return next(r for r, p in lay.parts.items() if r.startswith(prefix) and net in p.conns.values())

def pad_of_net(lay, ref, net):
    return next(P(p.GetPosition()) for p in lay.fps[ref].Pads() if p.GetNetname() == net)

def serdes(lay, olt):
    """TX0 leaves through the empty Y4 site, RX0 from the edge row; Y7/Y8 (RX ch1) tie to W7 (GND)."""
    g = lambda b: bga_xy(b)
    w4, w5, y5, y6, y7, y8, w7 = (g(b) for b in ('W4', 'W5', 'Y5', 'Y6', 'Y7', 'Y8', 'W7'))
    ybot = g('Y1')[1] + 1.0       # below the last ball row
    lay.track('GND', [y8, y7, w7], 0.2)
    if olt:
        sfp = refs(lay, 'SFP_HOST')[0]
        tdp, tdn = lay.pad_xy(sfp, '19'), lay.pad_xy(sfp, '18')     # TX0_P -> TD-, TX0_N -> TD+
        rdp, rdn = lay.pad_xy(sfp, '13'), lay.pad_xy(sfp, '12')
        tx_end = (tdp[0], tdp[1] - 1.0), (tdn[0], tdn[1] - 1.0)
        rx_to = (rdp[0], rdp[1] - 1.0), (rdn[0], rdn[1] - 1.0)
        lay.track('RX0_N', [y6, (y6[0], ybot - 0.6)], NECK_W)
        lay.track('RX0_N', [(y6[0], ybot - 0.6), (y6[0], ybot - 0.35), (rx_to[1][0], ybot - 0.35), rx_to[1], rdn], SERDES_W)
        lay.track('RX0_P', [y5, (y5[0], ybot - 0.6)], NECK_W)
        lay.track('RX0_P', [(y5[0], ybot - 0.6), (y5[0], ybot + 0.45), (rx_to[0][0], ybot + 0.45), rx_to[0], rdp], SERDES_W)
    else:
        cp, cn = part_on(lay, 'ONU_TD_P', 'C'), part_on(lay, 'ONU_TD_N', 'C')
        rcp, rcn = part_on(lay, 'ONU_RD_P', 'C'), part_on(lay, 'ONU_RD_N', 'C')
        put_pair(lay, [(cp, w4[0] - 1.2, ybot + 2.4), (cn, w4[0] - 0.2, ybot + 2.4)], 270)
        put_pair(lay, [(rcp, y5[0] + 0.2, ybot + 2.4), (rcn, y6[0] + 0.4, ybot + 2.4)], 90)
        tx_end = pad_of_net(lay, cp, 'TX0_P'), pad_of_net(lay, cn, 'TX0_N')
        for ball, ref, net in ((y5, rcp, 'RX0_P'), (y6, rcn, 'RX0_N')):
            lay.track(net, [ball, (ball[0], ybot - 0.6)], NECK_W)
            lay.track(net, [(ball[0], ybot - 0.6), pad_of_net(lay, ref, net)], SERDES_W)
        # burst enable (TX ch1): W8/W9 leave through the empty Y9/Y10 sites to the 0 ohm links
        w8, w9 = g('W8'), g('W9')
        bp, bn = part_on(lay, 'ONU_BEN_P', 'R'), part_on(lay, 'ONU_BEN_N', 'R')
        put_pair(lay, [(bp, w8[0] + 0.6, ybot + 2.4), (bn, w8[0] + 1.6, ybot + 2.4)], 270)
        bpe, bne = pad_of_net(lay, bp, 'BEN_P'), pad_of_net(lay, bn, 'BEN_N')
        lay.track('BEN_P', [w8, (w8[0] + 0.6, w8[1] + 0.6)], NECK_W)
        lay.track('BEN_P', [(w8[0] + 0.6, w8[1] + 0.6), (w8[0] + 0.6, ybot), bpe], SERDES_W)
        lay.track('BEN_N', [w9, (w9[0] + 0.4, w9[1] + 0.4)], NECK_W)
        lay.track('BEN_N', [(w9[0] + 0.4, w9[1] + 0.4), (w9[0] + 0.4, ybot), bne], SERDES_W)
        optics_pairs(lay)
    # TX0 neck through the Y4 site: P at x-0.2, N at x+0.2 of the site
    site = (w4[0], g('Y5')[1])
    pn = (site[0] - 0.2, w4[1] + 0.2), (site[0] + 0.2, w5[1] + 0.6)
    lay.track('TX0_P', [w4, pn[0], (pn[0][0], ybot), (tx_end[0][0], ybot + 0.6)], NECK_W)
    lay.track('TX0_N', [w5, pn[1], (pn[1][0], ybot + 0.4), (tx_end[1][0], ybot + 0.6)], NECK_W)
    lay.track('TX0_P', [(tx_end[0][0], ybot + 0.6), tx_end[0]], SERDES_W)
    lay.track('TX0_N', [(tx_end[1][0], ybot + 0.6), tx_end[1]], SERDES_W)
    if olt:
        lay.track('TX0_P', [tx_end[0], (tx_end[0][0], tx_end[0][1] + 1.0)], SERDES_W)
        lay.track('TX0_N', [tx_end[1], (tx_end[1][0], tx_end[1][1] + 1.0)], SERDES_W)
    return {'W4', 'W5', 'Y5', 'Y6', 'Y7', 'Y8', 'W8', 'W9'}

def hand_pair(lay, net, pts, neck):
    """F.Cu route, the last `neck` segments at the QFN pin width; its corridor kept free of passives."""
    for k, (a, b) in enumerate(zip(pts, pts[1:])):
        lay.track(net, [a, b], NECK_W * 2 if k >= len(pts) - 1 - neck else SERDES_W)
        lay.keepout['F'].append((min(a[0], b[0]) - 0.45, min(a[1], b[1]) - 0.45, max(a[0], b[0]) + 0.45, max(a[1], b[1]) + 0.45))

def optics_pairs(lay):
    """SerDes caps -> SY88236L: TD to DIN on its west side (P/N swapped there), RD from ROUT on its north side,
    1 mm apart and 45 degree corners."""
    u = refs(lay, 'SY88236L')[0]
    pin = lambda n: lay.pad_xy(u, n)
    cap = lambda net: pad_of_net(lay, part_on(lay, net, 'C'), net)
    tp, tn, a, b = cap('ONU_TD_P'), cap('ONU_TD_N'), pin('7'), pin('6')
    hand_pair(lay, 'ONU_TD_P', [tp, (tp[0], a[1] - 1.0), (tp[0] + 1.0, a[1]), (a[0] - 0.6, a[1]), a], 1)
    hand_pair(lay, 'ONU_TD_N', [tn, (tn[0], a[1] - 2.0), (tn[0] + 1.0, a[1] - 1.0), (b[0] - 1.3, a[1] - 1.0), (b[0] - 0.8, b[1]), b], 2)
    rp, rn, c, d = cap('ONU_RD_P'), cap('ONU_RD_N'), pin('31'), pin('30')
    yp, yn = rp[1] + 1.5, rn[1] + 1.1
    hand_pair(lay, 'ONU_RD_P', [rp, (rp[0], yp), (rp[0] + c[1] - 1.56 - yp, c[1] - 1.56), (c[0], c[1] - 1.56), c], 1)
    hand_pair(lay, 'ONU_RD_N', [rn, (rn[0], yn), (rn[0] + c[1] - 2.56 - yn, c[1] - 2.56), (d[0], c[1] - 2.56), d], 1)

def escape(lay, net, steps, layer):
    """A ball boxed in by its neighbours' dogbones: from its (south-west) dogbone via, relative
    steps on one layer through free channels to a via outside the ball field."""
    cx, cy = BGA_C
    bx, by = ball_xy(lay.ball_of(net))
    assert bx < 0 < by, f'{net}: escape expects a south-west ball'
    pts = [(cx + bx - 0.4, cy + by + 0.4)]
    for dx, dy in steps:
        pts.append((round(pts[-1][0] + dx, 3), round(pts[-1][1] + dy, 3)))
    lay.track(net, pts, 0.1, layer)
    lay.via(net, *pts[-1])

ISLAND = (BGA_C[0] + 9.8 + 0.5, BGA_C[1] - 11.0 + 0.5, BGA_C[0] + 26.0 - 0.5, BGA_C[1] + 2.5 - 0.5)  # wide +1V1, 0.5 mm in

def planes(lay):
    w, h, e = lay.w, lay.h, 0.3
    full = [(e, e), (w - e, e), (w - e, h - e), (e, h - e)]
    cx, cy = BGA_C
    lay.zone('GND', pcbnew.In1_Cu, full)
    lay.zone('+3V3', pcbnew.In2_Cu, full)
    core = 2.75
    # core, a neck along the via-free centre line, then wide once out of the via field (decaps + buck output)
    lay.zone('+1V1', pcbnew.In2_Cu, [(cx - core, cy - core), (cx + core, cy - core), (cx + core, cy - 0.55),
                                     (cx + 9.8, cy - 0.55), (ISLAND[0] - 0.5, ISLAND[1] - 0.5), (ISLAND[2] + 0.5, ISLAND[1] - 0.5),
                                     (ISLAND[2] + 0.5, ISLAND[3] + 0.5), (ISLAND[0] - 0.5, ISLAND[3] + 0.5), (cx + 9.8, cy + 0.55),
                                     (cx + core, cy + 0.55), (cx + core, cy + core), (cx - core, cy + core)],
             priority=1, clearance=0.2, min_width=0.15)
    lay.no_vias([(cx + core, cy - 0.55), (cx + 9.8, cy - 0.55), (cx + 9.8, cy + 0.55), (cx + core, cy + 0.55)])
    # outer GND pours come after routing (finish.py): poured copper would wall the router in


if __name__ == '__main__':
    force = '--force' in sys.argv
    names = [a for a in sys.argv[1:] if not a.startswith('-')] or ['olt', 'onu', 'hsmc']
    for make in (olt_layout, onu_layout, hsmc_layout):
        if make.__name__.split('_')[0] not in names:
            continue
        lay = make()
        target = os.path.join(board_dir(lay.bd.name), lay.bd.name + '.kicad_pcb')
        if os.path.exists(target) and not force:  # a routed board is the source from then on
            sys.exit(f'{target} exists: routing would be lost; --force to regenerate')
        if not lay.bga:
            passives(lay, None)
            hsmc_planes(lay)
            added, missed = plane_vias(lay)
            print('plane vias', added, 'NO ROOM for', missed if missed else 'none')
            lay.outline()
            print(lay.bd.name, 'placed', len(lay.placed_refs()), 'of', len(lay.parts))
            print(lay.save())
            continue
        hand = serdes(lay, lay.bd.name == 'olt')   # its exact-placed pair parts first, the passives fill around
        passives(lay, 13.0)
        lay.fanout(hand | {'Y11', 'Y12'} | {b for b in lay.bus_balls() if b[0] == 'A'}, PLANE_NETS)
        for net in ('CCLK', 'MISO'):  # row channels U/V, V/W west on top (U1-2, V1, W1 unused), out to x = centre - 9
            bx, _ = ball_xy(lay.ball_of(net))
            escape(lay, net, [(-9.0 - bx + 0.4, 0)], pcbnew.F_Cu)
        if lay.bd.name == 'olt':  # PROGRAMN goes east: under the field along its own pad row (empty on B.Cu)
            bx, _ = ball_xy(lay.ball_of('OLT_PROGRAMN'))
            escape(lay, 'OLT_PROGRAMN', [(0.4, -0.4), (9.0 - bx, 0)], pcbnew.B_Cu)
        for net in ('TDO', 'TCK'):  # back to its pad site, west along its (V, T) row line on B.Cu
            bx, _ = ball_xy(lay.ball_of(net))
            escape(lay, net, [(-0.4, -0.4), (-8.6 - bx, 0)], pcbnew.B_Cu)
        planes(lay)
        added, missed = plane_vias(lay)
        print('plane vias', added, 'NO ROOM for', missed if missed else 'none')
        lay.outline()
        print(lay.bd.name, 'placed', len(lay.placed_refs()), 'of', len(lay.parts))
        print(lay.save())
