# SPDX-License-Identifier: CERN-OHL-S-2.0
"""Place the boards (pcbgen.Layout), fan out the BGA, lay the planes, save."""
import math, os, sys
import pcbnew
sys.path.insert(0, os.path.dirname(__file__))
from pcbgen import Layout, BGA_C, P, V, plane_vias, PLANE_NETS
import paths
from paths import board_dir
from boards import onu, olt, hsmc, sniffer, inline, ball_xy
import bosa

PASSIVE = ('R', 'C', 'FB', 'L', 'RN', 'TP')
def is_passive(ref):
    return ref.rstrip('0123456789') in PASSIVE

def refs(lay, symname):
    return [r for r, p in lay.parts.items() if p.lib_id.endswith(':' + symname)]

def bga_xy(ball):   # ECP5 boards (BGA unrotated at BGA_C)
    bx, by = ball_xy(ball)
    return BGA_C[0] + bx, BGA_C[1] + by

def island(field):
    """Core plane island east of a BGA whose dog-bone field ends `field` mm from its centre, 0.5 mm in."""
    cx, cy = BGA_C
    return (cx + field + 0.5, cy - 11.0 + 0.5, cx + 26.0 - 0.5, cy + 2.5 - 0.5)

def common(lay, h):
    u = refs(lay, 'LFE5UM-25F-BG381')[0]
    lay.bga, lay.island = u, island(9.8)
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
    for link in refs(lay, 'Conn_01x08'):                 # OLT-ONU link, under the header's SPI/I2C pins
        lay.place(link, 3.0, 10.0)
    lay.place(refs(lay, 'Conn_01x06')[0], 8.0, 38.0)    # JTAG

def bosa_ports(lay):
    """The BOSAs by their receiver's net prefix (DS_ / US_)."""
    out_p = next(pad for pad, lead in bosa.LEADS.items() if lead == 'OUT+')
    return {lay.parts[r].conns[out_p][:3]: r for r in refs(lay, 'BOSA_GPON')}

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

def keep_places(lay):
    """The passives the cells did not place go back where the routed board has them, when footprint and nets are the
    same and the place is still free: the automatic search below answers to every part around it, so one new chip
    moved parts (and lost their routing) all over the board."""
    name = lay.bd.name
    path = paths.ROUTED.get(name, f'{board_dir(name)}/{name}.kicad_pcb')
    if not os.path.exists(path):
        return
    pads = lambda fp: (fp.GetFPID().GetUniStringLibItemName(), {p.GetNumber(): p.GetNetname() for p in fp.Pads()})
    done = set(lay.placed_refs())
    for old in pcbnew.LoadBoard(path).GetFootprints():
        ref = old.GetReference()
        if ref in lay.fps and ref not in done and is_passive(ref) and pads(old) == pads(lay.fps[ref]):
            lay.place_if_free(ref, pcbnew.VECTOR2I(old.GetPosition()), old.GetOrientationDegrees(), old.IsFlipped())

def passives(lay, header_rn_y):
    parts = lay.parts
    keep_places(lay)
    placed = set(lay.placed_refs())
    # source series packs between the header and the BGA, under their balls
    for r in sorted(r for r in parts if r.startswith('RN')):
        balls = [lay.ball_of(n) for n in parts[r].conns.values() if n.endswith('_S')]
        xs = [lay.pad_xy(lay.bga, b)[0] for b in balls if b]
        lay.place_near(r, sum(xs) / len(xs), header_rn_y, rot=90)
    rail_balls = {}
    if lay.bga:
        if header_rn_y:
            lay.keepout['F'].append((10, 6.5, 54, BGA_C[1] - 9.8))  # bus fan-in corridor, top layer
        # ECP5 rail decoupling: one cap per ball, cycling, on the bottom ring
        for b, n in parts[lay.bga].conns.items():
            if n.startswith('+'):
                rail_balls.setdefault(n, []).append(b)
    used, order = {}, {r: k for k, r in enumerate(parts)}
    pending = sorted((r for r in parts if r not in placed and is_passive(r) and not r.startswith('RN')),
                     key=lambda r: (r.rstrip('0123456789'), int(r.lstrip('ABCDEFGHIJKLMNOPQRSTUVWXYZ'))))
    while pending:
        later = []
        for r in pending:
            nets = [n for n in parts[r].conns.values() if n != 'GND']
            if parts[r].block.startswith('power:'):  # at the regulator pin (or inductor end) it serves
                done = set(lay.placed_refs())
                hosts = [u for u, p in parts.items() if p.block == parts[r].block and u[0] in 'UL' and u != r and u in done]
                pts = [(u, lay.pad_xy(u, pin)) for u in hosts for pin, n in parts[u].conns.items() if n in nets]
                if pts:
                    u, xy = pts[0]
                    lay.place_near(r, *xy, bottom=lay.fps[u].IsFlipped())   # the side of the part it serves
                else:
                    later.append(r)
                continue
            if nets == [lay.core] and lay.cap_for_bga(r):  # on the wide core island east of the BGA
                try:
                    lay.place_near(r, lay.island[0] + 0.7, BGA_C[1], bottom=True, within=lay.island)
                except RuntimeError:   # bottom full: on top at the island's far end, clear of the lanes leaving the balls
                    lay.place_near(r, lay.island[2], BGA_C[1], within=lay.island)
                continue
            if r.startswith('C') and len(nets) == 1 and nets[0] in rail_balls and lay.cap_for_bga(r):
                balls = rail_balls[nets[0]]
                k = used.get(nets[0], 0); used[nets[0]] = k + 1
                lay.place_near(r, *lay.pad_xy(lay.bga, balls[k % len(balls)]), bottom=True, outside=lay.island)   # its own plane under it
                continue
            pts, host = [], None
            if len(nets) == 1 and nets[0].startswith('+') and not (lay.bga and lay.cap_for_bga(r)):
                # decoupling: at the chip of its block it was declared after (a block may hold several chips)
                chips = [u for u, p in parts.items() if p.block == parts[r].block and not is_passive(u) and u in placed
                         and nets[0] in p.conns.values()]
                host = next((u for u in reversed(chips) if order[u] < order[r]), chips[0] if chips else None)
                pts = [lay.pad_xy(host, pin) for pin, n in parts[host].conns.items() if n == nets[0]] if host else []
            # signals first; between two rails the one with fewer pads: a ferrite sits at its load, not at the plane's centroid
            plane_last = ((lambda n: (n in PLANE_NETS, n.startswith('+'))) if not lay.bga else
                          (lambda n: (n.startswith('+'), len(lay.pads_of(n)) if n.startswith('+') else 0)))
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
            lay.place_near(r, *anchor, bottom=lay.fps[host].IsFlipped() if host else bottom)
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

def sniffer_layout():
    """Each BOSA at the east edge with its fibre (and the body of the XL-BCB2111) over the edge, its limiting
    amp beside it; the CML pairs come back to the SerDes balls south of the BGA."""
    bd = sniffer()
    lay = Layout(bd, 64, 72)
    common(lay, lay.h)
    lay.place(refs(lay, 'VCXO_LVDS_3225')[0], 42.0, 37.5)       # at the REFCLK balls Y11/Y12, as on the ONU board
    la = {lay.parts[r].conns['1'][:3]: r for r in refs(lay, 'SY88349NDL')}
    # each limiting amp right at its RX can: the TIA's millivolts stay short, the CML pairs run to the BGA
    ports = bosa_ports(lay)
    lay.place(ports['DS_'], 49.0, 57.0)                          # downstream: body to the east edge, fiber over it
    lay.place(ports['US_'], 59.0, 43.0)                          # upstream: RX can 5 mm from the east edge
    lay.place(la['DS_'], 56.5, 50.0)                             # north of the X100DG RX can (57.5, 57)
    lay.place(la['US_'], 51.0, 43.0)
    tps = {lay.parts[r].conns['5'][:3]: r for r in refs(lay, 'TPS61391')}   # pin 5 = APD output
    diodes = {lay.parts[r].conns['1'][:3]: r for r in refs(lay, 'D_Schottky')}
    for p, (x, y) in (('DS_', (38.0, 63.0)), ('US_', (60.5, 33.0))):   # each next to its BOSA
        lay.place(tps[p], x, y)
        lay.place(diodes[p], x, y + 3.6)
    lay.place(refs(lay, 'AD5593R')[0], 22.0, 64.0)
    return lay

def inline_layout():
    """Compact: USB and its ULPI PHY north-west of the BGA, the flash socket and the logic south of it, both optical
    ports east (OLT port north, ONU port south): each BOSA across the east edge, TX can at the edge, RX can inside with
    its limiting amp in a ring of shield clips. The fibre leaves the body's RX end and passes above the low parts west
    of the RX can (bosa.BOOT): nothing tall there. The Artix-7 turned so its ball A1 is north-east: the
    GTP quad (rows A-F) faces the ports, the ULPI bus (rows Y-AB, columns 1-8) the north-west, the config flash bank
    (columns 19-22) the south-west."""
    w, h = 84, 54
    lay = Layout(inline(), w, h)
    lay.ring, lay.gap = 0.8, 0.15   # dense board: the routers need the channels around each chip
    one = lambda sym: refs(lay, sym)[0]
    by_net = lambda sym, pin: {lay.parts[r].conns[pin][:3]: r for r in refs(lay, sym)}
    lay.bga, lay.core, lay.pitch, lay.island = one('XC7A35T-FGG484'), '+1V0', 1.0, island(11.8)
    lay.place(lay.bga, *BGA_C, 270)
    a1 = lay.pad_xy(lay.bga, 'A1')
    assert a1[0] > BGA_C[0] and a1[1] < BGA_C[1], f'ball A1 at {a1}: not north-east'
    lay.keepout['B'].append((BGA_C[0] - 11.5, BGA_C[1] - 11.5, BGA_C[0] + 11.5, BGA_C[1] + 11.5))   # dog-bone via field
    bucks = {lay.parts[r].block: r for r in refs(lay, 'TLV62569DBV')}
    # cells clear of the via field; +1V0 east of the SuperSpeed receive pair's way to its balls
    for rail, xy in (('+1V0', (51.0, 23.0)), ('+1V8', (49.5, 32.5)), ('+3V3', (38.0, 41.5))):
        lay.place(bucks[f'power:{rail}'], *xy, 180, True)
        buck_cell(lay, bucks[f'power:{rail}'])
    lay.place(one('TLV75510PDBV'), 56.0, 31.0, 0, True)    # GTP supplies from +1V8
    lay.place(one('TLV75512PDBV'), 56.0, 35.0, 0, True)
    lay.place(one('USB_C_Receptacle'), 52.5, 4.73, 180)              # mouth 0.5 mm over the north edge, GTP side
    lay.place(one('USBLC6-2SC6'), 45.0, 3.0, 0, True)                 # under the oscillator, where D+ / D- leave west
    lay.place(one('USB3320'), 13.0, 16.0, 180)                        # ULPI pins 1-8 face bank 34, USB pins the west edge
    pins_joined(lay, one('USB3320'), '+1V8_USB')
    lay.place(one('W25Q32JVSS'), 9.0, 34.5)                           # the flash socket, hand reach from the west edge
    lay.place(one('Conn_01x06'), 2.5, 21.5)                           # JTAG pads (not fitted), west of its balls
    lay.place(one('LED'), 14.0, 47.5)
    xo = {lay.parts[r].conns['4']: r for r in refs(lay, 'XO_LVDS_7050')}   # by output net
    lay.place(xo['XO_OUT_P'], 43.6, 8.5)                              # 155.52 MHz, north of the REFCLK balls F6/E6
    lay.place(xo['XO125_OUT_P'], 34.5, 6.5)                           # 125 MHz (USB 3)
    for hole, xy in zip(refs(lay, 'MountingHole'), ((4, 4), (w - 4, 25.6), (4, h - 4), (w - 4, h - 4))):   # east: between the ports
        lay.place(hole, *xy)
    ports = bosa_ports(lay)   # DS_ = OLT port
    drivers = by_net('MAX3643', '2')
    tps = by_net('TPS61391', '5')
    amp = {'DS_': one('SY88993AV'), 'US_': one('SY88349NDL')}
    clips = iter(refs(lay, 'SHIELD_CLIP'))
    rx_x = w - 5.0 - bosa.TX_X
    # y: the BOSA's axis; turn: its APD boost's SW pin east and the row south into the gap between the ports (the pairs to
    # the FPGA pass west of it), or SW west and the row north (the board ends south of it)
    for rx, tx, y, turn in (('DS_', 'US_', 17.0, 270), ('US_', 'DS_', 42.0, 90)):
        lay.place(ports[rx], rx_x, y)
        lay.place(amp[rx], rx_x - 4.0, y - 8.5, 180)      # north-west of the can: inputs east at its leads A / B, outputs west
        tia_pair(lay, amp[rx], ports[rx], rx, rx_x, y - 6.5)
        quiet_leads(lay, ports[rx])
        for t, dx in (('P', -0.2), ('N', -1.2)):          # AC caps to a top-escaped lane: on top, inside the can, toward
            cap = next((r for r, p in lay.parts.items()   # its open south side (the clips wall the other three)
                        if r.startswith('C') and f'{rx}RX_{t}' in p.conns.values()), None)
            if cap:
                pad = next(n for n, net in lay.parts[cap].conns.items() if net == f'{rx}LA_{t}')
                lay.place_pad(cap, pad, pad_of_net(lay, amp[rx], f'{rx}LA_{t}')[0] + dx, y - 8.5 + 2.2, (0, 1), bottom=False)
        pecl_outputs(lay, amp[rx], rx)
        for cx, cy, rot in ((rx_x - 4.0, y - 12.8, 0), (rx_x - 8.9, y - 8.5, 90), (rx_x + 2.9, y - 8.5, 90)):
            lay.place(next(clips), cx, cy, rot)
        lay.place(drivers[tx], w - 5.0, y - 8.0, 270, True)   # bottom, on the TX can's axis: outputs (pins 13-18) face its leads
        laser_cell(lay, drivers[tx], tx, ports[rx])
        lay.place(tps[rx], rx_x - 5.0, y + 7.5, turn, True)  # APD bias south-west of the RX can, on the bottom with its parts
        boost_cell(lay, tps[rx], rx)
    gate, cut = sorted(refs(lay, '74LVC1G97'), key=lambda r: lay.parts[r].block != 'gate')
    chain = (one('74LVC1G07'), gate, one('74LVC1G123'), cut)   # the upstream laser's gate and cutoff: on top, over the bucks
    for k, r in enumerate(chain):
        lay.place(r, 47.5, 37.0 + 3.4 * k)
    lay.place(one('74LVC1G32'), 52.0, 47.0)
    lay.place(one('TLV3501AIDBV'), rx_x - 10.5, 49.5)    # at the ONU port's APD monitor
    dacs = {lay.parts[r].conns['2']: r for r in refs(lay, 'MCP4728')}   # by clock: SCL feeds the OLT port's
    lay.place(dacs['SCL'], 52.9, 2.6, 0, True)   # under the USB receptacle's mouth, beside the OLT port it sets
    lay.place(dacs['SCL2'], 23.0, 46.5)
    lay.place(one('ADS7828'), 33.0, 44.5)
    usb3_cell(lay)
    return lay

def pins_joined(lay, chip, net):
    """A chip's pins of one supply with other pins between them: a via outside each, joined on the bottom."""
    cx, cy = P(lay.fps[chip].GetPosition())
    vias = []
    for q in (q for q in lay.fps[chip].Pads() if q.GetNetname() == net):
        (x, y), _ = P(q.GetPosition()), lay.tied.add((chip, q.GetNumber()))
        out = (x, y + math.copysign(0.85, y - cy)) if abs(y - cy) > abs(x - cx) else (x + math.copysign(0.85, x - cx), y)
        lay.track(net, [(x, y), out], 0.25)
        lay.via(net, *out)
        keep_free(lay, 'F', [(x, y), out], 0.3)
        vias.append(out)
    lay.track(net, vias, 0.3, pcbnew.B_Cu)
    keep_free(lay, 'B', vias, 0.45)

def tia_pair(lay, la, port, p, x, y):
    """The receiver's millivolt pair, on top inside the can, around the axis x: from the amplifier's east pins P runs
    inside and N around it, 3 mm apart from the row y on. Row y: the two terminations in line between the legs, their
    middle the reference (the pair is ended across itself, whatever the reference's own path); next row the coupling
    capacitors and, between them, the burst receiver's offset resistors, grounded at the BOSA's ground lead; then each
    leg straight on to its lead. A reference pin between the inputs is wired inside the pair with its capacitor;
    outside them it crosses the N leg on the bottom."""
    def part(prefix, a, b):
        return next((r for r, q in lay.parts.items() if r.startswith(prefix) and set(q.conns.values()) == {a, b}), None)
    pad = lambda r, net: next(n for n, v in lay.parts[r].conns.items() if v == net)
    w, vref, half, cap_y = lay.bd.serdes_w, f'{p}VREF', 1.5, y + 1.0
    pins = {t: next(q for q in lay.fps[la].Pads() if q.GetNetname() == f'{p}DIN_{t}') for t in 'PN'}
    pin_y = {t: P(q.GetPosition())[1] for t, q in pins.items()}
    east = max(P(q.GetBoundingBox().GetEnd())[0] for q in pins.values())
    middle, ground = {}, []
    for t, side, run in (('P', -1, east + 0.3), ('N', 1, x + half)):
        din, tia, cx = f'{p}DIN_{t}', f'{p}TIA_{t}', x + side * half
        cap, term, offset = part('C', din, tia), part('R', din, vref), part('R', din, 'GND')
        lay.place_pad(term, pad(term, din), cx, y, (-side, 0), bottom=False)
        lay.place_pad(cap, pad(cap, din), cx, cap_y, (0, 1), bottom=False)
        lay.track(din, [(east - 0.1, pin_y[t]), (run, pin_y[t]), (run, y - abs(run - cx)), (cx, y), (cx, cap_y)], w)
        middle[t] = pad_of_net(lay, term, vref)
        if offset:
            lay.place_pad(offset, pad(offset, din), x + side * 0.5, cap_y, (0, 1), bottom=False)
            lay.track(din, [(cx, cap_y), (x + side * 0.5, cap_y)], w)
            ground.append(pad_of_net(lay, offset, 'GND'))
            lay.tied.add((offset, pad(offset, 'GND')))
        (lx, ly), start = pad_of_net(lay, port, tia), pad_of_net(lay, cap, tia)
        north = ly - 0.7   # each leg enters its lead from the north: the ground lead stands between the two
        lay.track(tia, [start, (cx, north - abs(lx - cx)), (lx, north), (lx, ly)], w)
    if ground:
        lead = min((P(q.GetPosition()) for q in lay.fps[port].Pads() if q.GetNetname() == 'GND'), key=lambda g: abs(g[0] - x) + abs(g[1] - y))
        lay.track('GND', [ground[0], (lead[0], ground[0][1]), ground[1]], 0.25)
        lay.track('GND', [(lead[0], ground[0][1]), lead], 0.25)
    vx, vy = pad_of_net(lay, la, vref)
    lay.track(vref, [middle['P'], (x, y), middle['N']], 0.2)
    if min(pin_y.values()) < vy < max(pin_y.values()):
        cap = part('C', vref, 'GND')
        lay.place_pad(cap, pad(cap, vref), middle['P'][0], vy, (1, 0), bottom=False)
        lay.track(vref, [(east - 0.1, vy), (middle['P'][0], vy), middle['P']], 0.2)
    else:
        lay.track(vref, [(east - 0.1, vy), (x, vy)], 0.2)
        lay.track(vref, [(x, y), (x, y - 0.8)], 0.2)
        lay.track(vref, [(x, vy), (x, y - 0.8)], 0.2, pcbnew.B_Cu)
        lay.via(vref, x, vy)
        lay.via(vref, x, y - 0.8)
        keep_free(lay, 'B', [(x, vy), (x, y - 0.8)], 0.45)
    top = min(vy, *pin_y.values()) - 0.45   # nothing else inside the pair: it could not be reached on top
    lay.keepout['F'] += [(east, top, x + half + 0.3, y), (x - half - 0.3, y, x + half + 0.3, pad_of_net(lay, port, f'{p}TIA_P')[1])]

def pecl_outputs(lay, la, p):
    """A DC-coupled limiting amplifier's outputs, on top inside the can west of its pins: each series resistor in line
    with its pin, the emitter load beside it (P south, N north), and a via each down to the bottom: the legs leave alike."""
    def part(a, b):
        return next((r for r, q in lay.parts.items() if r.startswith('R') and set(q.conns.values()) == {a, b}), None)
    pad = lambda r, net: next(n for n, v in lay.parts[r].conns.items() if v == net)
    for t, away in (('P', 1), ('N', -1)):
        out, rx = f'{p}LA_{t}', f'{p}RX_{t}'
        series, load = part(out, rx), part(out, 'GND')
        if not series:
            return
        x, y = pad_of_net(lay, la, out)
        lay.place_pad(series, pad(series, out), x - 1.15, y, (-1, 0), bottom=False)
        lay.place_pad(load, pad(load, out), x - 1.15, y + away, (0, away), bottom=False)
        vx, vy = pad_of_net(lay, series, rx)
        lay.track(out, [(x, y), (x - 1.15, y), (x - 1.15, y + away)], lay.bd.serdes_w)
        lay.track(rx, [(vx, vy), (vx, vy + 0.75 * away)], lay.bd.serdes_w)
        lay.via(rx, vx, vy + 0.75 * away)
        lay.rects['B'].append((vx - 0.45, vy + 0.75 * away - 0.45, vx + 0.45, vy + 0.75 * away + 0.45))   # no bottom part on the via

def boost_cell(lay, chip, p):
    """The APD boost's switching node swings tens of volts beside a receiver: the inductor straight out of the SW pin,
    then the diode and the output capacitor in a row along the chip's side, away from its other pins (left to the
    placer the node was 12 mm long). The CAP capacitor at its pin, which its neighbours' tracks box in; the mirror's
    series resistor at the MONIN pin (behind the inductor its 0.2 mm clearance found no way to the pin); the input
    capacitor at the inductor's other end."""
    sw, out, cap_net, monin, rail = f'{p}HV_SW', f'{p}HV_OUT', f'{p}APD_CAP', f'{p}HV_MONIN', f'+3V3_{p}APD'
    pad = lambda r, net: next(n for n, v in lay.parts[r].conns.items() if v == net)
    (sx, sy), (gx, gy), (cx, cy) = lay.pad_xy(chip, '8'), lay.pad_xy(chip, '7'), P(lay.fps[chip].GetPosition())
    ax, ay = round((sx - gx) / 0.5), round((sy - gy) / 0.5)                    # along the pin row, from pin 7 to SW
    ox, oy = (0, 1 if sy > cy else -1) if ax else (1 if sx > cx else -1, 0)    # out of the chip
    at = lambda x, y, away, along: (x + ox * away + ax * along, y + oy * away + ay * along)
    coil, diode = part_on(lay, sw, 'L'), part_on(lay, sw, 'D')
    cout = next(r for r, q in lay.parts.items() if r.startswith('C') and set(q.conns.values()) == {out, 'GND'})
    lay.place_pad(coil, pad(coil, sw), *at(sx, sy, 1.75, 1.05), (ox, oy))
    lay.place_pad(diode, pad(diode, sw), *at(sx, sy, 1.75, 3.2), (ax, ay))
    lay.place_pad(cout, pad(cout, out), *at(sx, sy, 1.75, 6.7), (ax, ay))
    lay.track(sw, [(sx, sy), at(sx, sy, 1.75, 0), at(sx, sy, 1.75, 3.2)], 0.3, pcbnew.B_Cu)
    lay.track(out, [at(sx, sy, 1.75, 5.4), at(sx, sy, 1.75, 6.7)], 0.2, pcbnew.B_Cu)
    lay.track('GND', [(gx, gy), at(gx, gy, -0.7, 0)], 0.2, pcbnew.B_Cu)   # pin 7 into the thermal pad: the cell takes its via's place
    lay.tied.add((chip, '7'))
    px, py = pad_of_net(lay, chip, cap_net)          # the next side's first pin: out of the chip there is `along` here
    cap = part_on(lay, cap_net, 'C')
    lay.place_pad(cap, pad(cap, cap_net), *at(px, py, 0.8, 1.25), (ax, ay))
    lay.track(cap_net, [(px, py), at(px, py, 0, 0.45), at(px, py, 0.8, 1.25)], 0.2, pcbnew.B_Cu)
    mx, my = pad_of_net(lay, chip, monin)
    series = part_on(lay, monin, 'R')
    lay.place_pad(series, pad(series, monin), *at(mx, my, 1.15, 0), (-ax, -ay))
    lay.track(monin, [(mx, my), at(mx, my, 1.15, 0)], 0.2, pcbnew.B_Cu)
    cin = next(r for r, q in lay.parts.items() if q.value == '10u' and set(q.conns.values()) == {rail, 'GND'})
    lay.place_pad(cin, pad(cin, rail), *at(sx, sy, 5.25, 1.05), (ax, ay))
    lay.track(rail, [at(sx, sy, 3.55, 1.05), at(sx, sy, 5.25, 1.05)], 0.3, pcbnew.B_Cu)

def quiet_leads(lay, port, margin=2.0, slot=0.5):
    """The RX can's leads carry its microamps, and no can covers their ends on the bottom: within `margin` of them no
    foreign track on either side (a laser's current passed at 2 mm on the bottom, a laser's data pair between two
    leads on top; over the planes 0.2 mm away, 2 mm is ten heights). Bottom: no track at all. Top: the pair leaves
    north inside its cell, the supply and APD leads south through two slots, laid here; the rest is closed. The
    outer ground pours fill both sides."""
    at = {lead: lay.pad_xy(port, lead) for lead in 'ABCDE'}
    x0, x1 = min(x for x, _ in at.values()) - margin, max(x for x, _ in at.values()) + margin
    y0, y1 = min(y for _, y in at.values()) - margin, max(y for _, y in at.values()) + margin
    lay.rule_area([(x0, y0), (x1, y0), (x1, y1), (x0, y1)], pcbnew.LSET().AddLayer(pcbnew.B_Cu), tracks=False)
    lay.keepout['B'].append((x0, y0, x1, y1))
    (dx, dy), (ex, _) = at['D'], at['E']
    west, east, south = at['A'][0] - 1.0, at['C'][0] + 1.0, dy + 1.0
    for poly in ([(x0, y0), (west, y0), (west, south), (dx - slot, south), (dx - slot, y1), (x0, y1)],
                 [(dx + slot, south), (ex - slot, south), (ex - slot, y1), (dx + slot, y1)],
                 [(east, y0), (x1, y0), (x1, y1), (ex + slot, y1), (ex + slot, south), (east, south)]):
        lay.rule_area(poly, pcbnew.LSET().AddLayer(pcbnew.F_Cu), tracks=False)
    for lead, width in (('D', 0.3), ('E', 0.2)):
        x, y = at[lead]
        lay.track(lay.parts[port].conns[lead], [(x, y), (x, y1 + 0.3)], width)

def laser_cell(lay, driver, p, port):
    """The laser current's parts between the driver's output pins and the TX can: the series resistors in a row south
    of their pins, each dummy-load diode beside its resistor; the input pair's termination north of its pins; the three
    current-setting resistors at their pins (IMAX east, MODSET / BIASSET west). Their tracks are laid here: straight,
    and clear of the via each VCC pin between them needs."""
    def wire(net, r, x, y, toward, jog):
        pin = pad_of_net(lay, driver, net)
        lay.place_pad(r, next(n for n, v in lay.parts[r].conns.items() if v == net), x, y, toward)
        lay.track(net, [pin, (pin[0], y - jog), (x, y)], lay.bd.serdes_w, pcbnew.B_Cu)
    for pin, dx, dy, toward in (('IMAX', 1.5, 0, (0, 1)), ('MODSET', -1.0, 0.2, (-1, 0)), ('BIASSET', -1.0, -0.2, (-1, 0))):
        net = f'{p}LD_{pin}'
        (x, y), r = pad_of_net(lay, driver, net), part_on(lay, net, 'R')
        lay.place_pad(r, next(n for n, v in lay.parts[r].conns.items() if v == net), x + dx, y + dy, toward)
        lay.track(net, [(x, y), (x + dx - math.copysign(abs(dy), dx), y), (x + dx, y + dy)], 0.1, pcbnew.B_Cu)
    term, (x, y) = part_on(lay, f'{p}LD_IN_P', 'R'), pad_of_net(lay, driver, f'{p}LD_IN_P')
    wire(f'{p}LD_IN_P', term, x - 0.26, y - 2.0, (1, 0), -0.6)
    lay.track(f'{p}LD_IN_N', [(x + 0.5, y), (x + 0.5, y - 1.4), pad_of_net(lay, term, f'{p}LD_IN_N')], lay.bd.serdes_w, pcbnew.B_Cu)
    row = lay.pad_xy(driver, '16')[1] + 1.5
    for pin, dx in (('OUT_P', 0), ('BIAS_P', 0), ('OUT_N', -0.5), ('BIAS_N', 0.5)):
        net = f'{p}LD_{pin}'
        x = pad_of_net(lay, driver, net)[0] + dx
        wire(net, part_on(lay, net, 'R'), x, row, (0, 1), 0.5)
        if dx:
            lay.place_near(part_on(lay, f'{net}_D', 'D'), x + 3.2 * dx, row + 1.0, bottom=True, rot=90)
    # at the TX can, away from the RX can: the anode's feed (resistor and inductor side by side) west of its lead,
    # the monitor diode's load east of its lead
    part = lambda prefix, a, b: next((r for r, q in lay.parts.items() if r.startswith(prefix) and set(q.conns.values()) == {a, b}), None)
    pad = lambda r, net: next(n for n, v in lay.parts[r].conns.items() if v == net)
    anode, monitor, vcc = f'{p}LD_A', f'{p}MPD_A', f'+3V3_{p}LD'
    (ax, ay), (mx, my) = pad_of_net(lay, port, anode), pad_of_net(lay, port, monitor)
    feed = [part('R', vcc, anode), part('L', vcc, anode)]
    for r, dy in zip(feed, (-0.5, 0.5)):
        lay.place_pad(r, pad(r, anode), ax - 2.7, ay + dy, (-1, 0))
    lay.track(anode, [(ax, ay), (ax - 2.7, ay)], 0.3, pcbnew.B_Cu)
    lay.track(anode, [(ax - 2.7, ay - 0.5), (ax - 2.7, ay + 0.5)], 0.3, pcbnew.B_Cu)
    lay.track(vcc, [pad_of_net(lay, r, vcc) for r in feed], 0.3, pcbnew.B_Cu)
    load, cap = part('R', monitor, 'GND'), part('C', monitor, 'GND')
    lay.place_pad(load, pad(load, monitor), mx + 1.7, my - 1.1, (0, -1))
    if cap:
        lay.place_pad(cap, pad(cap, monitor), mx + 1.7, my, (0, 1))
    lay.track(monitor, [(mx, my), (mx + 1.7, my), (mx + 1.7, my - 1.1)], 0.2, pcbnew.B_Cu)

def sniffer_serdes(lay):
    """RX0 (Y5/Y6, downstream) and RX1 (Y7/Y8, upstream) straight south to their AC caps; TX balls stay open."""
    ybot = bga_xy('Y1')[1] + 1.0
    caps = []
    for ball, net, dx in (('Y5', 'DS_RX_P', 0.2), ('Y6', 'DS_RX_N', 0.4), ('Y7', 'US_RX_P', 0.6), ('Y8', 'US_RX_N', 0.8)):
        xy = bga_xy(ball)
        caps.append((part_on(lay, net, 'C'), xy[0] + dx, ybot + 2.4))
    put_pair(lay, caps, 90)
    for ball, net in (('Y5', 'DS_RX_P'), ('Y6', 'DS_RX_N'), ('Y7', 'US_RX_P'), ('Y8', 'US_RX_N')):
        xy = bga_xy(ball)
        lay.track(net, [xy, (xy[0], ybot - 0.6)], NECK_W)
        lay.track(net, [(xy[0], ybot - 0.6), pad_of_net(lay, part_on(lay, net, 'C'), net)], SERDES_W)
    return {'Y5', 'Y6', 'Y7', 'Y8'}

HSMC_W, HSMC_H, HSMC_HOST = 80.0, 79.0, (16.0, 78.0)   # the host overlap strip ends at y = 78: south of it, the corner is cut

def hsmc_layout():
    """The card hangs east of the DE25-Standard: connector on the bottom along the west edge (bank 1 north, odd row
    west, as on the open Zipper HSMC card), SFP+ cages and the BOSA fiber out of the east edge, bottom empty over the host."""
    lay = Layout(hsmc(), HSMC_W, HSMC_H)
    lay.notch = HSMC_HOST
    lay.keepout['F'].append((0, HSMC_HOST[1], HSMC_HOST[0], HSMC_H))
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
    lay.keepout['B'].append((0, 0, HSMC_HOST[0], HSMC_H))
    cage0, cage1 = refs(lay, 'SFP_HOST')
    lay.place(cage0, HSMC_W - 41.6, 10.0, 180)    # front at the east edge (footprint PCB edge mark at x = -41.6)
    lay.place(cage1, HSMC_W - 41.6, 28.6, 180)
    bx, by = HSMC_W - 16.0, 62.0                  # BOSA: fiber over the east edge
    lay.place(refs(lay, 'BOSA_GPON')[0], bx, by)
    lay.place(refs(lay, 'SY88236L')[0], bx - 8.5, by)
    ux, uy = bx - 8.5, by   # RX AC-coupling pair straight north of U1 pins 31/30
    put_pair(lay, [(part_on(lay, 'ONU_RD_P', 'C'), ux - 1.6, uy - 4.3), (part_on(lay, 'ONU_RD_N', 'C'), ux - 0.4, uy - 4.3)], 90)
    lay.place(refs(lay, 'TPS61391')[0], bx + 8.5, 48.0)
    lay.place(refs(lay, 'D_Schottky')[0], bx + 8.5, 51.6)
    lay.place(refs(lay, 'AD5593R')[0], 32.0, 70.0)
    vcxo = {'VCXO156_P' in lay.parts[r].conns.values(): r for r in refs(lay, 'VCXO_LVDS_3225')}
    lay.place(vcxo[False], 14.0, 9.0)    # 155.52 MHz at XCVR_RX7 (pins 2/4)
    lay.place(vcxo[True], 14.0, 15.0)    # 156.25 MHz at XCVR_RX6 (pins 6/8)
    lay.place(refs(lay, 'TPS563201')[0], 46.0, 40.5)   # 12 V buck for the cages, under cage 1
    hsmc_pairs(lay)
    # cage 1 parts on +3V3_SFP: under the cage, on that plane; beside the cage pins they fall across the ONU RX pair
    for r, p in lay.parts.items():
        if p.block == 'sfp:SFP1' and '+3V3_SFP' in p.conns.values():
            lay.place_near(r, 60.0, 40.5, within=(52.0, 37.2, 72.0, 43.8))
    lay.place_near(refs(lay, '74LVC1G123')[0], bx - 24.0, by + 10.0)
    lay.place_near(refs(lay, '74LVC1G97')[0], bx - 28.0, by + 10.0)
    lay.place_near(refs(lay, '74LVC1G32')[0], bx - 32.0, by + 10.0)
    for k, y in enumerate((41.5, 70.0)):   # the second clear of the BOSA courtyard
        lay.place(refs(lay, 'MountingHole')[k], HSMC_W - 4.5, y)
    return lay

def hsmc_planes(lay):
    w, h, e = lay.w, lay.h, 0.3
    full = [(e, e), (w - e, e), (w - e, h - e), (e, h - e)]
    lay.zone('GND', pcbnew.In1_Cu, full)
    lay.zone('+3V3', pcbnew.In2_Cu, full)
    # cages (~2.2 A): east of where the cage RX pairs leave B.Cu and clear of ONU RX (B.Cu), so no pair crosses
    # the plane split
    lay.zone('+3V3_SFP', pcbnew.In2_Cu, [(27.0, e), (w - e, e), (w - e, 47.0), (48.5, 47.0), (27.0, 25.5)], priority=1)

SERDES_W = 0.35   # ~50 ohm on JLC 7628 prepreg (0.21 mm), P/N at 0.8 mm pitch: loosely coupled 100 ohm diff
NECK_W = 0.1      # between balls

PAIR_PITCH = 0.8

def left_offset(pts, d):
    """The 45/90 degree polyline moved d to its left (y down), corners mitred."""
    def left(a, b):
        dx, dy = b[0] - a[0], b[1] - a[1]
        n = math.hypot(dx, dy)
        return dy / n, -dx / n
    out = []
    for i, (x, y) in enumerate(pts):
        ns = ([left(pts[i - 1], pts[i])] if i else []) + ([left(pts[i], pts[i + 1])] if i < len(pts) - 1 else [])
        (ax, ay), (bx, by) = ns[0], ns[-1]
        k = d / (1 + ax * bx + ay * by) if len(ns) == 2 else d / 2
        out.append((round(x + k * (ax + bx), 4), round(y + k * (ay + by), 4)))
    return out

def keep_free(lay, side, pts, margin=0.5):
    """A route's corridor kept free of parts, in <= 1 mm pieces so a diagonal does not block its bounding box."""
    for a, b in zip(pts, pts[1:]):
        n = max(1, math.ceil(math.hypot(b[0] - a[0], b[1] - a[1])))
        for k in range(n):
            p, q = ((a[0] + (b[0] - a[0]) * t / n, a[1] + (b[1] - a[1]) * t / n) for t in (k, k + 1))
            lay.keepout[side].append((min(p[0], q[0]) - margin, min(p[1], q[1]) - margin,
                                      max(p[0], q[0]) + margin, max(p[1], q[1]) + margin))

def route_pair(lay, layer, center, ends, width=SERDES_W, pitch=PAIR_PITCH):
    """Locked pair along `center`, `width` at `pitch`; ends: (net, head, tail) for the left line, then the right one,
    head and tail being the thin (pad pitch) pieces to the pads or vias."""
    side = 'F' if layer == pcbnew.F_Cu else 'B'
    for (net, head, tail), d in zip(ends, (pitch / 2, -pitch / 2)):
        line = left_offset(center, d)
        for pts, w in ((head + line[:1], 2 * NECK_W), (line, width), (line[-1:] + tail, 2 * NECK_W)):
            if len(pts) > 1:
                lay.track(net, pts, w, layer, locked=True)
                keep_free(lay, side, pts)

SS_W, SS_PITCH = 0.3, 0.5   # USB 3 pairs: about 90 ohm differential on the outer layers (JLC 7628 prepreg, 0.21 mm)

def left_first(seg, a, b):
    """Two (net, xy) targets ordered for route_pair: the one left of a pair running along `seg` first."""
    (x0, y0), (x1, y1) = seg
    return sorted((a, b), key=lambda q: -((q[1][0] - x1) * (y1 - y0) - (q[1][1] - y1) * (x1 - x0)))

def turn_to(lay, ref, ok):
    """The rotation at which `ok(pad)` holds, pad(number) being that pad's place around the part's origin."""
    fp = lay.fps[ref]
    fp.SetPosition(V(0, 0))
    for rot in (0, 90, 180, 270):
        fp.SetOrientationDegrees(rot)
        if ok(lambda n: lay.pad_xy(ref, n)):
            return rot
    raise RuntimeError(f'{ref}: no rotation fits')

def usb3_cell(lay):
    """SuperSpeed between the USB-C receptacle (north edge, pad rows A / B south of its body) and GTP lane 2, its pairs
    laid here at SS_W / SS_PITCH. On top, going south: row A's pairs, the two ESD arrays (one per plug orientation),
    the switch, the transmit pair with its capacitors to balls B6 / A6. Row B is under the body: its pairs go down by
    vias north of it and come up beside the arrays. The receive pair dives at the switch and runs on the bottom to
    balls B10 / A10, under the downstream receive pair that leaves the balls between the two on top. Every path has two
    vias or none. D-, CC1 and CC2 leave on the bottom (the pairs wall row A in), D+ and VBUS north under the body."""
    j, mux, comparator = (refs(lay, s)[0] for s in ('USB_C_Receptacle', 'HD3SS3212', 'LMV331'))
    F, B = pcbnew.F_Cu, pcbnew.B_Cu
    x0 = P(lay.fps[j].GetPosition())[0]
    ya, yb = lay.pad_xy(j, 'A1')[1], lay.pad_xy(j, 'B1')[1]
    y_esd, ym = ya + 3.35, ya + 6.55
    at = lambda ref, net: pad_of_net(lay, ref, net)
    mid = lambda ref, pair: (at(ref, pair[0])[0] + at(ref, pair[1])[0]) / 2
    def wire(net, pts, width=0.2, layer=F):
        lay.track(net, pts, width, layer, locked=True)
        keep_free(lay, 'F' if layer == F else 'B', pts, 0.3)
    def via(net, xy):
        lay.via(net, *xy, locked=True)
        for side in 'FB':
            keep_free(lay, side, [xy, xy], 0.4)
    def pair(layer, center, a, b):
        """A pair toward the targets a and b: (net, xy, head, tail)."""
        ends = left_first(center[-2:], a[:2], b[:2])
        full = {q[0]: q for q in (a, b)}
        route_pair(lay, layer, center, [(net, full[net][2], full[net][3]) for net, _ in ends], SS_W, SS_PITCH)

    lay.place(mux, x0, ym, turn_to(lay, mux, lambda pad: pad('19')[0] < pad('12')[0] and pad('19')[1] < pad('3')[1]))
    lay.keepout['B'].append(lay.rect(lay.fps[mux], -0.2))   # no part under it: a plane via would land in its pads
    for k, sx in (('2', -1), ('1', 1)):   # lane, its side of the receptacle
        esd = next(r for r in refs(lay, 'TPD4E05U06') if f'USB_TX{k}_P' in lay.parts[r].conns.values())
        lay.place(esd, x0 + sx * 1.55, y_esd, turn_to(lay, esd, lambda pad: pad('1')[0] < pad('5')[0] - 1))
        ground = (lay.pad_xy(esd, '3')[0], y_esd - 1.15)   # its ground: a via north of it, between the pairs
        wire('GND', [ground, (ground[0], y_esd + 0.385)]); via('GND', ground)
        lay.tied |= {(esd, '3'), (esd, '8')}
        for d in 'TR':
            nets = [f'USB_{d}X{k}_{t}' for t in 'PN']
            ca, cm = mid(esd, nets), mid(mux, nets)
            down = [(ca, y_esd - 0.6), (ca, y_esd + 0.75), (cm, y_esd + 0.75 + abs(cm - ca)), (cm, ym - 1.188)]
            pins = [(n, at(mux, n)) for n in nets]
            if abs(at(j, nets[0])[1] - ya) < 0.01:   # row A: on top all the way
                cj = mid(j, nets)
                pair(F, [(cj, ya), (cj, ya + 0.65), (ca, ya + 0.65 + abs(ca - cj))] + down, *((n, xy, [], []) for n, xy in pins))
                continue
            outer, inner = sorted(nets, key=lambda n: -sx * at(j, n)[0])
            drop = {outer: (at(j, outer)[0] + sx * 0.4, yb - 1.15), inner: (at(j, inner)[0] + sx * 0.1, yb - 1.15)}
            rise = {outer: (ca + sx * 0.5, ya + 1.75), inner: (ca - sx * 0.3, ya + 1.75)}   # clear of row A's pair
            for n, jog in ((outer, 0.4), (inner, 0.1)):
                x = at(j, n)[0]
                wire(n, [(x, yb), (x, yb - 0.6), (x + sx * jog, yb - 0.6 - jog), drop[n]])
                via(n, drop[n]); via(n, rise[n])
            cv = (drop[outer][0] + drop[inner][0]) / 2
            pair(B, [(cv, yb - 0.9), (cv, yb - 0.3), (ca, yb - 0.3 + abs(ca - cv)), (ca, ya + 1.35)],
                 *((n, rise[n], [drop[n]], [rise[n]]) for n in nets))
            pair(F, down, *((n, xy, [rise[n]], []) for n, xy in pins))

    # the receptacle's other pads: rows joined where the same signal faces itself; D- crosses D+ on the bottom
    pos = lambda n: lay.pad_xy(j, n)
    net = lambda n: next(q.GetNetname() for q in lay.fps[j].Pads() if q.GetNumber() == n)
    for b, a in (('B1', 'A12'), ('B4', 'A9'), ('B6', 'A6'), ('B9', 'A4'), ('B12', 'A1')):
        assert net(a) == net(b), (a, b)
        late = abs(pos(b)[0] - x0) < abs(pos(a)[0] - x0)   # the long piece on the pad nearer the middle: clear of the peg holes
        wire(net(a), [pos(b), (pos(b)[0], yb + (1.1 if late else 0.7)), (pos(a)[0], yb + (1.35 if late else 0.95)), pos(a)])
    y_bus = yb - 2.65
    ferrite = next(r for r, q in lay.parts.items() if r.startswith('FB') and 'VBUS' in q.conns.values())
    lay.place_pad(ferrite, next(n for n, v in lay.parts[ferrite].conns.items() if v == 'VBUS'), x0 - 6.3, y_bus - 2.6, (-1, 0), bottom=False)
    for b in ('B4', 'B9'):
        wire('VBUS', [pos(b), (pos(b)[0], y_bus)], 0.2)
    wire('VBUS', [(pos('B9')[0], y_bus), (x0 - 3.7, y_bus), (x0 - 6.3, y_bus - 2.6)], 0.4)
    dp, dm, cc1, cc2 = (x0, yb - 1.85), (pos('B7')[0], yb - 1.15), (pos('A5')[0] - 0.25, ya + 1.0), (pos('B5')[0], yb - 1.15)
    hop = (pos('A7')[0], ya + 1.0)
    wire(net('B6'), [pos('B6'), dp]); via(net('B6'), dp)
    wire(net('B7'), [pos('B7'), dm]); via(net('B7'), dm)
    wire(net('B7'), [dm, (dm[0], hop[1] - 1.55), (hop[0], hop[1] - 0.8), hop], 0.2, B); via(net('B7'), hop)
    wire(net('A7'), [pos('A7'), hop])
    wire(net('B5'), [pos('B5'), cc2]); via(net('B5'), cc2)
    wire(net('A5'), [pos('A5'), (pos('A5')[0], ya + 0.55), (cc1[0], ya + 0.8), cc1]); via(net('A5'), cc1)
    pull = {v: r for r, q in lay.parts.items() if r.startswith('R') and 'GND' in q.conns.values()
            for v in q.conns.values() if v in ('USB_CC1', 'USB_CC2')}
    for cc, pts in ((net('B5'), [cc2, (cc2[0], cc2[1] + 0.7), (cc2[0] - 0.3, cc2[1] + 1.0), (cc2[0] - 0.3, ya + 2.15)]),
                    (net('A5'), [cc1, (cc1[0], ya + 2.15)])):   # each CC line's 5.1 k at the end of its way down
        wire(cc, pts, 0.2, B)
        lay.place_pad(pull[cc], next(n for n, v in lay.parts[pull[cc]].conns.items() if v == cc), *pts[-1], (0, 1))
    lay.place(comparator, x0 + 5.7, ya + 1.4, 0, True)   # the CC comparator east of the pairs, north of the core island
    for a, sx in (('A12', -1), ('A1', 1)):
        xy = (pos(a)[0] + sx * 0.45, ya + 0.95)
        wire('GND', [pos(a), (pos(a)[0], ya + 0.5), xy]); via('GND', xy)
    lay.tied |= {(j, n) for n in ('A1', 'A12', 'B1', 'B12')}

    # the switch's supply pins: ground into its pad and out at both ends, VCC to a via south of it
    for n in ('11', '20'):
        (x, y), out = lay.pad_xy(mux, n), math.copysign(1, lay.pad_xy(mux, n)[0] - x0)
        wire('GND', [(x0 + out * 1.3, y), (x0 + out * 2.9, y)]); via('GND', (x0 + out * 2.9, y))
    (x, y), (ox, oy) = lay.pad_xy(mux, '5'), lay.pad_xy(mux, '2')
    wire('GND', [(x, y), (x, ym + 0.3)])
    wire('GND', [(ox, oy), (ox, ym + 1.75), (x0 - 2.9, ym + 1.75), (x0 - 2.9, lay.pad_xy(mux, '20')[1])])
    # VCC: the plane under the switch is the core island, so on the bottom to a via north of the island, its capacitor there
    (x, y), feed = at(mux, '+3V3'), (x0 + 3.1, ym - 1.9)
    cap = next(r for r, q in lay.parts.items() if r.startswith('C') and q.block == lay.parts[mux].block and '+3V3' in q.conns.values()
               and lay.fps[r].GetBoard() is None)
    lay.place_pad(cap, next(n for n, v in lay.parts[cap].conns.items() if v == '+3V3'), feed[0], feed[1] + 0.7, (1, 0))
    wire('+3V3', [(x, y), (x, y + 1.0)]); via('+3V3', (x, y + 1.0)); via('+3V3', feed)
    wire('+3V3', [(x, y + 1.0), (x, feed[1] + 0.7), (feed[0], feed[1] + 0.7), feed], 0.25, B)
    lay.tied |= {(mux, n) for n in ('2', '5', '6', '11', '20', '21')} | {(cap, n) for n, v in lay.parts[cap].conns.items() if v == '+3V3'}

    # transmit: switch, capacitors, balls; the second-row ball leaves through its dog-bone site, north of the edge one
    ball = {t: lay.pad_xy(lay.bga, lay.ball_of(f'SS_TX_{t}')) for t in 'PN'}
    north, south = sorted('PN', key=lambda t: -ball[t][0])[::-1]
    yt, x_cap = ball[north][1] - 0.25, ball[south][0] + 3.0
    caps = {}
    for t, dy in ((north, -0.5), (south, 0.5)):
        c = next(r for r, q in lay.parts.items() if set(q.conns.values()) == {f'SS_TX_{t}', f'SS_TXC_{t}'})
        lay.place_pad(c, next(n for n, v in lay.parts[c].conns.items() if v == f'SS_TXC_{t}'), x_cap + 0.48, yt + dy, (-1, 0), bottom=False)
        caps[t] = (at(c, f'SS_TXC_{t}'), at(c, f'SS_TX_{t}'))
        x, y = caps[t][1]
        wire(f'SS_TX_{t}', [(x, y), (x - 0.27, y), (x - 0.52, y - dy / 2), (ball[south][0] + 0.95, y - dy / 2)], SS_W)
    wire(f'SS_TX_{north}', [(ball[south][0] + 0.95, yt - 0.25), (ball[north][0] + 0.5, yt - 0.25), ball[north]])
    wire(f'SS_TX_{south}', [(ball[south][0] + 0.95, yt + 0.25), ball[south]])
    cm = mid(mux, ['SS_TXC_P', 'SS_TXC_N'])
    turn, x_in = ym + 2.5, x_cap + 1.05   # west north of the regulator's divider (its ground via comes up there), then down to the capacitors
    pair(F, [(cm, ym + 1.188), (cm, turn - 0.4), (cm - 0.4, turn), (x_in + yt - turn, turn), (x_in, yt), (x_cap + 0.75, yt)],
         *((f'SS_TXC_{t}', caps[t][0], [], [caps[t][0]]) for t in 'PN'))

    # receive: vias south of the switch, bottom, west under the transmit pair, south beside the ball field, up at the balls
    ball = {t: lay.pad_xy(lay.bga, lay.ball_of(f'SS_RX_{t}')) for t in 'PN'}
    north, south = sorted('PN', key=lambda t: -ball[t][0])[::-1]
    pins = {t: at(mux, f'SS_RX_{t}') for t in 'PN'}
    west, east = sorted('PN', key=lambda t: pins[t][0])
    cm, yv, edge = mid(mux, ['SS_RX_P', 'SS_RX_N']), ym + 2.5, ball[south][0]
    dive = {west: (pins[west][0] + 0.05, yv), east: (pins[east][0] + 0.35, yv)}
    wire(f'SS_RX_{west}', [pins[west], (pins[west][0], yv - 0.3), dive[west]])
    wire(f'SS_RX_{east}', [pins[east], (pins[east][0], yv - 0.6), (dive[east][0], yv - 0.25), dive[east]])
    up = {north: (edge + 1.2, ball[north][1] - 0.5), south: (edge + 1.2, ball[north][1] + 0.3)}
    wire(f'SS_RX_{north}', [ball[north], (ball[north][0] + 0.5, up[north][1]), up[north]])
    wire(f'SS_RX_{south}', [ball[south], (edge + 0.7, ball[south][1]), (edge + 1.0, up[south][1]), up[south]])
    for t in 'PN':
        via(f'SS_RX_{t}', dive[t]); via(f'SS_RX_{t}', up[t])
    cv, run, lane = (dive[west][0] + dive[east][0]) / 2, yv + 0.55, edge + 2.25
    pair(B, [(cv, yv), (cv, yv + 0.2), (cv - 0.35, run), (lane + 0.45, run), (lane, run + 0.45), (lane, up[north][1] - 0.25)],
         (f'SS_RX_{north}', (lane - 0.5, up[north][1]), [dive[north]], [(lane - 0.5, up[north][1]), up[north]]),
         (f'SS_RX_{south}', (lane + 0.25, up[south][1]), [dive[south]], [(lane + 0.25, up[south][1] - 0.25), (lane, up[south][1]), up[south]]))

def hsmc_pairs(lay):
    """GTS lanes by hand: TX on F.Cu over the In1 GND plane, RX on B.Cu over the In2 3.3 V plane, so the TX and RX of a
    lane, side by side on the connector rows, never cross. The left line of each pair is the north connector pin; P/N are
    swapped in the netlist (HSMC_SWAP) where the destination order needs it. Corridors are kept free of passives."""
    j = refs(lay, 'HSMC_CARD')[0]
    cage0, cage1 = refs(lay, 'SFP_HOST')
    u = refs(lay, 'SY88236L')[0]
    pad = lambda ref, n: lay.pad_xy(ref, n)
    net = lambda ref, n: next(p.GetNetname() for p in lay.fps[ref].Pads() if p.GetNumber() == n)
    def via(n, xy, layer_to_pad, pad_xy):
        lay.via(n, *xy, locked=True)
        lay.track(n, [xy, pad_xy], 2 * NECK_W, layer_to_pad, locked=True)
        keep_free(lay, 'F', [xy, pad_xy] if layer_to_pad == pcbnew.F_Cu else [xy, xy])
    def tx(pin, rest, tails):   # odd row: north pin to a via west of it (left line), south pin to a via east of it
        (xa, ya), (xb, yb) = pad(j, str(pin)), pad(j, str(pin + 2))
        west, east = (xa - 1.5, ya), (xb + 1.6, yb)
        center = [(east[0], ya + 0.1)] + rest
        nl, nr = net(j, str(pin)), net(j, str(pin + 2))
        via(nl, west, pcbnew.B_Cu, (xa, ya)); via(nr, east, pcbnew.B_Cu, (xb, yb))
        route_pair(lay, pcbnew.F_Cu, center, ((nl, [west, (west[0] + 0.3, ya - 0.3)], tails[0]), (nr, [], tails[1])))
        return nl, nr
    def rx(pin, rest, tails, up=None):   # even row: straight out east on B.Cu, spread from the 0.5 mm pin pitch;
        (xa, ya), (xb, yb) = pad(j, str(pin)), pad(j, str(pin + 2))   # up: center point where it goes on F.Cu
        x0 = xa + 1.6
        center = [(x0, ya + 0.25)] + rest
        nl, nr = net(j, str(pin)), net(j, str(pin + 2))
        k = center.index(up) + 1 if up else len(center)
        route_pair(lay, pcbnew.B_Cu, center[:k], ((nl, [(xa, ya), (x0 - 0.15, ya)], [] if up else tails[0]),
                                                (nr, [(xb, yb), (x0 - 0.15, yb)], [] if up else tails[1])))
        if up:
            for n, xy in zip((nl, nr), (left_offset(center[k - 2:k], d)[-1] for d in (PAIR_PITCH / 2, -PAIR_PITCH / 2))):
                lay.via(n, *xy, locked=True); keep_free(lay, 'F', [xy, xy])
            route_pair(lay, pcbnew.F_Cu, center[k - 1:], ((nl, [], tails[0]), (nr, [], tails[1])))
        return nl, nr
    def to_sfp(cage, nets, nums):   # left line on the north pad of the cage pair
        assert [net(cage, n) for n in nums] == list(nets), (cage, nets)
    # cage TX straight onto the cage pads (TD+ north, 0.8 mm pitch)
    for pin, cage, x_turn in ((17, cage0, 28.0), (21, cage1, 20.0)):
        y0 = pad(j, str(pin))[1] + 0.1
        t, b = pad(cage, '18'), pad(cage, '19')
        ye = (t[1] + b[1]) / 2
        to_sfp(cage, tx(pin, [(x_turn, y0), (x_turn + abs(ye - y0), ye), (t[0], ye)], ([], [])), ('18', '19'))
    # cage RX: B.Cu out of the connector, up to F.Cu (past the TX pairs, west of the +3V3_SFP plane)
    # onto RD- (north) / RD+
    for pin, cage, x_turn, x_up in ((18, cage0, 22.0, 25.0), (22, cage1, 29.6, 25.5)):
        y0 = pad(j, str(pin))[1] + 0.25
        t, b = pad(cage, '12'), pad(cage, '13')
        ye = (t[1] + b[1]) / 2
        bend = [(x_turn, y0), (x_turn + abs(ye - y0), ye), (t[0], ye)]
        up = (x_up, y0 + (1 if ye > y0 else -1) * max(0.0, x_up - x_turn))   # on the straight run or on the bend
        bend.insert(0 if x_up < x_turn else 1, up)
        nl, nr = rx(pin, bend, ([], []), up)
        to_sfp(cage, (nl, nr), ('12', '13'))
    # ONU TX through its AC caps (left: DIN+ pin 6, north) to the SY88236L west pins
    p6, p7 = pad(u, '6'), pad(u, '7')
    yc = (p6[1] + p7[1]) / 2
    nl, nr = net(j, '25'), net(j, '27')
    cn, cp = part_on(lay, nl, 'C'), part_on(lay, nr, 'C')
    put_pair(lay, [(cn, p6[0] - 3.0, yc - 0.5), (cp, p6[0] - 3.0, yc + 0.5)], 0)
    a = [pad_of_net(lay, c, n) for c, n in ((cn, nl), (cp, nr))]
    y0 = pad(j, '25')[1] + 0.1
    tx(25, [(a[0][0] - 1.5 - (yc - y0), y0), (a[0][0] - 1.5, yc), (a[0][0] - 0.5, yc)],
       ([(a[0][0] - 0.4, yc - 0.5), a[0]], [(a[1][0] - 0.4, yc + 0.5), a[1]]))
    for c, pin in ((cn, p6), (cp, p7)):
        n = net(u, '6' if pin is p6 else '7')
        q = pad_of_net(lay, c, n)
        pts = [q, (pin[0] - 0.5 - abs(pin[1] - q[1]), q[1]), (pin[0] - 0.5, pin[1]), pin]
        lay.track(n, pts, 2 * NECK_W, locked=True); keep_free(lay, 'F', pts)
    # burst enable straight to the LVDS receiver, approached from the north (A = BEN+ on the east pin)
    sn = refs(lay, 'SN65LVDT2DBV')[0]
    for rot in (0, 90, 180, 270):
        lay.place(sn, 40.0, 60.0, rot)
        pa, pb = pad(sn, '3'), pad(sn, '4')
        if pa[1] < 60.0 and pb[1] < 60.0 and pa[0] > pb[0]:
            break
        lay.pcb.Remove(lay.fps[sn]); lay.rects['F'].pop()
    else:
        raise RuntimeError('SN65LVDT2 orientation')
    xm, y0 = (pa[0] + pb[0]) / 2, pad(j, '29')[1] + 0.1
    ye = pa[1] - 1.7
    nl, nr = tx(29, [(xm - (ye - y0), y0), (xm, ye), (xm, ye + 0.55)],
                ([(pa[0], ye + 0.55 + pa[0] - xm - 0.4), pa], [(pb[0], ye + 0.55 + xm - 0.4 - pb[0]), pb]))
    assert (nl, nr) == (net(sn, '3'), net(sn, '4'))
    # LA input (2.5 Gb/s downstream): the TIA AC-coupling caps right north of pins 28 / 27, short straight necks
    for pin, dx in (('28', 0.0), ('27', 0.7)):
        n, (x, y) = net(u, pin), pad(u, pin)
        c = part_on(lay, n, 'C')
        put_pair(lay, [(c, x + dx, y - 5.0)], 270)
        q = pad_of_net(lay, c, n)
        pts = [(x, y), (x, q[1] + 0.5 + dx), (x + dx, q[1] + 0.5), q]
        lay.track(n, pts, 2 * NECK_W, locked=True); keep_free(lay, 'F', pts)
    # ONU RX: B.Cu to vias north of its AC caps (straight north of SY88236L pins 31/30)
    cl, cr = (pad_of_net(lay, part_on(lay, net(j, n), 'C'), net(j, n)) for n in ('26', '28'))
    assert cl[0] > cr[0]   # travelling south, the left line is the east one
    xm, y0, ye = (cl[0] + cr[0]) / 2, pad(j, '26')[1] + 0.25, cl[1] - 2.6
    nl, nr = rx(26, [(xm - (ye - y0), y0), (xm, ye), (xm, ye + 0.7)],
                ([(cl[0], ye + 0.7 + cl[0] - xm - 0.4), (cl[0], cl[1] - 1.1)],
                 [(cr[0], ye + 0.7 + xm - 0.4 - cr[0]), (cr[0], cr[1] - 1.1)]))
    for n, c in ((nl, cl), (nr, cr)):
        via(n, (c[0], c[1] - 1.1), pcbnew.F_Cu, c)

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

def planes(lay):
    w, h, e = lay.w, lay.h, 0.3
    full = [(e, e), (w - e, e), (w - e, h - e), (e, h - e)]
    cx, cy = BGA_C
    lay.zone('GND', pcbnew.In1_Cu, full)
    lay.zone('+3V3', pcbnew.In2_Cu, full)
    # core: the box of the core balls' dog-bones, a neck along the via-free centre line, then wide once out of the
    # via field (decaps + buck output)
    m = lay.pitch / 2 + 0.35
    core = [xy for r, xy in lay.pads_of(lay.core) if r == lay.bga]
    x0, x1 = min(x for x, y in core) - m, max(x for x, y in core) + m
    y0, y1 = min(min(y for x, y in core) - m, cy - 0.55), max(max(y for x, y in core) + m, cy + 0.55)
    i0, i1, i2, i3 = lay.island
    neck = i0 - 0.5
    lay.zone(lay.core, pcbnew.In2_Cu, [(x0, y0), (x1, y0), (x1, cy - 0.55), (neck, cy - 0.55), (neck, i1 - 0.5),
                                       (i2 + 0.5, i1 - 0.5), (i2 + 0.5, i3 + 0.5), (neck, i3 + 0.5), (neck, cy + 0.55),
                                       (x1, cy + 0.55), (x1, y1), (x0, y1)],
             priority=1, clearance=0.2, min_width=0.15)
    lay.rule_area([(x1, cy - 0.55), (neck, cy - 0.55), (neck, cy + 0.55), (x1, cy + 0.55)], pcbnew.LSET.AllCuMask(), vias=False)
    # outer GND pours come after routing (finish.py): poured copper would wall the router in


def finish(lay):
    added, missed = plane_vias(lay)
    print('plane vias', added, 'NO ROOM for', missed if missed else 'none')
    lay.outline()
    print(lay.bd.name, 'placed', len(lay.placed_refs()), 'of', len(lay.parts))
    print(lay.save())

def lane_exit(lay, second, edge=None, reach=1.0):
    """Top-layer lane out of the ball field: the second-row ball leaves through its own dog-bone site, between the
    edge balls. With `edge` the two leave as a pair, `reach` mm east of the edge row, and the edge-row leg first passes
    under the other on the bottom: it is then the north one, the west one once the pair turns north to its capacitors
    (left to the router, the two legs wall each other in). The corridor is kept free of parts."""
    net = lambda ball: next(q.GetNetname() for q in lay.fps[lay.bga].Pads() if q.GetNumber() == ball)
    (x, y), w = lay.pad_xy(lay.bga, second), lay.bd.serdes_w
    tracks = [(net(second), [(x, y), (x + 0.5, y - 0.5), (x + 1.0 + reach, y - 0.5)], pcbnew.F_Cu)]
    if edge:
        (ex, ey), n = lay.pad_xy(lay.bga, edge), net(edge)
        down, up = (ex + 1.4, ey), (ex + 1.4, ey - 1.0)
        tracks += [(n, [(ex, ey), down], pcbnew.F_Cu), (n, [down, up], pcbnew.B_Cu), (n, [up, (ex + reach, ey - 1.0)], pcbnew.F_Cu)]
        lay.via(n, *down)
        lay.via(n, *up)
    for n, pts, layer in tracks:
        lay.track(n, pts, w, layer)
        keep_free(lay, 'F' if layer == pcbnew.F_Cu else 'B', pts[-2:], 0.45)

def fpga_board(lay):
    if lay.bd.name == 'inline':   # Artix-7: the edge-row GTP lane 0 (downstream) leaves on top, the rest dog-bones
        lane_exit(lay, 'B4')
        lane_exit(lay, 'B8', 'A8', 6.0)   # the receive pair, past the parts around the balls
        done = next(r for r, q in lay.parts.items() if set(q.conns.values()) == {'DONE', '+3V3'})
        lay.place_near(done, lay.island[0], lay.island[3] + 1.5)   # south of the core island: +3V3 is under it there
        passives(lay, None)
        lay.fanout({'A4', 'B4', 'A8', 'B8'} | {lay.ball_of(f'SS_{d}X_{t}') for d in 'TR' for t in 'PN'}, PLANE_NETS)
        planes(lay)
        return
    hand = sniffer_serdes(lay) if lay.bd.name == 'sniffer' else serdes(lay, lay.bd.name == 'olt')   # exact pairs first
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


FORMER_BOSA = {'onu', 'hsmc', 'sniffer'}   # routed with the estimated land: converted when next regenerated (bosa.py)

if __name__ == '__main__':
    force = '--force' in sys.argv
    names = [a for a in sys.argv[1:] if not a.startswith('-')] or ['olt', 'onu', 'hsmc', 'sniffer', 'inline']
    for make in (olt_layout, onu_layout, hsmc_layout, sniffer_layout, inline_layout):
        board = make.__name__.split('_')[0]
        if board not in names:
            continue
        if board in FORMER_BOSA:
            sys.exit(f'{board}: placed for the former BOSA land (origin, can distance): convert its layout first')
        lay = make()
        target = os.path.join(board_dir(lay.bd.name), lay.bd.name + '.kicad_pcb')
        if os.path.exists(target) and not force:  # a routed board is the source from then on
            sys.exit(f'{target} exists: routing would be lost; --force to regenerate')
        if lay.bga:
            fpga_board(lay)
        else:
            passives(lay, None)
            hsmc_planes(lay)
        finish(lay)
