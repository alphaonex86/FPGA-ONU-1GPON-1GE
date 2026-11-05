# SPDX-License-Identifier: CERN-OHL-S-2.0
"""Initial PCB of every board: placement, outline, rules, BGA fanout, planes."""
import json, math, os, re, sys
import pcbnew
sys.path.insert(0, os.path.dirname(__file__))
from boards import ECP5_BALLS, ball_xy
from kisch import uid
from paths import LIB, KI_LIB, board_dir

FP = '/usr/share/kicad/footprints'
OX, OY = 100.0, 100.0           # board origin on the page
mm = pcbnew.FromMM
def V(x, y): return pcbnew.VECTOR2I(mm(OX + x), mm(OY + y))
def P(v): return (pcbnew.ToMM(v.x) - OX, pcbnew.ToMM(v.y) - OY)

BGA_C = (32.0, 26.0)
VIA_D, VIA_HOLE = 0.45, 0.2
POWER_PREFIX = ('+', 'GND')
PLANE_NETS = ('GND', '+3V3', '+1V1')   # In1 GND, In2 +3V3 with a +1V1 island
HV_PREFIX, HV_CLEAR = 'HV_', 0.2   # APD bias nets (<prefix>HV_*), up to 61 V
SUB_RAILS = ('+1V1_SD', '+2V5')   # SerDes 1.1 V (~0.2 A) and VCCAUX 2.5 V (~50 mA): 0.2 mm, ~17 mV over 35 mm

def fp_dir(lib):
    return f'{LIB}/{lib}.pretty' if lib == 'bridge' else f'{FP}/{lib}.pretty'

def unconnected_name(ref, part, num, multi_unit):
    unit, name = next(((p[0], p[2]) for p in part.pins if p[1] == num), (1, '~'))
    letter = chr(ord('A') + unit - 1) if multi_unit else ''
    pin = f'-{name}' if name not in ('', '~') else ''
    return f'unconnected-({ref}{letter}{pin}-Pad{num})'.replace('/', '{slash}')

def is_free(net):
    return not net or net.startswith('unconnected-')

def own_models(fp):
    """Point the footprint's 3D models at common/pcb/lib/3d where we made one; return how many changed."""
    models, changed = list(fp.Models()), 0   # python sees copies: edit them, then put them back
    for m in models:
        name = os.path.basename(m.m_Filename)
        if os.path.exists(f'{LIB}/3d/{name}') and m.m_Filename != f'{KI_LIB}/3d/{name}':
            m.m_Filename = f'{KI_LIB}/3d/{name}'
            changed += 1
    if changed:
        fp.Models().clear()
        for m in models:
            fp.Add3DModel(m)
    return changed

def finish_footprint(fp):
    """Our silk conventions, on generated and routed boards alike: references on the fab layer
    (0402 density makes them unreadable on silk; JLC assembles from the CPL), passive outlines on fab too,
    no field text but the value on fab. Return whether anything changed."""
    changed = own_models(fp) > 0
    fab = pcbnew.B_Fab if fp.IsFlipped() else pcbnew.F_Fab
    for f in fp.GetFields():
        if f.IsReference() and f.GetLayer() != fab:
            f.SetLayer(fab); changed = True
        elif not f.IsReference() and not f.IsValue() and f.IsVisible():
            f.SetVisible(False); changed = True
    if fp.GetReference().rstrip('0123456789') in ('R', 'C', 'L', 'FB', 'RN'):
        to_fab = {pcbnew.F_SilkS: pcbnew.F_Fab, pcbnew.B_SilkS: pcbnew.B_Fab}   # not Remove(): it breaks the SWIG state
        for g in fp.GraphicalItems():
            if g.GetLayer() in to_fab:
                g.SetLayer(to_fab[g.GetLayer()]); changed = True
    return changed

class Layout:
    def __init__(self, bd, width, height):
        self.bd, self.w, self.h = bd, width, height
        self.notch = None   # (x, y): the corner west of x and south of y cut away
        self.pcb = pcbnew.BOARD()
        self.pcb.SetCopperLayerCount(4)
        for layer in (pcbnew.In1_Cu, pcbnew.In2_Cu):  # planes: vias only, never traces
            self.pcb.SetLayerType(layer, pcbnew.LT_POWER)
        self.nets = {}
        for name in sorted(bd.nets()):
            ni = pcbnew.NETINFO_ITEM(self.pcb, name)
            self.pcb.Add(ni)
            self.nets[name] = ni
        self.parts = {p.ref: p for p in bd.parts if p.on_board and p.footprint}
        self.fps, self.rects = {}, {'F': [], 'B': []}
        self.keepout = {'F': [], 'B': []}
        root = uid(bd.name, 'sheet')
        for ref, part in self.parts.items():
            lib, name = part.footprint.split(':')
            fp = pcbnew.FootprintLoad(fp_dir(lib), name)
            fp.SetFPID(pcbnew.LIB_ID(lib, name))
            fp.SetReference(ref)
            fp.SetValue(part.value)
            fp.SetPath(pcbnew.KIID_PATH(f'/{root}/{uid(bd.name, ref, 1)}'))
            for k, v in part.fields.items():
                fp.SetField(k, v)
            finish_footprint(fp)
            fp.SetDNP(part.dnp)
            units = {pin[0] for pin in part.pins}
            for pad in fp.Pads():
                num = pad.GetNumber()
                if not num or pad.GetAttribute() == pcbnew.PAD_ATTRIB_NPTH:  # mounting holes: no pin, no net
                    continue
                net = part.conns.get(num) or unconnected_name(ref, part, num, len(units) > 1)
                if net not in self.nets:  # a free pin gets the net name eeschema gives it
                    self.nets[net] = pcbnew.NETINFO_ITEM(self.pcb, net)
                    self.pcb.Add(self.nets[net])
                pad.SetNet(self.nets[net])
            self.fps[ref] = fp

    def rect(self, fp, margin=0.15):
        bb = fp.GetBoundingBox(False)
        x0, y0 = P(bb.GetOrigin()); x1, y1 = P(bb.GetEnd())
        return (x0 - margin, y0 - margin, x1 + margin, y1 + margin)

    def place(self, ref, x, y, rot=0, bottom=False):
        fp = self.fps[ref]
        self.pcb.Add(fp)
        if fp.IsFlipped() != bottom:
            fp.SetLayerAndFlip(pcbnew.B_Cu if bottom else pcbnew.F_Cu)
        fp.SetOrientationDegrees(rot)
        fp.SetPosition(V(x, y))
        self.rects['B' if bottom else 'F'].append(self.rect(fp))
        if fp.GetAttributes() & pcbnew.FP_THROUGH_HOLE or any(p.GetAttribute() in (pcbnew.PAD_ATTRIB_PTH, pcbnew.PAD_ATTRIB_NPTH) for p in fp.Pads()):
            self.rects['F' if bottom else 'B'].append(self.rect(fp, 0.1))  # holes block the other side
        return fp

    def free(self, r, side):
        if r[0] < 0.5 or r[1] < 0.5 or r[2] > self.w - 0.5 or r[3] > self.h - 0.5:
            return False
        ov = lambda a, b: a[0] < b[2] and b[0] < a[2] and a[1] < b[3] and b[1] < a[3]
        return not any(ov(r, o) for o in self.rects[side] + self.keepout[side])

    def place_near(self, ref, x, y, bottom=False, rot=0, step=0.25, limit=25, within=None):
        fp = self.fps[ref]
        self.pcb.Add(fp)
        if bottom:
            fp.SetLayerAndFlip(pcbnew.B_Cu)
        fp.SetOrientationDegrees(rot)
        side = 'B' if bottom else 'F'
        for radius in range(0, int(limit / step)):
            n = max(1, int(2 * math.pi * radius))
            for k in range(n):
                a = 2 * math.pi * k / n
                px, py = round((x + radius * step * math.cos(a)) / 0.05) * 0.05, round((y + radius * step * math.sin(a)) / 0.05) * 0.05
                fp.SetPosition(V(px, py))
                r = self.rect(fp)
                inside = within is None or (within[0] <= r[0] and within[1] <= r[1] and r[2] <= within[2] and r[3] <= within[3])
                if inside and self.free(r, side):
                    self.rects[side].append(r)
                    return fp
        self.pcb.Remove(fp)
        raise RuntimeError(f'{self.bd.name}: no room for {ref} near ({x:.1f}, {y:.1f})')

    def place_pad(self, ref, num, x, y, toward, bottom=True):
        """2-pad part: pad `num` at (x, y), the other pad in direction `toward`."""
        fp = self.fps[ref]
        self.pcb.Add(fp)
        if fp.IsFlipped() != bottom:
            fp.SetLayerAndFlip(pcbnew.B_Cu if bottom else pcbnew.F_Cu)
        fp.SetPosition(V(0, 0))
        def pads(rot):
            fp.SetOrientationDegrees(rot)
            xy = {p.GetNumber(): P(p.GetPosition()) for p in fp.Pads()}
            return xy[num], next(v for k, v in xy.items() if k != num)
        def score(rot):
            a, b = pads(rot)
            return (b[0] - a[0]) * toward[0] + (b[1] - a[1]) * toward[1]
        a, _ = pads(max((0, 90, 180, 270), key=score))
        fp.SetPosition(V(x - a[0], y - a[1]))
        self.rects['B' if bottom else 'F'].append(self.rect(fp))

    def pad_xy(self, ref, num):
        for p in self.fps[ref].Pads():
            if p.GetNumber() == num:
                return P(p.GetPosition())
        raise KeyError((ref, num))

    def placed_refs(self):
        return [r for r, fp in self.fps.items() if fp.GetBoard() is not None]

    def pads_of(self, net):
        return [(r, P(p.GetPosition())) for r in self.placed_refs() for p in self.fps[r].Pads() if p.GetNetname() == net]

    def ball_of(self, net):
        return next((p.GetNumber() for p in self.fps[self.bga].Pads() if p.GetNetname() == net), None)

    def bus_balls(self):
        """Rows A/B balls carrying the Nano bus: escaped on the top layer, no via."""
        return [p.GetNumber() for p in self.fps[self.bga].Pads()
                if p.GetNumber()[0] in 'AB' and not is_free(p.GetNetname()) and not p.GetNetname().startswith(('+', 'GND'))]

    def cap_for_bga(self, ref):
        return self.parts[ref].block == 'ecp5_support'

    def on_bottom_net(self, nets):
        return any(self.fps[r].IsFlipped() for n in nets if not n.startswith(POWER_PREFIX) for r, xy in self.pads_of(n)
                   if r.rstrip('0123456789') not in ('R', 'C', 'FB', 'L', 'RN', 'TP'))

    def header(self, east_x, odd_y):
        """2x20 socket on the bottom: pin 1 east, pins westward, even row south (outward)."""
        fp = self.fps['J1']
        self.pcb.Add(fp)
        fp.SetLayerAndFlip(pcbnew.B_Cu)
        for rot in (0, 90, 180, 270):
            fp.SetOrientationDegrees(rot)
            fp.SetPosition(V(0, 0))
            p1, p2, p39 = (self.pad_xy('J1', n) for n in ('1', '2', '39'))
            if p39[0] < p1[0] - 40 and p2[1] > p1[1] + 2:
                break
        else:
            raise RuntimeError('header orientation')
        fp.SetPosition(V(east_x - p1[0], odd_y - p1[1]))
        self.rects['B'].append(self.rect(fp))
        self.rects['F'].append(self.rect(fp, 0.0))

    def outline(self):
        pts = [(0, 0), (self.w, 0), (self.w, self.h), (0, self.h)]
        if self.notch:
            x, y = self.notch
            pts[3:] = [(x, self.h), (x, y), (0, y)]
        for a, b in zip(pts, pts[1:] + pts[:1]):
            s = pcbnew.PCB_SHAPE(self.pcb)
            s.SetShape(pcbnew.SHAPE_T_SEGMENT)
            s.SetStart(V(*a)); s.SetEnd(V(*b))
            s.SetLayer(pcbnew.Edge_Cuts); s.SetWidth(mm(0.1))
            self.pcb.Add(s)

    def track(self, net, pts, width, layer=pcbnew.F_Cu, locked=False):
        for a, b in zip(pts, pts[1:]):
            if a == b:
                continue
            t = pcbnew.PCB_TRACK(self.pcb)
            t.SetStart(V(*a)); t.SetEnd(V(*b)); t.SetWidth(mm(width)); t.SetLayer(layer)
            t.SetNet(self.nets[net]); t.SetLocked(locked)
            self.pcb.Add(t)

    def via(self, net, x, y, locked=False):
        v = pcbnew.PCB_VIA(self.pcb)
        v.SetPosition(V(x, y)); v.SetWidth(mm(VIA_D)); v.SetDrill(mm(VIA_HOLE))
        v.SetNet(self.nets[net]); v.SetLocked(locked)
        self.pcb.Add(v)

    def zone(self, net, layer, poly, priority=0, clearance=0.2, min_width=0.2):
        z = pcbnew.ZONE(self.pcb)
        z.SetLayer(layer)
        z.SetNet(self.nets[net])
        ol = z.Outline(); ol.NewOutline()
        for x, y in poly:
            ol.Append(mm(OX + x), mm(OY + y))
        z.SetAssignedPriority(priority)
        z.SetLocalClearance(mm(clearance))
        z.SetMinThickness(mm(min_width))
        z.SetPadConnection(pcbnew.ZONE_CONNECTION_THERMAL)
        z.SetThermalReliefGap(mm(0.2)); z.SetThermalReliefSpokeWidth(mm(0.25))
        self.pcb.Add(z)

    def no_vias(self, poly):
        """Rule area: no via may cut this plane neck; tracks still pass."""
        z = pcbnew.ZONE(self.pcb)
        z.SetIsRuleArea(True)
        z.SetDoNotAllowVias(True); z.SetDoNotAllowTracks(False); z.SetDoNotAllowPads(False)
        z.SetDoNotAllowZoneFills(False); z.SetDoNotAllowFootprints(False)
        z.SetLayerSet(pcbnew.LSET.AllCuMask())
        ol = z.Outline(); ol.NewOutline()
        for x, y in poly:
            ol.Append(mm(OX + x), mm(OY + y))
        self.pcb.Add(z)

    def fanout(self, top_balls, plane_nets):
        """Dog-bone via for every connected ball except `top_balls` (left to the top-layer
        router or routed by hand). Vias point away from the BGA centre, so the centre lines
        stay via-free: that channel carries the +1V1 plane out on In2."""
        cx, cy = BGA_C
        fp = self.fps[self.bga]
        for pad in fp.Pads():
            b, net = pad.GetNumber(), pad.GetNetname()
            if is_free(net) or b in top_balls:
                continue
            bx, by = ball_xy(b)
            dx = 0.4 if bx > 0 else -0.4
            dy = 0.4 if by > 0 else -0.4
            x, y = cx + bx, cy + by
            self.track(net, [(x, y), (x + dx, y + dy)], 0.2 if net.startswith(POWER_PREFIX) else 0.1)
            self.via(net, x + dx, y + dy)
        # rails without a plane: neighbour balls also join on top, the via field is too tight for their width
        net_at = {tuple(round(v, 2) for v in ball_xy(p.GetNumber())): p.GetNetname() for p in fp.Pads() if p.GetNumber() not in top_balls}
        for (bx, by), net in net_at.items():
            if net.startswith('+') and net not in plane_nets:
                for nx, ny in ((bx + 0.8, by), (bx, by + 0.8)):
                    if net_at.get((round(nx, 2), round(ny, 2))) == net:
                        self.track(net, [(cx + bx, cy + by), (cx + nx, cy + ny)], 0.1)

    def save(self):
        d = board_dir(self.bd.name)
        path = f'{d}/{self.bd.name}.kicad_pcb'
        self.pcb.Save(path)
        write_project(f'{d}/{self.bd.name}.kicad_pro', self.bd)
        return path

def write_project(path, bd):
    """JLCPCB 4-layer rules (1.6 mm, 1 oz outer): 0.1/0.1 mm track/space, 0.45/0.2 via."""
    cls = lambda name, track, clear, via=(VIA_D, VIA_HOLE): {
        'name': name, 'track_width': track, 'clearance': clear, 'via_diameter': via[0], 'via_drill': via[1],
        'diff_pair_width': 0.2, 'diff_pair_gap': 0.2, 'diff_pair_via_gap': 0.25, 'microvia_diameter': 0.3,
        'microvia_drill': 0.1, 'bus_width': 12, 'wire_width': 6, 'line_style': 0,
        'pcb_color': 'rgba(0, 0, 0, 0.000)', 'schematic_color': 'rgba(0, 0, 0, 0.000)', 'priority': 0}
    pro = json.load(open(path)) if os.path.exists(path) else {}
    pro.setdefault('meta', {'filename': os.path.basename(path), 'version': 3})
    pro['board'] = {'design_settings': {
        'defaults': {}, 'diff_pair_dimensions': [{'gap': 0.2, 'via_gap': 0.25, 'width': 0.2}],
        'drc_exclusions': [], 'meta': {'filename': 'board_design_settings.json', 'version': 2},
        'rules': {'allow_blind_buried_vias': False, 'allow_microvias': False, 'min_clearance': 0.1,
                  'min_track_width': 0.1, 'min_via_diameter': VIA_D, 'min_through_hole_diameter': VIA_HOLE,
                  'min_via_annular_width': 0.1, 'min_hole_clearance': 0.2, 'min_hole_to_hole': 0.25,
                  'min_copper_edge_clearance': 0.3, 'min_silk_clearance': 0.0},
        'rule_severities': {'lib_footprint_mismatch': 'ignore'},   # finish_footprint moves silk to fab on purpose
        'track_widths': [0.1, 0.15, 0.2, 0.3, 0.5], 'via_dimensions': [{'diameter': VIA_D, 'drill': VIA_HOLE}]}}
    power = [n for n in bd.nets() if n.startswith(POWER_PREFIX)]
    # BGA rails without a plane thread the via field: 0.35 mm channels fit 0.15 mm at 0.1 clearance
    ecp5 = next((p for p in bd.parts if p.value.startswith('LFE5')), None)
    rails = sorted({n for n in ecp5.conns.values() if n.startswith('+')} - set(PLANE_NETS)) if ecp5 else []
    sub = [n for n in SUB_RAILS if n in power]
    power = [n for n in power if n not in rails and n not in sub]
    pro['net_settings'] = {'meta': {'version': 4}, 'net_colors': None, 'netclass_assignments': None,
                           'classes': [cls('Default', 0.1, 0.1), cls('Power', 0.3, 0.1), cls('Rail', 0.15, 0.1), cls('Sub', 0.2, 0.1),
                                       cls('SerDes', bd.serdes_w, 0.1), cls('HV', 0.2, HV_CLEAR)],
                           'netclass_patterns': [{'netclass': 'Power', 'pattern': n} for n in sorted(power)] +
                                                [{'netclass': 'Rail', 'pattern': n} for n in rails] +
                                                [{'netclass': 'Sub', 'pattern': n} for n in sub] +
                                                [{'netclass': 'SerDes', 'pattern': p} for p in
                                                 ('TX0_*', 'RX0_*', 'BEN_*', 'ONU_TD_*', 'ONU_RD_*', 'ONU_BEN_*', 'LD_*', 'TIA_*', 'RIN_*',
                                                  'ONU_TX_*', 'ONU_RX_*', 'SFP0_TD_*', 'SFP0_RD_*', 'SFP1_TD_*', 'SFP1_RD_*', 'GTS_*', 'US_TIA_*', 'US_DIN_*',
                                                  'US_LA_*', 'US_RX_*')] +
                                                [{'netclass': 'HV', 'pattern': '*' + HV_PREFIX + '*'}]}
    json.dump(pro, open(path, 'w'), indent=2, sort_keys=True)


def seg_dist(p, a, b):
    ax, ay = a; bx, by = b; px, py = p
    dx, dy = bx - ax, by - ay
    t = 0.0 if dx == dy == 0 else max(0.0, min(1.0, ((px - ax) * dx + (py - ay) * dy) / (dx * dx + dy * dy)))
    return math.hypot(px - ax - t * dx, py - ay - t * dy)

def plane_vias(lay, clear=0.12):
    """A via beside every SMD pad of a plane net, where that net's inner plane is under it.
    Returns (vias added, pads over their plane that found no room)."""
    zones = sorted((z for z in lay.pcb.Zones() if z.GetLayer() in (pcbnew.In1_Cu, pcbnew.In2_Cu)),
                   key=lambda z: -z.GetAssignedPriority())
    plane_nets = {z.GetNetname() for z in zones}
    def owner(layer, x, y):
        return next((z.GetNetname() for z in zones if z.GetLayer() == layer and z.Outline().Contains(V(x, y))), None)
    def plane_of(net, x, y):  # every point within 0.5 mm belongs to the net's top-priority zone on some inner layer
        pts = ((x, y), (x + 0.5, y), (x - 0.5, y), (x, y + 0.5), (x, y - 0.5))
        return any(all(owner(layer, *p) == net for p in pts) for layer in (pcbnew.In1_Cu, pcbnew.In2_Cu))
    obst = []   # (kind, geometry, net, layers)
    for fp in lay.pcb.GetFootprints():
        for p in fp.Pads():
            bb = p.GetBoundingBox()
            (x0, y0), (x1, y1) = P(bb.GetOrigin()), P(bb.GetEnd())
            obst.append(('rect', (x0, y0, x1, y1), p.GetNetname(), 'all' if p.HasHole() else ('B' if fp.IsFlipped() else 'F')))
    for t in lay.pcb.GetTracks():
        if t.GetClass() == 'PCB_VIA':
            obst.append(('circle', (P(t.GetPosition()), pcbnew.ToMM(t.GetWidth(pcbnew.F_Cu)) / 2), t.GetNetname(), 'all'))
        else:
            obst.append(('seg', (P(t.GetStart()), P(t.GetEnd()), pcbnew.ToMM(t.GetWidth()) / 2), t.GetNetname(),
                         'F' if t.GetLayer() == pcbnew.F_Cu else 'B'))
    def free(x, y, r, net, side, via):
        """No foreign copper within clearance of a disc (a via, or a stub sample on one side)."""
        for kind, g, n, layers in obst:
            if n == net and not (via and kind == 'circle'):
                continue
            if not via and layers not in ('all', side):
                continue
            if kind == 'rect':
                d = math.hypot(max(g[0] - x, 0, x - g[2]), max(g[1] - y, 0, y - g[3])) - r
            elif kind == 'circle':
                d = math.hypot(g[0][0] - x, g[0][1] - y) - g[1] - r
            else:
                d = seg_dist((x, y), g[0], g[1]) - g[2] - r
            if d < (0.25 + r if via and kind == 'circle' else clear + (HV_CLEAR - 0.1 if HV_PREFIX in n else 0)):
                return False
        return True
    keepouts = [z for z in lay.pcb.Zones() if z.GetIsRuleArea() and z.GetDoNotAllowVias()]
    def ok(px, py, x, y, net, side):
        if not (1.0 < x < lay.w - 1.0 and 1.0 < y < lay.h - 1.0) or not plane_of(net, x, y):
            return False
        if any(z.Outline().Contains(V(x, y), -1, mm(VIA_D / 2 + clear)) for z in keepouts):
            return False
        return free(x, y, VIA_D / 2, net, side, True) and stub_free(px, py, x, y, net, side)
    rule_areas = list(lay.pcb.Zones()) + [z for fp in lay.pcb.GetFootprints() for z in fp.Zones()]   # cage keepouts
    no_tracks = {s: [z for z in rule_areas if z.GetIsRuleArea() and z.GetDoNotAllowTracks() and z.IsOnLayer(l)]
                 for s, l in (('F', pcbnew.F_Cu), ('B', pcbnew.B_Cu))}
    def stub_free(px, py, x, y, net, side):
        n = max(2, int(math.hypot(x - px, y - py) / 0.05))
        pts = [(px + (x - px) * k / n, py + (y - py) * k / n) for k in range(n + 1)]
        return all(free(*q, 0.125, net, side, False) and
                   not any(z.Outline().Contains(V(*q), -1, mm(0.125 + clear)) for z in no_tracks[side]) for q in pts)
    added, missed = 0, []
    via_in_pad = {(fp.GetReference(), p.GetNumber()) for fp in lay.pcb.GetFootprints() for p in fp.Pads() if p.HasHole()}
    pads = [(fp, p) for fp in lay.pcb.GetFootprints() if fp.GetReference() != lay.bga for p in fp.Pads()
            if not p.HasHole() and p.GetNetname().startswith(POWER_PREFIX) and (fp.GetReference(), p.GetNumber()) not in via_in_pad]
    for fp, p in sorted(pads, key=lambda fp_p: fp_p[1].GetNetname() == 'GND'):  # GND last: the outer pours also tie it
        net = p.GetNetname()
        cx, cy = P(fp.GetPosition())
        px, py = P(p.GetPosition())
        bb = p.GetBoundingBox()
        half = max(pcbnew.ToMM(bb.GetWidth()), pcbnew.ToMM(bb.GetHeight())) / 2
        away = math.atan2(py - cy, px - cx) if (px, py) != (cx, cy) else 0.0
        axis = 0.0 if bb.GetWidth() >= bb.GetHeight() else math.pi / 2   # along the pad: fine-pitch connector rows
        angles = [away + (k + 1) // 2 * (1 if k % 2 else -1) * math.pi / 8 for k in range(16)] + [axis, axis + math.pi]
        side = 'B' if fp.IsFlipped() else 'F'
        done = False
        for dist in (half + 0.35, half + 0.6, half + 0.9, half + 1.2, half + 1.6, half + 2.5, half + 3.5, half + 5.0):
            for a in angles:
                x, y = px + dist * math.cos(a), py + dist * math.sin(a)
                x, y = round(x / 0.05) * 0.05, round(y / 0.05) * 0.05
                if ok(px, py, x, y, net, side):
                    layer = pcbnew.B_Cu if side == 'B' else pcbnew.F_Cu
                    lay.track(net, [(px, py), (x, y)], 0.25, layer, locked=True)  # KRT never rips locked copper
                    lay.via(net, x, y, locked=True)
                    obst.append(('circle', ((x, y), VIA_D / 2), net, 'all'))
                    obst.append(('seg', ((px, py), (x, y), 0.125), net, side))
                    added += 1
                    done = True
                    break
            if done:
                break
        if not done:   # no room for its own via: a via of the net within reach, the stub alone
            near = sorted((math.hypot(g[0][0] - px, g[0][1] - py), g[0]) for kind, g, n, _ in obst if kind == 'circle' and n == net)
            reuse = next((xy for d, xy in near if d < half + 1.6 and stub_free(px, py, *xy, net, side)), None)
            if reuse:
                lay.track(net, [(px, py), reuse], 0.25, pcbnew.B_Cu if side == 'B' else pcbnew.F_Cu, locked=True)
                obst.append(('seg', ((px, py), reuse, 0.125), net, side))
                done = True
        if not done and (plane_of(net, px, py) or net in plane_nets):   # off its plane: left to the router, so say it
            missed.append(f'{fp.GetReference()}.{p.GetNumber()}' + ('' if plane_of(net, px, py) else ' (off plane)'))
    return added, missed
