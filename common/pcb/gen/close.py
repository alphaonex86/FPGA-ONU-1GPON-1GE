# SPDX-License-Identifier: CERN-OHL-S-2.0
"""Closes the open connections of a DRC report on F.Cu / B.Cu with a grid A* (KiCad's DRC stays the oracle).

    close.py BOARD DRC.json OUT [NET | LEADER:FOLLOWER]...

LEADER:FOLLOWER: a pair; the leader prefers room for two, the follower prefers the leader's side.
A net with an inner plane is closed by a via into the plane at each end that has none.
KEEP="X0,Y0,X1,Y1;..." (layout mm): boxes a net with no pad inside may not enter (a receiver's leads).
GRID / MARGIN (mm) in the environment: a finer raster for a tight spot; VIA_MM: the track length a via is worth. Existing copper is never moved. GND is left to the pours unless named.
"""
import fnmatch, heapq, json, math, os, re, sys
import numpy as np, pcbnew
from PIL import Image, ImageDraw
from pcbgen import OX, OY

G, MARGIN = float(os.environ.get('GRID', 0.05)), float(os.environ.get('MARGIN', 0.03))
VIA_D, VIA_DRILL, VIA_COST = 0.45, 0.2, round(float(os.environ.get('VIA_MM', 3)) / G)
DEAR = 3    # a pair's cells away from where it wants to be cost this many times a step
EDGE, HOLE_CLEAR, HOLE_HOLE, PAD_VIA = 0.3, 0.2, 0.25, 0.1
LAYERS = [pcbnew.F_Cu, pcbnew.B_Cu]
IU = pcbnew.FromMM
ERR = IU(0.005)

board_path, drc_path, out_path = sys.argv[1:4]
pairs = dict(a.split(':') for a in sys.argv[4:] if ':' in a)
only = [n for a in sys.argv[4:] for n in a.split(':')]
pcb = pcbnew.LoadBoard(board_path)
box = pcb.GetBoardEdgesBoundingBox()
X0, Y0 = box.GetX(), box.GetY()
W, H = round(pcbnew.ToMM(box.GetWidth()) / G) + 1, round(pcbnew.ToMM(box.GetHeight()) / G) + 1


def cell(p):
    """Drawing coordinates: PIL's pixel i is centred on i + 0.5, grid cell i on i."""
    return (p.x - X0) / IU(G) + 0.5, (p.y - Y0) / IU(G) + 0.5


SETTINGS = json.load(open(os.path.splitext(board_path)[0] + '.kicad_pro'))['net_settings']
CLASSES = {c['name']: c for c in SETTINGS['classes']}
CLASS_OF = {}


def netclass(name):
    if name not in CLASS_OF:
        match = [p['netclass'] for p in SETTINGS['netclass_patterns'] if fnmatch.fnmatchcase(name, p['pattern'])]
        CLASS_OF[name] = CLASSES[match[0] if match else 'Default']
    return CLASS_OF[name]


def clearance_of(item):
    return netclass(item.GetNetname())['clearance']


class Raster:
    def __init__(self):
        self.image = Image.new('1', (W, H), 0)
        self.draw = ImageDraw.Draw(self.image)

    def shape(self, item, layer, grow, fill=1):
        poly = pcbnew.SHAPE_POLY_SET()
        item.TransformShapeToPolygon(poly, layer, IU(grow), ERR, pcbnew.ERROR_OUTSIDE)
        for i in range(poly.OutlineCount()):
            line = poly.Outline(i)
            self.draw.polygon([cell(line.CPoint(j)) for j in range(line.PointCount())], fill=fill)

    def disc(self, centre, radius, fill=1):
        x, y = cell(centre)
        r = radius / G
        self.draw.ellipse([x - r, y - r, x + r, y + r], fill=fill)

    def area(self, zone, grow):
        outline = zone.Outline()
        for i in range(outline.OutlineCount()):
            line = outline.Outline(i)
            points = [cell(line.CPoint(j)) for j in range(line.PointCount())]
            self.draw.polygon(points, fill=1)
            self.draw.line(points + points[:1], fill=1, width=math.ceil(2 * grow / G))

    def frame(self, grow):
        n = math.ceil(grow / G) - 1
        for edge in ([0, 0, W, n], [0, H - 1 - n, W, H], [0, 0, n, H], [W - 1 - n, 0, W, H]):
            self.draw.rectangle(edge, fill=1)

    def array(self):
        return np.array(self.image, dtype=bool)


KEEP = [tuple(map(float, box.split(','))) for box in os.environ.get('KEEP', '').split(';') if box]


def foreign_boxes(net):
    """The KEEP boxes (drawing coordinates) holding no pad of the net."""
    pads = [p.GetPosition() for fp in pcb.GetFootprints() for p in fp.Pads() if p.GetNetCode() == net]
    for x0, y0, x1, y1 in KEEP:
        a, b = pcbnew.VECTOR2I(IU(x0 + OX), IU(y0 + OY)), pcbnew.VECTOR2I(IU(x1 + OX), IU(y1 + OY))
        if not any(a.x <= p.x <= b.x and a.y <= p.y <= b.y for p in pads):
            yield [*cell(a), *cell(b)]


def copper(layer):
    for fp in pcb.GetFootprints():
        for pad in fp.Pads():
            if pad.IsOnLayer(layer):
                yield pad
    for track in pcb.GetTracks():
        if track.IsOnLayer(layer):
            yield track


def holes():
    for fp in pcb.GetFootprints():
        for pad in fp.Pads():
            if pad.GetDrillSize().x:
                yield pad, pad.GetPosition(), pcbnew.ToMM(max(pad.GetDrillSize().x, pad.GetDrillSize().y)) / 2
    for track in pcb.GetTracks():
        if track.Type() == pcbnew.PCB_VIA_T:
            yield track, track.GetPosition(), pcbnew.ToMM(track.GetDrill()) / 2


def rasters(net, width, clear):
    """Blocked cells for a track centre on each layer, and for a via centre."""
    half = width / 2 + MARGIN
    blocked, via = [], Raster()
    for layer in LAYERS:
        tracks = Raster()
        for item in copper(layer):
            keep = max(clear, clearance_of(item))
            if item.GetNetCode() != net:
                tracks.shape(item, layer, keep + half)
                via.shape(item, layer, keep + VIA_D / 2 + MARGIN)
            elif item.Type() == pcbnew.PCB_PAD_T:
                via.shape(item, layer, PAD_VIA + VIA_D / 2)
        for item, centre, radius in holes():
            if item.GetNetCode() != net:
                tracks.disc(centre, radius + HOLE_CLEAR + half)
        for zone in pcb.Zones():
            if zone.GetIsRuleArea() and zone.IsOnLayer(layer) and zone.GetDoNotAllowTracks():
                tracks.area(zone, half)
        tracks.frame(EDGE + half)
        for box in foreign_boxes(net):
            tracks.draw.rectangle(box, fill=1)
        for item in copper(layer):  # the net's own pads stay enterable under a neighbour's clearance
            if item.GetNetCode() == net and item.Type() == pcbnew.PCB_PAD_T:
                tracks.disc(item.GetPosition(), G, fill=0)
        blocked.append(tracks.array())
    for item, centre, radius in holes():
        via.disc(centre, radius + HOLE_HOLE + VIA_DRILL / 2 + MARGIN)
    for zone in pcb.Zones():
        if zone.GetIsRuleArea() and zone.GetDoNotAllowVias():
            via.area(zone, VIA_D / 2 + MARGIN)
    via.frame(EDGE + VIA_D / 2 + MARGIN)
    for box in foreign_boxes(net):
        via.draw.rectangle(box, fill=1)
    return blocked, via.array()


MOVES = [(1, 0, 1.0), (-1, 0, 1.0), (0, 1, 1.0), (0, -1, 1.0),
         (1, 1, 1.4142), (1, -1, 1.4142), (-1, 1, 1.4142), (-1, -1, 1.4142)]


def beside(net, reach, swap=1.5):
    """Per layer: the cells farther than `reach` from the net's tracks' edges, and than `swap` from its vias (where
    the follower may change sides: it crosses while the two are on different layers)."""
    out = []
    for layer in LAYERS:
        near = Raster()
        for track in pcb.GetTracks():
            if track.GetNetCode() == net and track.IsOnLayer(layer):
                near.shape(track, layer, reach)
                if track.Type() == pcbnew.PCB_VIA_T:
                    near.disc(track.GetPosition(), swap)
        out.append(~near.array())
    return out


def plane(net):
    """Where a via ties the net to its inner plane (cells), or None when it has none."""
    inner = [z for z in pcb.Zones() if not z.GetIsRuleArea() and not any(z.IsOnLayer(layer) for layer in LAYERS)]
    own = [z for z in inner if z.GetNetCode() == net]
    if not own:
        return None
    mine, above = Raster(), Raster()
    for zone in own:
        mine.area(zone, 0)
        for other in inner:
            if (other.GetNetCode() != net and other.GetAssignedPriority() > zone.GetAssignedPriority()
                    and other.GetLayerSet().Contains(zone.GetFirstLayer())):
                above.area(other, 0.5)
    return mine.array() & ~above.array()


def search(blocked, via, starts, goals, dear=None, sites=None):
    """A* over (layer, x, y); returns the cell path or None. `dear`: per layer, the cells that cost DEAR steps.
    `sites`: any of these cells ends the search too (a via there closes the net)."""
    goal_xy = [(x, y) for _, x, y in goals]
    goals = set(goals)

    def left(x, y):
        if sites is not None:
            return 0
        best = 1e9
        for gx, gy in goal_xy:
            dx, dy = abs(x - gx), abs(y - gy)
            best = min(best, max(dx, dy) + 0.4142 * min(dx, dy))
        return 1.1 * best

    cost = np.full((2, H, W), np.inf, dtype=np.float32)
    came = {}
    heap = []
    for s in starts:
        cost[s[0], s[2], s[1]] = 0
        heapq.heappush(heap, (left(s[1], s[2]), 0.0, s))
    while heap:
        _, c, node = heapq.heappop(heap)
        layer, x, y = node
        if c > cost[layer, y, x]:
            continue
        if node in goals or (sites is not None and sites[y, x]):
            path = [node]
            while path[-1] in came:
                path.append(came[path[-1]])
            return path[::-1]
        steps = [(layer, x + dx, y + dy, d) for dx, dy, d in MOVES]
        if not via[y, x]:
            steps.append((1 - layer, x, y, VIA_COST))
        for nl, nx, ny, d in steps:
            if not (0 <= nx < W and 0 <= ny < H) or blocked[nl][ny, nx]:
                continue
            if nl != layer and blocked[layer][y, x]:
                continue
            nc = c + (d * DEAR if dear is not None and nl == layer and dear[nl][ny, nx] else d)
            if nc < cost[nl, ny, nx]:
                cost[nl, ny, nx] = nc
                came[(nl, nx, ny)] = node
                heapq.heappush(heap, (nc + left(nx, ny), nc, (nl, nx, ny)))
    return None


def reach(blocked, via, starts, limit=400000):
    """Cells a flood from the starts gets to (capped), and its bounding box in layout mm."""
    seen, todo = set(starts), list(starts)
    while todo and len(seen) < limit:
        layer, x, y = todo.pop()
        steps = [(layer, x + dx, y + dy) for dx, dy, _ in MOVES[:4]]
        if not via[y, x]:
            steps.append((1 - layer, x, y))
        for n in steps:
            if 0 <= n[1] < W and 0 <= n[2] < H and not blocked[n[0]][n[2], n[1]] and n not in seen:
                seen.add(n)
                todo.append(n)
    xs, ys = [n[1] for n in seen], [n[2] for n in seen]
    return len(seen), (min(xs) * G, min(ys) * G, max(xs) * G, max(ys) * G)


def ends(item):
    """Grid cells of a DRC item's position on the layers it names."""
    layers = [i for i, name in enumerate(('F.Cu', 'B.Cu')) if name in item['description']]
    x = round((IU(item['pos']['x']) - X0) / IU(G))
    y = round((IU(item['pos']['y']) - Y0) / IU(G))
    return [(layer, x, y) for layer in layers]


def point(x, y):
    return pcbnew.VECTOR2I(X0 + x * IU(G), Y0 + y * IU(G))


def lay(path, net, width, exact):
    """Tracks and vias of a cell path; `exact` are the two items' real positions."""
    corners = [path[0]]
    for before, here, after in zip(path, path[1:], path[2:]):
        straight = (here[1] - before[1], here[2] - before[2]) == (after[1] - here[1], after[2] - here[2])
        if here[0] != before[0] or here[0] != after[0] or not straight:
            corners.append(here)
    corners.append(path[-1])
    spots = [exact[0]] + [point(x, y) for _, x, y in corners] + [exact[1]]
    layers = [corners[0][0]] + [c[0] for c in corners] + [corners[-1][0]]
    for a, b, la, lb in zip(spots, spots[1:], layers, layers[1:]):
        if la != lb:
            v = pcbnew.PCB_VIA(pcb)
            v.SetPosition(a)
            v.SetWidth(pcbnew.F_Cu, IU(VIA_D))
            v.SetDrill(IU(VIA_DRILL))
            v.SetLayerPair(pcbnew.F_Cu, pcbnew.B_Cu)
            v.SetNetCode(net)
            pcb.Add(v)
        elif a != b:
            t = pcbnew.PCB_TRACK(pcb)
            t.SetStart(a)
            t.SetEnd(b)
            t.SetWidth(IU(width))
            t.SetLayer(LAYERS[la])
            t.SetNetCode(net)
            pcb.Add(t)


opens = []
for u in json.load(open(drc_path)).get('unconnected_items', []):
    name = re.search(r'\[([^\]]+)\]', u['items'][0]['description']).group(1)
    if (name in only) if only else name != 'GND':
        a, b = u['items']
        span = math.hypot(a['pos']['x'] - b['pos']['x'], a['pos']['y'] - b['pos']['y'])
        opens.append((span, name, a, b))
failed = 0
for span, name, a, b in sorted(opens, key=lambda o: only.index(o[1]) if only else o[0]):
    net = pcb.FindNet(name).GetNetCode()
    width, clear = netclass(name)['track_width'], netclass(name)['clearance']
    blocked, via = rasters(net, width, clear)
    starts, goals = ends(a), ends(b)
    pitch = width + netclass(name)['diff_pair_gap']
    dear = None
    if name in pairs:
        dear = rasters(net, width + 2 * pitch, clear)[0]
    elif name in pairs.values():
        leader = next(l for l, f in pairs.items() if f == name)
        dear = beside(pcb.FindNet(leader).GetNetCode(), pitch + G - width / 2)
    sites = plane(net)
    loose = [i for i in (a, b) if ' - ' not in i['description']] if sites is not None else []
    for item in loose:
        path = search(blocked, via, ends(item), [], sites=sites & ~via)
        if path is None:
            failed += 1
            print(f'FAIL {name}: no way to a plane via from', item['description'])
            continue
        layer, x, y = path[-1]
        lay(path + [(1 - layer, x, y)], net, width, [pcbnew.VECTOR2I(IU(item['pos']['x']), IU(item['pos']['y'])), point(x, y)])
        blocked, via = rasters(net, width, clear)
        print(f'ok   {name} plane via at {x * G:.2f} {y * G:.2f}, {len(path) * G:.1f} mm')
    if loose:
        continue
    path = search(blocked, via, starts, goals, dear)
    if path is None:
        failed += 1
        print(f'FAIL {name} {span:.1f} mm (width {width}, clearance {clear})')
        for label, cells in (('  from', starts), ('  to', goals)):
            print(label, [(c[0], round(c[1] * G, 2), round(c[2] * G, 2), bool(blocked[c[0]][c[2], c[1]])) for c in cells],
                  'reaches', reach(blocked, via, cells))
        continue
    exact = [pcbnew.VECTOR2I(IU(i['pos']['x']), IU(i['pos']['y'])) for i in (a, b)]
    lay(path, net, width, exact)
    vias = sum(1 for p, q in zip(path, path[1:]) if p[0] != q[0])
    print(f'ok   {name} {len(path) * G:.1f} mm, {vias} via(s), width {width}')
if not pcbnew.SaveBoard(out_path, pcb):
    sys.exit('cannot save ' + out_path)
sys.exit(1 if failed else 0)
