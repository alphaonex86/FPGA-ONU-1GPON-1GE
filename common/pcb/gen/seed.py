# SPDX-License-Identifier: CERN-OHL-S-2.0
"""A newer generated board seeded with the part of an adopted routing that is still good.
seed.py OLD_GENERATED ADOPTED NEW_GENERATED OUT X_MM [NET...] [-NET...] [NET>X...] [REF:DX,DY...]
Kept: the routed copper of the nets whose pads are all west of X_MM (layout mm) and of the nets named, when none of
their pads moved and none of their tracks lies in a no-track area of the new board.  -NET: not kept after all.
NET>X: only that net's copper east of X (layout mm) is kept, whatever moved west of it (a matched pair's far part).
REF:DX,DY moves a part of the new board (mm), without its generated stubs.  A generated pad-to-via stub of the new board that lands on kept copper
is removed (the router ties that pad instead); any other clash is listed."""
import sys, collections
import pcbnew
from pcbgen import OX

def key(t):
    if t.GetClass() == 'PCB_VIA':
        return ('via', t.GetNetname(), t.GetPosition().x, t.GetPosition().y)
    a, b = sorted([(t.GetStart().x, t.GetStart().y), (t.GetEnd().x, t.GetEnd().y)])
    return ('track', t.GetNetname(), t.GetLayerName(), a, b)

def ends(t):
    return [t.GetPosition()] if t.GetClass() == 'PCB_VIA' else [t.GetStart(), t.GetEnd()]

def groups(items):
    """Items joined end to end, as lists."""
    at = collections.defaultdict(list)
    for i, t in enumerate(items):
        for e in ends(t):
            at[(t.GetNetname(), e.x, e.y)].append(i)
    seen, out = set(), []
    for i in range(len(items)):
        stack, group = [i], []
        while stack:
            j = stack.pop()
            if j not in seen:
                seen.add(j)
                group.append(items[j])
                for e in ends(items[j]):
                    stack += at[(items[j].GetNetname(), e.x, e.y)]
        if group:
            out.append(group)
    return out

def pads(board):
    nets = collections.defaultdict(set)
    for p in board.GetPads():
        nets[p.GetNetname()].add((p.GetParentFootprint().GetReference(), p.GetNumber(), p.GetPosition().x, p.GetPosition().y,
                                  p.GetParentFootprint().GetLayer()))
    return nets

GAP = pcbnew.FromMM(0.095)   # under the board's 0.1 mm clearance; the DRC run after this is the judge
def clash(a, b):
    if a.GetNetname() == b.GetNetname() or not a.GetBoundingBox().GetInflated(GAP).Intersects(b.GetBoundingBox()):
        return False
    return any(a.IsOnLayer(layer) and b.IsOnLayer(layer) and a.GetEffectiveShape(layer).Collide(b.GetEffectiveShape(layer), GAP)
               for layer in (pcbnew.F_Cu, pcbnew.B_Cu))

old, adopted, new = (pcbnew.LoadBoard(p) for p in sys.argv[1:4])
west = pcbnew.FromMM(float(sys.argv[5]) + OX)
options = sys.argv[6:]
named = {a for a in options if not a.startswith('-') and ':' not in a and '>' not in a}
east = {net: pcbnew.FromMM(float(x) + OX) for net, x in (a.split('>') for a in options if '>' in a)}
dropped = {a[1:] for a in options if a.startswith('-')}
left = set()   # where the moved parts' pads were: their generated stubs go too
for ref, move in (a.split(':') for a in options if ':' in a):
    dx, dy = (pcbnew.FromMM(float(v)) for v in move.split(','))
    f = new.FindFootprintByReference(ref)
    left |= {(p.GetPosition().x, p.GetPosition().y) for p in f.Pads()}
    f.SetPosition(pcbnew.VECTOR2I(f.GetPosition().x + dx, f.GetPosition().y + dy))
generated = {key(t) for t in old.GetTracks()}
routed = collections.defaultdict(list)
for t in adopted.GetTracks():
    if key(t) not in generated:
        routed[t.GetNetname()].append(t)
areas = [z for z in new.Zones() if z.GetIsRuleArea() and z.GetDoNotAllowTracks()]
inside = lambda t: t.GetClass() != 'PCB_VIA' and any(
    z.IsOnLayer(t.GetLayer()) and z.Outline().Collide(pcbnew.SEG(t.GetStart(), t.GetEnd()), t.GetWidth() // 2) for z in areas)
was, now = pads(adopted), pads(new)   # the adopted board's pads: the generator keeps a part where it was routed
names = new.GetNetsByName()
kept = []
for net, tracks in sorted(routed.items()):
    if net in east:
        tracks = [t for t in tracks if all(e.x >= east[net] for e in ends(t))]
    elif net in dropped or not (net in named or all(p[2] < west for p in now[net])):
        continue
    here = lambda pads: {p for p in pads if p[2] >= east.get(net, p[2])}
    if here(was[net]) != here(now[net]):
        print('not kept, a pad moved:', net)
    elif any(inside(t) for t in tracks):
        print('not kept, in a no-track area:', net)
    else:
        kept += tracks
mm = lambda v: round(pcbnew.ToMM(v) - OX, 2)
stubs = []
for group in groups(list(new.GetTracks())):
    if any((e.x, e.y) in left for t in group for e in ends(t)):
        stubs += group
        continue
    hit = next((k for t in group for k in kept if clash(t, k)), None)
    if hit is None:
        continue
    where = (group[0].GetNetname(), mm(group[0].GetPosition().x), mm(group[0].GetPosition().y), 'on', hit.GetNetname())
    if len(group) <= 3 and any(t.GetClass() == 'PCB_VIA' for t in group):
        print('generated stub removed:', *where)
        stubs += group
    else:
        print('CLASH, generated copper:', *where)
for p in [p for f in new.GetFootprints() for p in f.Pads()]:
    hit = next((k for k in kept if clash(p, k)), None)
    if hit is not None:
        print('CLASH, pad:', p.GetParentFootprint().GetReference(), p.GetNumber(), p.GetNetname(), 'on', hit.GetNetname(),
              mm(p.GetPosition().x), mm(p.GetPosition().y))
for t in stubs:
    new.Remove(t)
for t in kept:
    c = t.Duplicate()
    c.SetNet(names[t.GetNetname()])
    new.Add(c)
print('routed items', sum(len(v) for v in routed.values()), 'kept', len(kept))
if not pcbnew.SaveBoard(sys.argv[4], new):
    sys.exit('cannot save ' + sys.argv[4])
