# SPDX-License-Identifier: CERN-OHL-S-2.0
"""Last step on a routed board: footprint conventions (finish_footprint), GND pours on both outer
layers, zone refill, dangling copper removed, DRC.

    python3 finish.py <board>.kicad_pcb [<folder the board lives in, when this file is a copy being routed>]
"""
import collections, json, os, subprocess, sys, tempfile
import pcbnew, re
from pcbgen import finish_footprint
from paths import ki_lib
from sexp import TOK

UUID = re.compile(r'\(uuid "([^"]+)"\)')

def outer_pours(pcb):
    gnd = pcb.FindNet('GND')
    if gnd is None:
        sys.exit('no GND net')
    if any(z.GetNetname() == 'GND' and z.GetLayer() in (pcbnew.F_Cu, pcbnew.B_Cu) for z in pcb.Zones()):
        return False
    box = pcb.GetBoardEdgesBoundingBox()
    e = pcbnew.FromMM(0.3)
    for layer in (pcbnew.F_Cu, pcbnew.B_Cu):
        z = pcbnew.ZONE(pcb)
        z.SetLayer(layer); z.SetNet(gnd)
        ol = z.Outline(); ol.NewOutline()
        for x, y in ((box.GetLeft() + e, box.GetTop() + e), (box.GetRight() - e, box.GetTop() + e),
                     (box.GetRight() - e, box.GetBottom() - e), (box.GetLeft() + e, box.GetBottom() - e)):
            ol.Append(x, y)
        z.SetLocalClearance(pcbnew.FromMM(0.2)); z.SetMinThickness(pcbnew.FromMM(0.2))
        z.SetPadConnection(pcbnew.ZONE_CONNECTION_FULL)  # thermal spokes starve on these small pours
        z.SetIslandRemovalMode(pcbnew.ISLAND_REMOVAL_MODE_ALWAYS)
        pcb.Add(z)
    return True

def drc(path):
    """Refill zones, save, return (errors by type, unconnected count, uuids of dangling vias/tracks)."""
    with tempfile.TemporaryDirectory() as d:
        report = os.path.join(d, 'drc.json')
        r = subprocess.run(['nice', '-n', '19', 'kicad-cli', 'pcb', 'drc', '--refill-zones', '--save-board',
                            '--format', 'json', '-o', report, path], capture_output=True, text=True)
        if not os.path.exists(report):
            sys.exit(f'kicad-cli drc failed ({r.returncode}): {r.stderr.strip()}')
        rep = json.load(open(report))
    errors = collections.Counter(v['type'] for v in rep['violations'] if v['severity'] == 'error')
    dangling = {i['uuid'] for v in rep['violations'] if v['type'] in ('via_dangling', 'track_dangling') for i in v['items']}
    return errors, len(rep['unconnected_items']), dangling

def drop_dangling(path):
    """Vias and track ends joined on one side only (escapes the router bypassed): useless copper,
    removed while the board stays connected."""
    errors, open_items, dangling = drc(path)
    for _ in range(5):
        if not dangling or errors or open_items:
            break
        before = open(path).read()
        drop_items(path, dangling)
        after = drc(path)
        if after[0] or after[1]:   # a "dangling" piece joined two others: keep it
            open(path, 'w').write(before)
            print(f'{path}: dropping {len(dangling)} dangling item(s) opened the board, kept them')
            break
        errors, open_items, dangling = after
    return errors, open_items

def drop_items(path, uuids):
    """Cut the top-level via/segment/arc items with these uuids out of the file text (removing them
    through pcbnew breaks its SWIG state for the rest of the process)."""
    text, out, depth, start, last = open(path).read(), [], 0, 0, 0
    for m in TOK.finditer(text):
        t = m.group(0)
        if t == '(':
            depth += 1
            start = m.start() if depth == 2 else start
        elif t == ')':
            item = text[start:m.end()]
            if depth == 2 and item.startswith(('(via', '(segment', '(arc')) and UUID.search(item).group(1) in uuids:
                out.append(text[last:start]); last = m.end()
            depth -= 1
    out.append(text[last:])
    open(path, 'w').write(''.join(out))

if __name__ == '__main__':
    path = sys.argv[1]
    home = sys.argv[2] if len(sys.argv) > 2 else os.path.dirname(os.path.abspath(path))   # 3D model paths start there
    pcb = pcbnew.LoadBoard(path)
    if outer_pours(pcb) | any([finish_footprint(fp, ki_lib(home)) for fp in pcb.Footprints()]):
        pcbnew.SaveBoard(path, pcb, True)   # board only: the .kicad_pro stays write_project's
    errors, open_items = drop_dangling(path)
    print(f'{path}: errors {dict(errors)}, unconnected {open_items}')
    sys.exit(1 if errors or open_items else 0)
