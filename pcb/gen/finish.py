# SPDX-License-Identifier: CERN-OHL-S-2.0
"""Last copper step on a routed board: GND pours on both outer layers, zone refill, DRC.

    python3 finish.py <board>.kicad_pcb
"""
import collections, json, os, subprocess, sys, tempfile
import pcbnew

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
    """Refill zones, save, return (errors by type, unconnected count)."""
    with tempfile.TemporaryDirectory() as d:
        report = os.path.join(d, 'drc.json')
        r = subprocess.run(['nice', '-n', '19', 'kicad-cli', 'pcb', 'drc', '--refill-zones', '--save-board',
                            '--format', 'json', '-o', report, path], capture_output=True, text=True)
        if not os.path.exists(report):
            sys.exit(f'kicad-cli drc failed ({r.returncode}): {r.stderr.strip()}')
        rep = json.load(open(report))
    errors = collections.Counter(v['type'] for v in rep['violations'] if v['severity'] == 'error')
    return errors, len(rep['unconnected_items'])

if __name__ == '__main__':
    path = sys.argv[1]
    pcb = pcbnew.LoadBoard(path)
    if outer_pours(pcb):
        pcb.Save(path)
    errors, open_items = drc(path)
    print(f'{path}: errors {dict(errors)}, unconnected {open_items}')
    sys.exit(1 if errors or open_items else 0)
