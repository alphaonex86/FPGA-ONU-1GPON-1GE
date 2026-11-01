# SPDX-License-Identifier: CERN-OHL-S-2.0
"""Freerouting now and then lays a bad segment and autoroute.py then rejects the whole
iteration. Take that iteration anyway minus the segments the DRC flags (KRT re-lays them),
and keep it when it leaves fewer open connections than the board.

    python3 salvage.py <rejected it*.kicad_pcb> <board>.kicad_pcb
"""
import json, os, shutil, subprocess, sys, tempfile
import pcbnew
from finish import drc

def flagged_tracks(path):
    with tempfile.TemporaryDirectory() as d:
        report = os.path.join(d, 'drc.json')
        subprocess.run(['nice', '-n', '19', 'kicad-cli', 'pcb', 'drc', '--format', 'json', '-o', report, path],
                       capture_output=True, text=True)
        if not os.path.exists(report):
            sys.exit(f'kicad-cli drc failed on {path}')
        rep = json.load(open(report))
    return {i['uuid'] for v in rep['violations'] if v['severity'] == 'error' for i in v['items']}

if __name__ == '__main__':
    rejected, board = sys.argv[1], sys.argv[2]
    bad = flagged_tracks(rejected)
    pcb = pcbnew.LoadBoard(rejected)
    gone = [t for t in pcb.GetTracks() if t.m_Uuid.AsString() in bad and t.GetClass() != 'PCB_VIA' and not t.IsLocked()]
    for t in gone:
        pcb.Remove(t)
    pcb.Save(rejected)
    errors, open_new = drc(rejected)
    _, open_old = drc(board)
    print(f'salvage: dropped {len(gone)} segment(s) -> errors {dict(errors)}, open {open_new} (board {open_old})')
    if not errors and open_new < open_old:
        shutil.copy(rejected, board)
        print('salvage: kept')
