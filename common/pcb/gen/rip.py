# SPDX-License-Identifier: CERN-OHL-S-2.0
"""Removes the tracks and vias of the named nets, except those wholly inside a kept box (layout mm); with
--inside, only those wholly inside the box.

    rip.py BOARD OUT X0 Y0 X1 Y1 [--inside] NET...
"""
import sys, pcbnew
from pcbgen import OX, OY
board, out = sys.argv[1:3]
x0, y0, x1, y1 = map(float, sys.argv[3:7])
nets = [n for n in sys.argv[7:] if n != '--inside']
only_inside = '--inside' in sys.argv
pcb = pcbnew.LoadBoard(board)
inside = lambda p: x0 <= pcbnew.ToMM(p.x) - OX <= x1 and y0 <= pcbnew.ToMM(p.y) - OY <= y1
gone = [t for t in pcb.GetTracks() if t.GetNetname() in nets and (inside(t.GetStart()) and inside(t.GetEnd())) == only_inside]
for t in gone:
    pcb.Remove(t)
print(len(gone), 'items removed')
if not pcbnew.SaveBoard(out, pcb):
    sys.exit('cannot save ' + out)
