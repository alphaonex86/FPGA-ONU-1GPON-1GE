# SPDX-License-Identifier: CERN-OHL-S-2.0
"""Generate <dev board>/pcb/<board>/<board>.kicad_sch (+ project files): gen.py [board ...], default every board."""
import os, sys
sys.path.insert(0, os.path.dirname(__file__))
from boards import onu, olt, hsmc, sniffer, inline
from kisch import write_schematic
from pcbgen import write_project
from paths import ki_lib, board_dir

makers = {f.__name__: f for f in (onu, olt, hsmc, sniffer, inline)}
unknown = [n for n in sys.argv[1:] if n not in makers]
if unknown:
    sys.exit(f'no board {unknown}: {list(makers)}')
for board in (makers[n]() for n in sys.argv[1:] or makers):
    d = board_dir(board.name)
    os.makedirs(d, exist_ok=True)
    nets = board.nets()
    flagged = {n for n in nets if n.startswith('+') or n == 'GND'} | set(board.flag_nets)
    write_schematic(board, f'{d}/{board.name}.kicad_sch', flagged)
    write_project(f'{d}/{board.name}.kicad_pro', board)
    open(f'{d}/sym-lib-table', 'w').write(
        '(sym_lib_table\n  (version 7)\n  (lib (name "bridge")(type "KiCad")(uri "' + ki_lib(d) + '/bridge.kicad_sym")(options "")(descr "Bridge boards"))\n)\n')
    open(f'{d}/fp-lib-table', 'w').write(
        '(fp_lib_table\n  (version 7)\n  (lib (name "bridge")(type "KiCad")(uri "' + ki_lib(d) + '/bridge.pretty")(options "")(descr "Bridge boards"))\n)\n')
    single = sorted(n for n, p in nets.items() if len(p) == 1)
    print(board.name, len(board.parts), 'parts', len(nets), 'nets; single-pin nets:', single)
