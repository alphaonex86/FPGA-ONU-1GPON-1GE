# SPDX-License-Identifier: CERN-OHL-S-2.0
"""Generate pcb/<board>/<board>.kicad_sch (+ project files) for both bridge boards."""
import os, sys
sys.path.insert(0, os.path.dirname(__file__))
from boards import onu, olt
from kisch import write_schematic
from pcbgen import write_project

ROOT = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..')
POWER = {'+5V', '+3V3', '+1V1', '+1V1_SD', '+2V5', '+2V5_AUX', '+2V5_AUXA', '+1V1_VCCA', '+1V1_HTX', '+1V1_HRX',
         '+3V3_XO', '+3V3_OPT', '+3V3_SFPT', '+3V3_SFPR', 'GND'}

for board in (onu(), olt()):
    d = f'{ROOT}/{board.name}'
    os.makedirs(d, exist_ok=True)
    nets = board.nets()
    write_schematic(board, f'{d}/{board.name}.kicad_sch', POWER & set(nets))
    write_project(f'{d}/{board.name}.kicad_pro', board)
    open(f'{d}/sym-lib-table', 'w').write(
        '(sym_lib_table\n  (version 7)\n  (lib (name "bridge")(type "KiCad")(uri "${KIPRJMOD}/../lib/bridge.kicad_sym")(options "")(descr "Bridge boards"))\n)\n')
    single = sorted(n for n, p in nets.items() if len(p) == 1)
    print(board.name, len(board.parts), 'parts', len(nets), 'nets; single-pin nets:', single)
