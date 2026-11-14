# SPDX-License-Identifier: CERN-OHL-S-2.0
"""A board as the generator lays it (placed, cells, no router copper) in HOME/pcb/BOARD instead of its own folder,
which holds the routed board: the input of seed.py.

    unrouted.py BOARD HOME
"""
import os, runpy, sys
import paths

board, home = sys.argv[1:3]
here = os.path.dirname(os.path.abspath(__file__))
paths.BOARD_HOME[board] = os.path.abspath(home)
os.makedirs(paths.board_dir(board), exist_ok=True)
for script in ('gen.py', 'place.py'):
    sys.argv = [script, board]
    runpy.run_path(os.path.join(here, script), run_name='__main__')
