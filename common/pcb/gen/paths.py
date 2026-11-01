# SPDX-License-Identifier: CERN-OHL-S-2.0
"""Where things live: the shared library in common/pcb/lib, each board under its dev board's folder."""
import os, sys

REPO = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..', '..'))
LIB = os.path.join(REPO, 'common', 'pcb', 'lib')
BOARD_HOME = {'olt': 'de25-nano', 'onu': 'de25-nano'}
KI_LIB = '${KIPRJMOD}/../../../common/pcb/lib'   # LIB seen from any <dev board>/pcb/<board> folder

def board_dir(name):
    return os.path.join(REPO, BOARD_HOME[name], 'pcb', name)

assert all(os.path.normpath(os.path.join(board_dir(b), KI_LIB.split('/', 1)[1])) == LIB for b in BOARD_HOME)

if __name__ == '__main__':
    print(board_dir(sys.argv[1]))
