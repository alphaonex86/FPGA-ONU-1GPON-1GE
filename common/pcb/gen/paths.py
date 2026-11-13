# SPDX-License-Identifier: CERN-OHL-S-2.0
"""Where things live: the shared library in common/pcb/lib, each board under its dev board's folder (the in-line
sniffer's in its own repository, checked out next to this one)."""
import os, sys

REPO = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..', '..'))
LIB = os.path.join(REPO, 'common', 'pcb', 'lib')
BOARD_HOME = {'olt': 'de25-nano', 'onu': 'de25-nano', 'hsmc': 'de25-standard', 'sniffer': 'sniffer',
              'inline': '../FPGA-USB-GPON-sniffer'}

def board_dir(name):
    return os.path.normpath(os.path.join(REPO, BOARD_HOME[name], 'pcb', name))

def ki_lib(folder):
    """LIB as a KiCad project in `folder` names it."""
    return '${KIPRJMOD}/' + os.path.relpath(LIB, folder)

KI_LIB = ki_lib(board_dir('sniffer'))   # in the library's own files: right for the boards of this repository

if __name__ == '__main__':
    print(board_dir(sys.argv[1]))
