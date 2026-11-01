# SPDX-License-Identifier: CERN-OHL-S-2.0
"""Write pcb/lib/bridge.kicad_sym: ECP5 from the Lattice pinout CSV (facts only) + small parts.

    python3 mksym.py <ECP5UM-25 pinout .csv from latticesemi.com> ../lib/bridge.kicad_sym
"""
import csv, re, sys
from sexp import Sym as S, dump
from kisch import box_symbol

csv_path, out = sys.argv[1], sys.argv[2]
balls = []  # (ball, function, bank, dual)
for row in csv.reader(l for l in open(csv_path) if not l.startswith('#')):
    if len(row) >= 8 and row[0] != 'PAD' and row[7] not in ('-', ''):
        balls.append((row[7], row[1], row[2], row[3]))

def key(b):
    m = re.match(r'([A-Z]+)(\d+)', b)
    return (m.group(1), int(m.group(2)))

io = lambda banks: sorted([(b, f + ('/' + d if d != '-' else ''), 'bidirectional')
                           for b, f, bank, d in balls if re.match(r'P[LRTB]\d+[A-D]$', f) and bank in banks], key=lambda p: key(p[0]))
def named(pattern, ptype):
    return sorted([(b, f, ptype) for b, f, bank, d in balls if re.match(pattern, f)], key=lambda p: (p[1], key(p[0])))
half = lambda l: (l[:(len(l) + 1) // 2], l[(len(l) + 1) // 2:])
config = named(r'(CCLK|CFG_\d|DONE|INITN|PROGRAMN|TCK|TDI|TMS)$', 'input') + named(r'TDO$', 'output')
serdes = named(r'HDTX', 'output') + named(r'HDRX|REFCLK', 'input') + named(r'RESERVED', 'no_connect')
serdes_pwr = named(r'VCCA0|VCCAUXA|VCCH', 'power_in')
power = named(r'VCC$|VCCAUX$|VCCIO\d', 'power_in')
gnd = named(r'GND$', 'power_in')
nc = named(r'NC$', 'no_connect')
units = [half(io('01')), half(io('23')), half(io('67')), (io('8'), config), (serdes, serdes_pwr),
         half(power), half(gnd), half(nc)]
assert sum(len(l) + len(r) for l, r in units) == len(balls) == 381, len(balls)
syms = [box_symbol('LFE5UM-25F-BG381', units, 'U', 'LFE5UM-25F-6BG381C',
                   'Package_BGA:Lattice_caBGA-381_17x17mm_Layout20x20_P0.8mm',
                   'ECP5 25k LUT, 1 SERDES DCU, caBGA381; units: banks 0/1, 2/3, 6/7, 8+config, SERDES, power, GND, NC')]
lvds_xo = lambda name, descr, pin1: box_symbol(name, [([('1', pin1, 'input'), ('2', 'NC', 'no_connect'), ('3', 'GND', 'power_in')],
                                                       [('6', 'VDD', 'power_in'), ('4', 'OUT+', 'tri_state'), ('5', 'OUT-', 'tri_state')])],
                                               'Y', name, 'Oscillator:Oscillator_SMD_SiTime_SiT9121-6Pin_3.2x2.5mm', descr)
syms.append(lvds_xo('XO_LVDS_3225', 'LVDS XO 3.3 V, 3.2x2.5 mm (SiT9121): OE high/open = on, low = outputs hi-Z', 'OE'))
syms.append(lvds_xo('VCXO_LVDS_3225', 'LVDS VCXO 3.3 V, 3.2x2.5 mm (SiT3821): VIN 0..VDD control voltage', 'VIN'))
syms.append(box_symbol('PCA9540B', [([('1', 'SCL', 'input'), ('2', 'SDA', 'bidirectional'), ('3', 'VDD', 'power_in'), ('6', 'VSS', 'power_in')],
                                     [('5', 'SC0', 'bidirectional'), ('4', 'SD0', 'bidirectional'), ('8', 'SC1', 'bidirectional'), ('7', 'SD1', 'bidirectional')])],
                       'U', 'PCA9540BD', 'Package_SO:SOIC-8_3.9x4.9mm_P1.27mm', '2-channel I2C mux, address 0x70'))
syms.append(box_symbol('SFP_HOST', [([('3', 'TX_DISABLE', 'input'), ('2', 'TX_FAULT', 'open_collector'), ('5', 'SCL', 'input'),
                                      ('4', 'SDA', 'bidirectional'), ('6', 'MOD_ABS', 'passive'), ('7', 'RS0', 'input'),
                                      ('9', 'RS1', 'input'), ('8', 'RX_LOS', 'open_collector'), ('16', 'VccT', 'power_in'),
                                      ('15', 'VccR', 'power_in')],
                                     [('18', 'TD+', 'input'), ('19', 'TD-', 'input'), ('13', 'RD+', 'output'), ('12', 'RD-', 'output'),
                                      ('1', 'VeeT', 'power_in'), ('17', 'VeeT', 'passive'), ('20', 'VeeT', 'passive'),
                                      ('10', 'VeeR', 'power_in'), ('11', 'VeeR', 'passive'), ('14', 'VeeR', 'passive'),
                                      ('CAGE', 'CAGE', 'passive')])],
                       'J', 'SFP', 'Connector:Connector_SFP_and_Cage', 'SFP host connector + cage, SFF-8431 pin names (pin 7/9 = RS0/RS1)'))
# RTL8290B + BOSA: functions known, balls to trace on the X100DG board (numbers are placeholders)
syms.append(box_symbol('RTL8290B_BOSA', [([('1', 'TD+', 'input'), ('2', 'TD-', 'input'), ('3', 'BEN+', 'input'), ('4', 'BEN-', 'input'),
                                           ('5', 'TX_DIS', 'input'), ('6', 'SCL', 'input'), ('7', 'SDA', 'bidirectional')],
                                          [('8', 'RD+', 'output'), ('9', 'RD-', 'output'), ('10', 'SD', 'output'),
                                           ('11', 'VCC', 'power_in'), ('12', 'GND', 'power_in')])],
                       'U', 'RTL8290B', '', 'RTL8290B + BOSA block from X100DG/X111W; pin numbers are placeholders until traced'))
lib = [S('kicad_symbol_lib'), [S('version'), 20251024], [S('generator'), 'kicad_symbol_editor'],
       [S('generator_version'), '10.0']] + syms
open(out, 'w').write(dump(lib) + '\n')
print('symbols', [s[1] for s in syms])
