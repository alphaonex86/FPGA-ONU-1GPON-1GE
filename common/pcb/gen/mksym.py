# SPDX-License-Identifier: CERN-OHL-S-2.0
"""Write common/pcb/lib/bridge.kicad_sym: ECP5 from the Lattice pinout CSV (facts only) + small parts,
and common/pcb/lib/bridge.pretty (BOSA footprint). Without the CSV the ECP5 symbol already in the library is kept.

    python3 mksym.py [<ECP5UM-25 pinout .csv from latticesemi.com>] ../lib/bridge.kicad_sym
"""
import csv, math, os, re, sys
from sexp import Sym as S, dump, lib_symbol
from kisch import box_symbol
from paths import KI_LIB

def ecp5_symbol(csv_path):
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
    return box_symbol('LFE5UM-25F-BG381', units, 'U', 'LFE5UM-25F-6BG381C',
                      'Package_BGA:Lattice_caBGA-381_17x17mm_Layout20x20_P0.8mm',
                      'ECP5 25k LUT, 1 SERDES DCU, caBGA381; units: banks 0/1, 2/3, 6/7, 8+config, SERDES, power, GND, NC')

out = sys.argv[-1]
syms = [ecp5_symbol(sys.argv[1]) if len(sys.argv) > 2 else lib_symbol(out, 'LFE5UM-25F-BG381')]
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
syms.append(box_symbol('SY88236L', [([('6', 'DIN+', 'input'), ('7', 'DIN-', 'input'), ('9', 'BEN+', 'input'), ('10', 'BEN-', 'input'),
                                       ('11', 'TXDIS', 'input'), ('12', 'APCSET', 'passive'), ('13', 'MODSET', 'passive'),
                                       ('14', 'BIASMAX', 'passive'), ('15', 'MODMON', 'output'), ('23', 'BIASMON', 'output'),
                                       ('25', '~{APCFAULT}', 'output'), ('1', 'LOS/SD_SEL', 'input'), ('2', 'LOSLVL', 'passive'),
                                       ('3', 'LOS/SD', 'open_collector'), ('4', '~{JAM}', 'input')],
                                      [('19', 'MOD+', 'output'), ('18', 'MOD-', 'output'), ('21', 'BIAS', 'output'), ('22', 'MD', 'input'),
                                       ('27', 'RIN+', 'input'), ('28', 'RIN-', 'input'), ('31', 'ROUT+', 'output'), ('30', 'ROUT-', 'output'),
                                       ('26', 'RSSI', 'output'), ('17', 'VCC', 'power_in'), ('20', 'VCC', 'passive'), ('32', 'VCC', 'passive'),
                                       ('5', 'GND', 'power_in'), ('8', 'GND', 'passive'), ('16', 'GND', 'passive'), ('24', 'GND', 'passive'),
                                       ('29', 'GND', 'passive'), ('33', 'EP', 'passive')])],
                       'U', 'SY88236LMG', 'Package_DFN_QFN:QFN-32-1EP_5x5mm_P0.5mm_EP3.1x3.1mm_ThermalVias',
                       'Microchip SY88236L 2.5 Gb/s burst-mode laser driver + limiting amp (pin 26 = RSSI, per the pin diagram)'))
syms.append(box_symbol('TPS61391', [([('10', 'VIN', 'power_in'), ('13', 'EN', 'input'), ('14', 'AVCC', 'power_in'), ('15', 'AVCC', 'passive'),
                                       ('12', 'FB', 'input'), ('11', 'ISHORT', 'passive'), ('9', 'CAP', 'passive'), ('16', 'AGND', 'power_in')],
                                      [('8', 'SW', 'output'), ('6', 'MONIN', 'input'), ('5', 'APD', 'output'), ('4', 'MON1', 'output'),
                                       ('3', 'MON2', 'output'), ('1', 'NC', 'no_connect'), ('2', 'NC', 'no_connect'), ('7', 'GND', 'power_in'),
                                       ('17', 'EP', 'passive')])],
                       'U', 'TPS61391RTE', 'Package_DFN_QFN:WQFN-16-1EP_3x3mm_P0.5mm_EP1.68x1.68mm_ThermalVias',
                       'TI APD bias boost, 85 V, APD current mirror 4:5 (MON1) / 1:5 (MON2)'))
syms.append(box_symbol('SN65LVDT2DBV', [([('3', 'A', 'input'), ('4', 'B', 'input')],
                                          [('5', 'R', 'output'), ('1', 'VCC', 'power_in'), ('2', 'GND', 'power_in')])],
                       'U', 'SN65LVDT2DBVR', 'Package_TO_SOT_SMD:SOT-23-5', 'TI LVDS receiver, 110 ohm termination, LVTTL out'))
# BOSA of the X100DG: TX TO-56 (laser + monitor diode), RX TO-46 (APD + TIA); pads T1-T4 / R1-R5 (R5 centre lead, GND assumed), order unknown
syms.append(box_symbol('BOSA_GPON', [([('T1', 'LD_A', 'passive'), ('T2', 'LD_K', 'passive'), ('T3', 'MPD_A', 'passive'), ('T4', 'MPD_K', 'passive')],
                                       [('R1', 'VAPD', 'passive'), ('R2', 'VCC', 'power_in'), ('R3', 'OUT+', 'output'), ('R4', 'OUT-', 'output'),
                                        ('R5', 'GND', 'power_in')])],
                       'U', 'BOSA GPON', 'bridge:BOSA_GPON', 'GPON ONU BOSA (1310 nm DFB TX, 1490 nm APD-TIA RX) from the X100DG; pad order to measure'))
lib = [S('kicad_symbol_lib'), [S('version'), 20251024], [S('generator'), 'kicad_symbol_editor'],
       [S('generator_version'), '10.0']] + syms
open(out, 'w').write(dump(lib) + '\n')
print('symbols', [s[1] for s in syms])

# BOSA footprint (common/pcb/lib/x100dg-bosa-land.jpg): both TO cans stand on the board under the body, leads straight down;
# TX TO-56 4 leads in a + at the back (x = 0), RX TO-46 4 leads in an X + 1 centre lead (x = RX_X), fiber out at +x past
# the board edge. Pin circles from public TO-56/TO-46 headers, RX_X scaled from the photo; lead order still unknown.
BOSA_LEN, BOSA_R, BOSA_BACK, RX_X, TX_PCD, RX_PCD, DRILL, PAD = 15.0, 3.5, 3.2, 8.5, 2.0, 2.54, 0.7, 1.05
polar = lambda cx, d, deg: (round(cx + d / 2 * math.cos(math.radians(deg)), 3) + 0.0, round(d / 2 * math.sin(math.radians(deg)), 3) + 0.0)
TX_PADS = [(f'T{k + 1}', *polar(0, TX_PCD, 90 * k + 180)) for k in range(4)]
RX_PADS = [(f'R{k + 1}', *polar(RX_X, RX_PCD, 90 * k + 135)) for k in range(4)] + [('R5', RX_X, 0.0)]

def bosa_footprint():
    font = [S('effects'), [S('font'), [S('size'), 1, 1], [S('thickness'), 0.15]]]
    rect = lambda x0, y0, x1, y1, layer, w: [S('fp_rect'), [S('start'), x0, y0], [S('end'), x1, y1],
                                             [S('stroke'), [S('width'), w], [S('type'), S('solid')]], [S('fill'), S('no')], [S('layer'), layer]]
    body = [rect(-BOSA_BACK, -BOSA_R, BOSA_LEN, BOSA_R, 'F.Fab', 0.1),
            rect(-BOSA_BACK - 0.15, -BOSA_R - 0.15, BOSA_LEN - 0.3, BOSA_R + 0.15, 'F.SilkS', 0.12)]
    yard = rect(-BOSA_BACK - 0.4, -BOSA_R - 0.4, BOSA_LEN + 0.25, BOSA_R + 0.4, 'F.CrtYd', 0.05)
    pads = [[S('pad'), n, S('thru_hole'), S('circle'), [S('at'), round(x, 3), round(y, 3)], [S('size'), PAD, PAD],
             [S('drill'), DRILL], [S('layers'), '*.Cu', '*.Mask']] for n, x, y in TX_PADS + RX_PADS]
    def label_at(n, x, y):
        if n == 'R5':
            return x, 2.6
        cx, dist = (0.0, 2.4) if n[0] == 'T' else (RX_X, 2.7)
        k = dist / math.hypot(x - cx, y)
        return cx + (x - cx) * k, y * k
    labels = [[S('fp_text'), S('user'), n, [S('at'), *(round(v, 3) for v in label_at(n, x, y))], [S('layer'), 'F.SilkS'],
               [S('effects'), [S('font'), [S('size'), 0.8, 0.8], [S('thickness'), 0.12]]]] for n, x, y in TX_PADS + RX_PADS]
    fp = [S('footprint'), 'BOSA_GPON', [S('version'), 20260206], [S('generator'), 'mksym.py'], [S('layer'), 'F.Cu'],
          [S('descr'), 'GPON ONU BOSA of the X100DG, both TO cans down under the body, fiber over the edge; '
                       'TX TO-56 4 leads, RX TO-46 5 leads; spacing estimated from a photo, lead order unknown'],
          [S('property'), 'Reference', 'REF**', [S('at'), 7, 0], [S('layer'), 'F.Fab'], font],
          [S('property'), 'Value', 'BOSA_GPON', [S('at'), 7, 1.8], [S('layer'), 'F.Fab'], font],
          [S('attr'), S('through_hole')]] + body + [yard] + pads + labels + \
         [[S('model'), KI_LIB + '/3d/BOSA_GPON.step', [S('offset'), [S('xyz'), 0, 0, 0]],
           [S('scale'), [S('xyz'), 1, 1, 1]], [S('rotate'), [S('xyz'), 0, 0, 0]]]]
    d = os.path.join(os.path.dirname(out), 'bridge.pretty')
    os.makedirs(d, exist_ok=True)
    open(os.path.join(d, 'BOSA_GPON.kicad_mod'), 'w').write(dump(fp) + '\n')

bosa_footprint()
