# SPDX-License-Identifier: CERN-OHL-S-2.0
"""Write common/pcb/lib/bridge.kicad_sym: the ECP5s from the Lattice pinout CSVs (facts only) + small parts,
and common/pcb/lib/bridge.pretty (BOSA, shield clip and HSMC card connector footprints). Without its CSV an ECP5 symbol already in the library is kept.

    python3 mksym.py [<ECP5UM-25 and/or ECP5UM-45 pinout .csv from latticesemi.com>] ../lib/bridge.kicad_sym
"""
import csv, math, os, re, sys
from sexp import Sym as S, dump, lib_symbol
from kisch import box_symbol
from hsmcpins import pinout as hsmc_pinout
from paths import KI_LIB

ECP5 = {'25': ('6', '25k LUT, 1 SERDES DCU'), '45': ('8', '45k LUT, 2 SERDES DCUs')}   # density: speed grade bought, what

def ecp5_symbol(csv_path, density):
    rows = list(csv.reader(l for l in open(csv_path, encoding='latin-1') if not l.startswith('#')))
    col = next(r for r in rows if r and r[0] == 'PAD').index('CABGA381')
    balls = []  # (ball, function, bank, dual)
    for row in rows:
        if len(row) > col and row[0] != 'PAD' and row[col] not in ('-', ''):
            balls.append((row[col], row[1], row[2], row[3]))
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
    serdes_pwr = named(r'VCCA\d|VCCAUXA|VCCH', 'power_in')
    power = named(r'VCC$|VCCAUX$|VCCIO\d', 'power_in')
    gnd = named(r'GND$', 'power_in')
    nc = named(r'NC$', 'no_connect')
    units = [half(io('01')), half(io('23')), half(io('67')), (io('8'), config), (serdes, serdes_pwr),
             half(power), half(gnd), half(nc)]
    assert sum(len(l) + len(r) for l, r in units) == len(balls) == 381, len(balls)
    return box_symbol(f'LFE5UM-{density}F-BG381', units, 'U', f'LFE5UM-{density}F-{ECP5[density][0]}BG381C',
                      'Package_BGA:Lattice_caBGA-381_17x17mm_Layout20x20_P0.8mm',
                      f'ECP5 {ECP5[density][1]}, caBGA381; units: banks 0/1, 2/3, 6/7, 8+config, SERDES, power, GND, NC')

out = sys.argv[-1]
csvs = {re.search(r'ECP5UM-(\d+)', open(p, encoding='latin-1').readline()).group(1): p for p in sys.argv[1:-1]}   # "# Pin Out For ECP5UM-45"
syms = [ecp5_symbol(csvs[d], d) if d in csvs else lib_symbol(out, f'LFE5UM-{d}F-BG381') for d in ECP5]
XO_3225 = 'Oscillator:Oscillator_SMD_SiTime_SiT9121-6Pin_3.2x2.5mm'
def lvds_xo(name, descr, pin1, pin2=('NC', 'no_connect'), footprint=XO_3225):
    return box_symbol(name, [([('1', pin1, 'input'), ('2', *pin2), ('3', 'GND', 'power_in')],
                              [('6', 'VDD', 'power_in'), ('4', 'OUT+', 'tri_state'), ('5', 'OUT-', 'tri_state')])],
                      'Y', name, footprint, descr)
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
syms.append(box_symbol('SY88349NDL', [([('1', 'DIN', 'input'), ('4', '/DIN', 'input'), ('2', 'VREF', 'output'), ('6', 'RESET', 'input'),
                                         ('10', '/AUTO_RESET', 'input'), ('13', 'LOS/SD_SEL', 'input'), ('14', 'LOS/SDLVL', 'passive'),
                                         ('15', 'JAM', 'input')],
                                        [('12', 'DOUT', 'output'), ('9', '/DOUT', 'output'), ('7', 'SD/LOS', 'open_collector'),
                                         ('5', 'VCC', 'power_in'), ('16', 'VCC', 'passive'), ('3', 'GND', 'power_in'),
                                         ('8', 'GND', 'passive'), ('11', 'GND', 'passive'), ('17', 'EP', 'passive')])],
                       'U', 'SY88349NDLMG', 'Package_DFN_QFN:QFN-16-1EP_3x3mm_P0.5mm_EP1.7x1.7mm_ThermalVias',
                       'Microchip SY88349NDL 2.5 Gb/s burst-mode limiting amp, CML out, 3x3 QFN-16; same pins: SY88149NDL (1.25 Gb/s, LVPECL out)'))
syms.append(box_symbol('SY88993AV', [([('2', 'DIN', 'input'), ('3', '/DIN', 'input'), ('4', 'VREF', 'output'), ('5', 'LOSLVL', 'passive'),
                                        ('1', '/EN', 'input')],
                                       [('9', 'DOUT', 'output'), ('8', '/DOUT', 'output'), ('7', 'LOS', 'open_collector'),
                                        ('10', 'VCC', 'power_in'), ('6', 'GND', 'power_in')])],
                       'U', 'SY88993AVKG', 'Package_SO:MSOP-10_3x3mm_P0.5mm',
                       'Microchip SY88993AV 3.2 Gb/s limiting amp, CML out, LOS open collector (high: no signal), MSOP-10'))
syms.append(box_symbol('MAX3643', [([('2', 'IN+', 'input'), ('3', 'IN-', 'input'), ('5', 'BEN+', 'input'), ('6', 'BEN-', 'input'),
                                      ('8', '~{EN}', 'input'), ('21', 'VMSET', 'input'), ('20', 'MODSET', 'passive'), ('23', 'VBSET', 'input'),
                                      ('24', 'BIASSET', 'passive'), ('10', 'IMAX', 'passive'), ('22', 'VREF', 'output'), ('12', 'MDIN', 'input')],
                                     [('16', 'OUT+', 'output'), ('17', 'OUT-', 'output'), ('14', 'BIAS+', 'output'), ('13', 'BIAS-', 'output'),
                                      ('11', 'MDOUT', 'output'), ('9', 'BCMON', 'output'), ('7', 'BENOUT', 'output'), ('1', 'VCCA', 'power_in'),
                                      ('4', 'VCCS', 'power_in'), ('15', 'VCCO', 'power_in'), ('18', 'VCCO', 'passive'), ('19', 'GND', 'power_in'),
                                      ('25', 'EP', 'passive')])],
                       'U', 'MAX3643ETG+', 'Package_DFN_QFN:TQFN-24-1EP_4x4mm_P0.5mm_EP2.6x2.6mm_ThermalVias',
                       'Maxim MAX3643 155 Mb/s - 2.5 Gb/s burst-mode laser driver: no APC, monitor diode sample and hold, TQFN-24'))
# Receive can of a GPON BOSA (TO-46 APD-TIA), leads per the XL-BCB2111-LS-02 drawing; the TX can is not soldered
syms.append(box_symbol('BOSA_RX', [([('1', 'VCC', 'power_in'), ('2', 'VAPD', 'passive'), ('4', 'GND', 'power_in')],
                                     [('5', 'OUT+', 'output'), ('3', 'OUT-', 'output')])],
                       'U', 'BOSA RX', 'bridge:BOSA_RX_TO46', 'GPON BOSA receive can only (APD-TIA): the laser can stays unconnected'))
# 7 x 5 mm LVDS VCXOs share this land and pin order (SiT3372, Si515 / Si550, CTS 356): pin 2 enables, high or open
syms.append(box_symbol('VCXO_LVDS_7050', [([('1', 'VIN', 'input'), ('2', 'OE', 'input'), ('3', 'GND', 'power_in'), ('7', 'EP', 'passive')],
                                            [('6', 'VDD', 'power_in'), ('4', 'OUT+', 'tri_state'), ('5', 'OUT-', 'tri_state')])],
                       'Y', 'VCXO_LVDS_7050', 'bridge:VCXO_7050_EP',
                       'LVDS VCXO 3.3 V, 7x5 mm, 6 pads 2.54 mm pitch + centre pad (GND): VIN control voltage, OE high = on'))
# Any pigtail BOSA lying on the board: its leads are formed by hand to these pads, so the pin order of the cans is not in the copper
syms.append(box_symbol('BOSA_PADS', [([('T1', 'MPD_A', 'passive'), ('T2', 'LD_A', 'passive'), ('T3', 'LD_K', 'passive'), ('T4', 'CASE', 'passive')],
                                       [('R1', 'VAPD', 'passive'), ('R2', 'GND', 'power_in'), ('R3', 'OUT+', 'output'), ('R4', 'OUT-', 'output'),
                                        ('R5', 'VCC', 'power_in'), ('S', 'STRAP', 'passive')])],
                       'U', 'BOSA', 'bridge:BOSA_PADS', 'Pigtail BOSA (laser can + APD-TIA can), leads lap-soldered to pad rows; '
                       'a monitor diode cathode lead of its own goes to LD_A'))
syms.append(box_symbol('SHIELD_CLIP', [([('1', 'GND', 'passive')], [])], 'SH', 'ICSRC6508SFR', 'bridge:SHIELD_CLIP_6.5x0.8',
                       'SMD clip for a 0.2 mm shield can wall (Innochips ICSRC6508SFR, LCSC C238199)'))
def hsmc_symbol():
    fn = hsmc_pinout()
    first = {f: min(p for p in fn if fn[p] == f) for f in ('3.3V', '12V', 'GND')}   # one typed power pin each, the rest passive (ERC)
    pin = lambda p: (str(p), fn[p], {first['3.3V']: 'power_out', first['12V']: 'power_out', first['GND']: 'power_in'}.get(p, 'passive'))
    half = lambda a, b: ([pin(p) for p in range(a, b, 2)], [pin(p) for p in range(a + 1, b, 2)])   # odd row | even row
    return box_symbol('HSMC_CARD', [half(1, 41), half(41, 101), half(101, 161), ([pin(p) for p in range(161, 173)], [])],
                      'J', 'ASP-122952-01', 'bridge:HSMC_ASP-122952-01',
                      'HSMC card connector (Samtec ASP-122952-01), HSMC spec pin functions; units: banks 1, 2, 3, ground bars')
syms.append(hsmc_symbol())
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

FONT = [S('effects'), [S('font'), [S('size'), 1, 1], [S('thickness'), 0.15]]]
rect = lambda x0, y0, x1, y1, layer, w: [S('fp_rect'), [S('start'), x0, y0], [S('end'), x1, y1],
                                         [S('stroke'), [S('width'), w], [S('type'), S('solid')]], [S('fill'), S('no')], [S('layer'), layer]]

def write_footprint(name, descr, attr, items, ref_at):
    fp = [S('footprint'), name, [S('version'), 20260206], [S('generator'), 'mksym.py'], [S('layer'), 'F.Cu'], [S('descr'), descr],
          [S('property'), 'Reference', 'REF**', [S('at'), *ref_at], [S('layer'), 'F.Fab'], FONT],
          [S('property'), 'Value', name, [S('at'), ref_at[0], ref_at[1] + 1.8], [S('layer'), 'F.Fab'], FONT],
          [S('attr'), S(attr)]] + items + \
         [[S('model'), f'{KI_LIB}/3d/{name}.step', [S('offset'), [S('xyz'), 0, 0, 0]],
           [S('scale'), [S('xyz'), 1, 1, 1]], [S('rotate'), [S('xyz'), 0, 0, 0]]]]
    d = os.path.join(os.path.dirname(out), 'bridge.pretty')
    os.makedirs(d, exist_ok=True)
    open(os.path.join(d, name + '.kicad_mod'), 'w').write(dump(fp) + '\n')

def bosa_footprint():
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
    write_footprint('BOSA_GPON', 'GPON ONU BOSA of the X100DG, both TO cans down under the body, fiber over the edge; '
                                 'TX TO-56 4 leads, RX TO-46 5 leads; spacing estimated from a photo, lead order unknown',
                    'through_hole', body + [yard] + pads + labels, (7, 0))

# HSMC card connector, Samtec ASP-122952-01 (QTH based, bank 1 differential -DP): odd pins on one row, even on the other,
# ground bars between them, 2 alignment pegs on the odd side (land pattern cross-checked against two open HSMC designs)
HSMC_ROW, HSMC_PEG = 3.09, (29.24, -2.03)
HSMC_BARS = [(-28.445, 2.54), (-23.175, 4.7), (-16.825, 4.7), (-11.555, 2.54), (-8.445, 2.54), (-3.175, 4.7)]

def hsmc_x(p):
    k = (p - 1) // 2
    return -27.25 + 1.5 * (k // 2) + 0.5 * (k % 2) if p <= 40 else (-7.25 if p <= 100 else 12.75) + 0.5 * ((p - (41 if p <= 100 else 101)) // 2)

def hsmc_footprint():
    pads = [[S('pad'), str(p), S('smd'), S('rect'), [S('at'), hsmc_x(p), -HSMC_ROW if p % 2 else HSMC_ROW], [S('size'), 0.3, 1.45],
             [S('layers'), 'F.Cu', 'F.Paste', 'F.Mask']] for p in range(1, 161)]
    bars = HSMC_BARS + [(-x, w) for x, w in reversed(HSMC_BARS)]
    pads += [[S('pad'), str(161 + k), S('smd'), S('rect'), [S('at'), x, 0], [S('size'), w, 0.64],
              [S('layers'), 'F.Cu', 'F.Paste', 'F.Mask']] for k, (x, w) in enumerate(bars)]
    pads += [[S('pad'), '', S('np_thru_hole'), S('circle'), [S('at'), sx * HSMC_PEG[0], HSMC_PEG[1]], [S('size'), 1.0, 1.0],
              [S('drill'), 1.0], [S('layers'), '*.Cu', '*.Mask']] for sx in (-1, 1)]
    body = [rect(-30.0, -2.98, 30.0, 2.98, 'F.Fab', 0.1), *[[S('fp_line'), [S('start'), x, -2.98], [S('end'), x, 2.98], [S('stroke'), [S('width'), 0.12], [S('type'), S('solid')]],
              [S('layer'), 'F.SilkS']] for x in (-30.0, 30.0)],   # body ends only: the pad rows overlap the body outline
            [S('fp_circle'), [S('center'), -28.0, -4.3], [S('end'), -27.75, -4.3], [S('stroke'), [S('width'), 0.12], [S('type'), S('solid')]],
             [S('fill'), S('yes')], [S('layer'), 'F.SilkS']]]   # pin 1
    write_footprint('HSMC_ASP-122952-01', 'HSMC card connector, Samtec ASP-122952-01, 160 contacts 0.5 mm + 12 ground bars, '
                    'mounted on the card bottom', 'smd', body + [rect(-30.25, -4.6, 30.25, 4.07, 'F.CrtYd', 0.05)] + pads, (0, 5.5))

# XL-BCB2111-LS-02 RX can: 5 leads 0.45 mm on a 2.54 mm circle, 72 degrees apart, clockwise from VCC at the top:
# VCC, Isource (APD), Dout-, GND, Dout+. The BOSA body and its TX can lie beside the pads, TX leads cut.
def bosa_rx_footprint():
    pads = [[S('pad'), str(k + 1), S('thru_hole'), S('circle'), [S('at'), *polar(0, RX_PCD, -90 + 72 * k)], [S('size'), 0.9, 0.9],
             [S('drill'), 0.6], [S('layers'), '*.Cu', '*.Mask']] for k in range(5)]
    can = [[S('fp_circle'), [S('center'), 0, 0], [S('end'), 2.7, 0], [S('stroke'), [S('width'), w], [S('type'), S('solid')]],
            [S('fill'), S('no')], [S('layer'), layer]] for layer, w in (('F.SilkS', 0.12), ('F.Fab', 0.1), ('F.CrtYd', 0.05))]
    write_footprint('BOSA_RX_TO46', 'GPON BOSA receive can (TO-46 APD-TIA), 5 leads on a 2.54 mm circle (XL-BCB2111-LS-02 drawing); '
                    'pad 1 VCC, 2 Isource, 3 Dout-, 4 GND, 5 Dout+', 'through_hole', can + pads, (0, -3.8))

bosa_footprint()
bosa_rx_footprint()
hsmc_footprint()

# BOSA on its side, fibre toward +x, origin where the RX can's axis meets the body's. The laser can's leads leave the
# back of the body (10 mm long, hand formed) to the west row, the receiver can's to the north row; pads are long so
# bodies of other lengths fit by sliding along x. A wire over the body, soldered in the two S holes, holds and grounds it.
def bosa_pads_footprint():
    smd = lambda n, x, y, w, h: [S('pad'), n, S('smd'), S('roundrect'), [S('at'), x, y], [S('size'), w, h],
                                 [S('layers'), 'F.Cu', 'F.Paste', 'F.Mask'], [S('roundrect_rratio'), 0.25]]
    pads = [smd(f'T{k + 1}', -12.5, -3 + 2 * k, 5.0, 1.0) for k in range(4)]
    pads += [smd(f'R{k + 1}', -4 + 2 * k, -11.25, 1.0, 4.5) for k in range(5)]
    pads += [[S('pad'), 'S', S('thru_hole'), S('circle'), [S('at'), -4.5, y], [S('size'), 2.0, 2.0], [S('drill'), 1.0],
              [S('layers'), '*.Cu', '*.Mask']] for y in (-5.0, 5.0)]
    names = [('MPD', -16.2, -3), ('LD+', -16.2, -1), ('LD-', -16.2, 1), ('CASE', -16.6, 3),
             ('APD', -4, -14.4), ('GND', -2, -14.4), ('D+', 0, -14.4), ('D-', 2, -14.4), ('VCC', 4.2, -14.4)]
    text = [[S('fp_text'), S('user'), t, [S('at'), x, y, 90 if y < -14 else 0], [S('layer'), 'F.SilkS'],
             [S('effects'), [S('font'), [S('size'), 0.7, 0.7], [S('thickness'), 0.1]]]] for t, x, y in names]
    write_footprint('BOSA_PADS', 'Pigtail BOSA on its side, fibre toward +x: laser can leads to the west pad row (MPD, LD+, LD-, case), '
                    'receiver can leads to the north row (APD, GND, D+, D-, VCC), strap wire holes S',
                    'smd', [rect(-9.5, -3.4, 3.5, 3.4, 'F.Fab', 0.1), rect(-9.5, -3.4, 3.5, 3.4, 'F.SilkS', 0.12),
                            rect(-15.3, -13.8, 3.8, 6.3, 'F.CrtYd', 0.05)] + pads + text, (-3, 0))

# Innochips ICSRC6508SFR: land 6.7 x 0.9 mm, paste in three openings (drawing rev C)
def shield_clip_footprint():
    pad = [S('pad'), '1', S('smd'), S('rect'), [S('at'), 0, 0], [S('size'), 6.7, 0.9], [S('layers'), 'F.Cu', 'F.Mask']]
    paste = [[S('pad'), '', S('smd'), S('rect'), [S('at'), x, 0], [S('size'), w, 0.8], [S('layers'), 'F.Paste']]
             for x, w in ((-2.65, 1.1), (0, 1.0), (2.65, 1.1))]
    write_footprint('SHIELD_CLIP_6.5x0.8', 'Shield can clip Innochips ICSRC6508SFR (LCSC C238199), 6.5 x 0.8 mm, for a 0.2 mm wall',
                    'smd', [rect(-3.25, -0.4, 3.25, 0.4, 'F.Fab', 0.1), rect(-3.6, -0.7, 3.6, 0.7, 'F.CrtYd', 0.05), pad] + paste, (0, -1.5))

# SiT3372 7.0 x 5.0 mm land (datasheet rev 1.07): pad columns 4.15 mm apart, 2.54 mm pitch, centre pad 1.70 x 3.00 mm (GND,
# for its 0.24 W). Pads widened outward to 1.6 mm so the same land takes the ceramic 7 x 5 parts.
def vcxo_footprint():
    pads = [[S('pad'), str(n), S('smd'), S('rect'), [S('at'), x, y], [S('size'), 1.6, 1.45], [S('layers'), 'F.Cu', 'F.Paste', 'F.Mask']]
            for n, x, y in ((1, -2.25, -2.54), (2, -2.25, 0), (3, -2.25, 2.54), (4, 2.25, 2.54), (5, 2.25, 0), (6, 2.25, -2.54))]
    pads.append([S('pad'), '7', S('smd'), S('rect'), [S('at'), 0, 0], [S('size'), 1.7, 3.0], [S('layers'), 'F.Cu', 'F.Paste', 'F.Mask']])
    mark = [S('fp_circle'), [S('center'), -3.6, -3.2], [S('end'), -3.45, -3.2], [S('stroke'), [S('width'), 0.12], [S('type'), S('solid')]],
            [S('fill'), S('yes')], [S('layer'), 'F.SilkS']]   # pin 1
    write_footprint('VCXO_7050_EP', 'LVDS VCXO 7.0 x 5.0 mm, 6 pads + centre pad (SiT3372 land), body 5 mm along x',
                    'smd', [rect(-2.5, -3.5, 2.5, 3.5, 'F.Fab', 0.1), rect(-3.3, -3.75, 3.3, 3.75, 'F.CrtYd', 0.05), mark] + pads, (0, -4.6))

bosa_pads_footprint()
shield_clip_footprint()
vcxo_footprint()
