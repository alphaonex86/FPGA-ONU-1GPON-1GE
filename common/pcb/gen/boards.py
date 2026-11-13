# SPDX-License-Identifier: CERN-OHL-S-2.0
"""Netlists: the two DE25-Nano SerDes bridge boards (ONU on GPIO_0, OLT on GPIO_1), the DE25-Standard HSMC card, the GPON sniffer
and the in-line GPON sniffer."""
import os, re
from kisch import Board
from hsmcpins import pinout as hsmc_pinout
from sexp import lib_symbol, pins

LIB = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'lib', 'bridge.kicad_sym')
R0402, C0402 = 'Resistor_SMD:R_0402_1005Metric', 'Capacitor_SMD:C_0402_1005Metric'
C0603, C0805 = 'Capacitor_SMD:C_0603_1608Metric', 'Capacitor_SMD:C_0805_2012Metric'
FB0603, L0805 = 'Inductor_SMD:L_0603_1608Metric', 'Inductor_SMD:L_0805_2012Metric'
RPACK = 'Resistor_SMD:R_Array_Convex_4x0402'

# Nano header: GPIO_x[i] -> header pin; pins 11/29 = 5 V / 3.3 V, 12/30 = GND
def hdr(i):
    return i + 1 if i <= 9 else i + 3 if i <= 25 else i + 5

# Physical assumption (check on the received Nano before ordering): seen from above with the
# board hanging outward (south), header pin 1 is at the east end and the even row is outward.
def hdr_x(pin):
    return -((pin - 1) // 2) * 2.54

def ecp5_balls(device):
    """{ball: function} of an ECP5 in caBGA381 (same land for every density, more balls bonded on the larger ones)."""
    return {p[1]: p[2] for p in pins(lib_symbol(LIB, f'LFE5UM-{device}-BG381'))}

ECP5_BALLS = ecp5_balls('25F')
ECP5_SPEED = {'25F': '6', '45F': '8'}   # grade bought
ROWS = 'ABCDEFGHJKLMNPRTUVWY'
def ball_xy(b):
    m = re.match(r'([A-Z])(\d+)', b)
    return (int(m.group(2)) - 10.5) * 0.8, (ROWS.index(m.group(1)) - 9.5) * 0.8

def assign_balls(signals, clock):
    """signals: {net: header pin}; header order west->east mapped onto rows A/B west->east.
    The forwarded clock, if any, lands on the nearest PCLKT ball."""
    io = [b for b, f in ECP5_BALLS.items() if re.match(r'P[TL]\d+[A-D]', f) and b[0] in 'AB']
    io.sort(key=lambda b: (ball_xy(b)[0], ball_xy(b)[1]))
    nets = sorted(signals, key=lambda n: (hdr_x(signals[n]), signals[n] % 2))
    assert len(nets) <= len(io), (len(nets), len(io))
    # spread the nets over the balls evenly
    step = len(io) / len(nets)
    chosen = [io[int(i * step)] for i in range(len(nets))]
    out = dict(zip(nets, chosen))
    pclk = [b for b in io if 'PCLKT' in ECP5_BALLS[b]]
    if clock and out[clock] not in pclk:
        free = [b for b in pclk if b not in chosen]
        target = min(free or pclk, key=lambda b: abs(ball_xy(b)[0] - ball_xy(out[clock])[0]))
        if target in chosen:  # swap with the net sitting there
            other = next(n for n, b in out.items() if b == target)
            out[other] = out[clock]
        out[clock] = target
    return out

class Bridge(Board):
    serdes_w = 0.2   # router width of the SerDes netclass
    serdes_nets = ()  # this board's own SerDes netclass patterns, besides pcbgen's
    power_nets = ()   # supply nets not named '+...': Power netclass too

    flag_nets = ()    # power inputs fed through a part (a power flag in the schematic)

    def r(self, value, a, b, fp=R0402, **fields):
        return self.add('R', 'Device', 'R', value, fp, {'1': a, '2': b}, **fields)

    def c(self, value, a, b='GND', fp=None, **fields):
        fp = fp or (C0603 if value in ('2.2u', '4.7u', '10u') else C0805 if value == '22u' else C0402)
        return self.add('C', 'Device', 'C', value, fp, {'1': a, '2': b}, **fields)

    def fb(self, a, b):
        return self.add('FB', 'Device', 'FerriteBead_Small', '120R@100MHz', FB0603, {'1': a, '2': b},
                        MPN='BLM18PG121SN1D')

    def decouple(self, rail, n100n=1, n10u=0, n22u=0):
        for _ in range(n100n): self.c('100n', rail)
        for _ in range(n10u): self.c('10u', rail)
        for _ in range(n22u): self.c('22u', rail)

    def buck(self, out, top, bottom):
        """TLV62569 from +5V (2 A): out = 0.6 V * (1 + top / bottom)."""
        fbn = f'FB_{out[1:]}'
        sw = f'SW_{out[1:]}'
        self.block = f'power:{out}'
        self.add('U', 'Regulator_Switching', 'TLV62569DBV', 'TLV62569DBVR', 'Package_TO_SOT_SMD:SOT-23-5',
                 {'VIN': '+5V', 'EN': '+5V', 'GND': 'GND', 'SW': sw, 'FB': fbn}, MPN='TLV62569DBVR')
        self.add('L', 'Device', 'L', '2.2u', 'Inductor_SMD:L_Cenker_CKCS252012', {'1': sw, '2': out},
                 MPN='SLO252012F2R2MTT')   # 2520, as the CKCS252012-2R2M
        self.r(top, out, fbn)
        self.r(bottom, fbn, 'GND')
        self.decouple('+5V', 0, 1)
        self.decouple(out, 0, 0, 2)

    def power(self, own_3v3=False):
        """own_3v3: +3V3 made here from +5V (3.32 V), else it comes in with +5V."""
        self.block = 'power'
        self.decouple('+5V', 0, 1, 1)
        self.decouple('+3V3', 0, 1, 2)
        for out in ('+1V1', '+1V1_SD'):   # 1.10 V
            self.buck(out, '100k', '120k')
        self.block = 'power:+2V5'
        self.add('U', 'Regulator_Linear', 'TLV75525PDBV', 'TLV75525PDBVR', 'Package_TO_SOT_SMD:SOT-23-5',
                 {'IN': '+3V3', 'EN': '+3V3', 'GND': 'GND', 'OUT': '+2V5'}, MPN='TLV75525PDBVR')
        self.c('1u', '+3V3')
        self.c('1u', '+2V5')
        if own_3v3:
            self.buck('+3V3', '453k', '100k')

    def ecp5(self, bus, outputs, misc, serdes, programn, spi, device='25F', flash=False):
        """bus: {net: header pin} on rows A/B, outputs: nets driven by the ECP5 (series R pack),
        misc: {net: ball}, serdes: {ball function: net}, spi: (sn, cclk, mosi, miso).
        flash: the ECP5 reads its own SPI flash (master SPI, sn = its chip select), else a host writes it (slave SPI)."""
        self.block = 'ecp5'
        all_balls = ecp5_balls(device)
        clock = next((n for n in bus if n.endswith('_CLK') and n not in outputs), None)
        balls = assign_balls(bus, clock) if bus else {}
        conn = {}
        for b, f in all_balls.items():
            if f == 'VCC': conn[b] = '+1V1'
            elif f == 'VCCAUX': conn[b] = '+2V5_AUX'
            elif f.startswith('VCCIO'): conn[b] = '+3V3'
            elif f == 'GND': conn[b] = 'GND'
            elif re.match(r'VCCA\d$', f): conn[b] = '+1V1_VCCA'   # an unused DCU's too (TN-02038)
            elif re.match(r'VCCAUXA\d$', f): conn[b] = '+2V5_AUXA'
            elif f.startswith('VCCHTX'): conn[b] = '+1V1_HTX'
            elif f.startswith('VCCHRX') and f in serdes: conn[b] = serdes[f]
            elif f in serdes: conn[b] = serdes[f]
        mode = 'CFG1' if flash else 'CFG0'   # CFG[2:0]: 010 master SPI, 001 slave SPI
        cfg = {'CFG_0': 'GND', 'CFG_1': 'GND', 'CFG_2': 'GND', 'PROGRAMN': programn, 'INITN': 'INITN', 'DONE': 'DONE',
               'TCK': 'TCK', 'TDI': 'TDI', 'TDO': 'TDO', 'TMS': 'TMS', 'CCLK': spi[1]}
        cfg[f'CFG_{mode[-1]}'] = mode
        select, idle = ('HOLDN/DI/BUSY/CSSPIN/CEN', 'SN/CSN') if flash else ('SN/CSN', 'HOLDN/DI/BUSY/CSSPIN/CEN')
        dual = {select: spi[0], 'D0/MOSI/IO0': spi[2], 'D1/MISO/IO1': spi[3], idle: 'HOLDN'}
        for b, f in all_balls.items():
            if f in cfg: conn[b] = cfg[f]
            if '/' in f and f.split('/', 1)[1] in dual: conn[b] = dual[f.split('/', 1)[1]]
        for net, b in balls.items():
            conn[b] = net + '_S' if net in outputs else net
        conn.update({b: n for n, b in misc.items()})
        value = f'LFE5UM-{device}-{ECP5_SPEED[device]}BG381C'
        self.add('U', LIB, f'LFE5UM-{device}-BG381', value,
                 'Package_BGA:Lattice_caBGA-381_17x17mm_Layout20x20_P0.8mm', conn, MPN=value)
        self.block = 'ecp5_support'
        outs = sorted(outputs, key=lambda n: ball_xy(balls[n]))  # (x, y): deterministic on A/B ties
        for i in range(0, len(outs), 4):  # 22 ohm source series, next to the ECP5
            group = outs[i:i + 4] + [None] * (4 - len(outs[i:i + 4]))
            c = {}
            for k, n in enumerate(group):
                if n:
                    c[str(k + 1)], c[str(8 - k)] = n + '_S', n
            self.add('RN', 'Device', 'R_Pack04', '22', RPACK, c, MPN='4D02WGJ0220TCE')
        self.decouple('+1V1', 10, 2)
        self.fb('+2V5', '+2V5_AUX'); self.decouple('+2V5_AUX', 4, 1)
        self.decouple('+3V3', 17, 4)
        self.fb('+1V1_SD', '+1V1_VCCA'); self.decouple('+1V1_VCCA', 2, 1)
        self.fb('+1V1_SD', '+1V1_HTX'); self.decouple('+1V1_HTX', 2, 1)
        self.fb('+1V1_SD', '+1V1_HRX'); self.decouple('+1V1_HRX', 2, 1)
        self.fb('+2V5', '+2V5_AUXA'); self.decouple('+2V5_AUXA', 2, 1)
        for net, value in ((mode, '4.7k'), (programn, '4.7k'), ('INITN', '4.7k'), ('DONE', '4.7k'),
                           (spi[0], '4.7k'), ('HOLDN', '10k'), (spi[1], '1k'),
                           ('TDI', '4.7k'), ('TMS', '4.7k')):  # TDO is an output
            self.r(value, net, '+3V3')
        self.r('4.7k', 'TCK', 'GND')
        self.add('J', 'Connector_Generic', 'Conn_01x06', 'JTAG', 'Connector_PinHeader_2.54mm:PinHeader_1x06_P2.54mm_Vertical',
                 {'1': '+3V3', '2': 'TCK', '3': 'TDI', '4': 'TDO', '5': 'TMS', '6': 'GND'})
        return balls

    def artix7(self, misc):
        """XC7A35T-2FGG484I, its supplies and configuration: master SPI from its flash (M = 001), JTAG pads (header not
        fitted); GTP quad 216
        lanes 0 (DS_) and 1 (US_) on MGTREFCLK0, lanes 2-3 unused (UG482 Table 5-6: RX to GND, TX open); XADC, thermal
        diode and key battery unused (UG480, UG470). misc: {net: ball}, every ball LVCMOS33."""
        self.block = 'power'
        self.decouple('+5V', 0, 1, 1)
        self.decouple('+3V3', 0, 1, 2)
        for out, top, bottom in (('+3V3', '453k', '100k'), ('+1V0', '100k', '150k'), ('+1V8', '200k', '100k')):
            self.buck(out, top, bottom)
        for out, sym, mpn in (('+1V0_GT', 'TLV75510PDBV', 'TLV74310PDBVR'), ('+1V2_GT', 'TLV75512PDBV', 'TLV74312PDBVR')):
            self.block = f'power:{out}'   # the GTP's analog supplies, their own LDOs (same SOT-23-5 pinout as the TLV755)
            self.add('U', 'Regulator_Linear', sym, mpn, 'Package_TO_SOT_SMD:SOT-23-5',
                     {'IN': '+1V8', 'EN': '+1V8', 'GND': 'GND', 'OUT': out}, MPN=mpn)
            self.c('1u', '+1V8'); self.c('1u', out)
        self.block = 'fpga'
        gt = {'MGTPRXP0_216': 'DS_RX_P', 'MGTPRXN0_216': 'DS_RX_N', 'MGTPTXP0_216': 'DS_TX_P', 'MGTPTXN0_216': 'DS_TX_N',
              'MGTPRXP1_216': 'US_RX_P', 'MGTPRXN1_216': 'US_RX_N', 'MGTPTXP1_216': 'US_TX_P', 'MGTPTXN1_216': 'US_TX_N',
              'MGTREFCLK0P_216': 'REFCLK_P', 'MGTREFCLK0N_216': 'REFCLK_N', 'MGTRREF_216': 'GT_RREF',
              **{f'MGTPRX{d}{k}_216': 'GND' for d in 'PN' for k in (2, 3)}}
        unused = ('VCCBATT_0', 'VP_0', 'VN_0', 'VREFP_0', 'VREFN_0', 'DXP_0', 'DXN_0', 'GNDADC_0', 'M1_0', 'M2_0')
        conn = {'VCCINT': '+1V0', 'VCCBRAM': '+1V0', 'VCCAUX': '+1V8', 'VCCADC_0': '+1V8', 'MGTAVCC': '+1V0_GT',
                'MGTAVTT': '+1V2_GT', 'GND': 'GND', **{n: 'GND' for n in unused},
                **{f'VCCO_{k}': '+3V3' for k in (0, 13, 14, 15, 16, 34, 35)}, 'CFGBVS_0': '+3V3', 'M0_0': '+3V3',
                'PROGRAM_B_0': 'PROGRAM_B', 'INIT_B_0': 'INIT_B', 'DONE_0': 'DONE', 'CCLK_0': 'CCLK',
                'TCK_0': 'TCK', 'TDI_0': 'TDI', 'TDO_0': 'TDO', 'TMS_0': 'TMS',
                'T19': 'FLASH_CS', 'P22': 'MOSI', 'R22': 'MISO', **gt, **{ball: net for net, ball in misc.items()}}   # FCS_B, D00_MOSI, D01_DIN
        self.add('U', 'FPGA_Xilinx_Artix7', 'XC7A35T-FGG484', 'XC7A35T-2FGG484I', 'Package_BGA:Xilinx_FGG484', conn,
                 MPN='XC7A35T-2FGG484I')
        self.block = 'fpga_support'
        self.r('100', 'GT_RREF', '+1V2_GT', MPN='100 ohm 1 %')
        self.flag_nets = ('GT_RREF',)   # MGTRREF is typed a power input
        self.decouple('+1V0', 10, 2, 1)
        self.decouple('+1V8', 5, 1)
        self.decouple('+3V3', 14, 3)
        for rail in ('+1V0_GT', '+1V2_GT'):
            self.decouple(rail, 4, 1)
        for net, value in (('PROGRAM_B', '4.7k'), ('INIT_B', '4.7k'), ('DONE', '330'), ('FLASH_CS', '4.7k'),
                           ('TDI', '4.7k'), ('TMS', '4.7k')):   # TDO is an output
            self.r(value, net, '+3V3')
        self.r('4.7k', 'TCK', 'GND')
        self.add('J', 'Connector_Generic', 'Conn_01x06', 'JTAG', 'Connector_PinHeader_2.54mm:PinHeader_1x06_P2.54mm_Vertical',
                 {'1': '+3V3', '2': 'TCK', '3': 'TDI', '4': 'TDO', '5': 'TMS', '6': 'GND'}, dnp=True)   # rescue only

    def header(self, signals):
        self.block = 'header'
        conn = {str(p): n for n, p in signals.items()}
        conn.update({'11': '+5V', '29': '+3V3', '12': 'GND', '30': 'GND'})
        self.add('J', 'Connector_Generic', 'Conn_02x20_Odd_Even', 'DE25-Nano GPIO',
                 'Connector_PinSocket_2.54mm:PinSocket_2x20_P2.54mm_Vertical', conn)

    def cable(self, programn_net):
        self.block = 'cable'
        self.add('J', 'Connector_Generic', 'Conn_01x08', 'OLT-ONU link', 'Connector_PinHeader_2.54mm:PinHeader_1x08_P2.54mm_Vertical',
                 {'1': 'GND', '2': 'ONU_SCL', '3': 'ONU_SDA', '4': 'CCLK', '5': 'MOSI', '6': 'MISO', '7': programn_net, '8': 'GND'})

    def holes(self):
        self.block = 'mech'
        for _ in range(2):
            self.add('H', 'Mechanical', 'MountingHole', 'M3', 'MountingHole:MountingHole_3.2mm_M3', {})

def lanes(idx, names):
    return {n: hdr(i) for n, i in zip(names, idx)}

def optics(b, tx='TX0', rx='RX0', ben='BEN'):
    """SY88236L burst laser driver + limiting amp, BOSA, TPS61391 APD bias, AD5593R trim/monitor (I2C 0x10).
    tx/rx/ben: SerDes lane nets (<name>_P/_N). Set-point resistors are starting values (SY88236L curves):
    Imod ~23 mA, Ibias max ~40 mA, trimmed by the DACs."""
    b.block = 'optics'
    for a, z in ((f'{tx}_P', 'ONU_TD_P'), (f'{tx}_N', 'ONU_TD_N'), ('ONU_RD_P', f'{rx}_P'), ('ONU_RD_N', f'{rx}_N')):
        b.c('100n', a, z)
    if ben:   # DC: bursts last up to 1 ms
        b.r('0', f'{ben}_P', 'ONU_BEN_P'); b.r('0', f'{ben}_N', 'ONU_BEN_N')
    b.fb('+3V3', '+3V3_OPT'); b.decouple('+3V3_OPT', 4, 2)
    b.add('U', LIB, 'SY88236L', 'SY88236LMG', 'Package_DFN_QFN:QFN-32-1EP_5x5mm_P0.5mm_EP3.1x3.1mm_ThermalVias',
          {'1': 'GND', '2': 'LOSLVL', '3': 'RX_SD', '4': 'RX_SD', '6': 'ONU_TD_N', '7': 'ONU_TD_P', '9': 'LD_BEN', '10': 'LD_BEN_N',
           '11': 'LASER_DIS', '12': 'APCSET', '13': 'MODSET', '14': 'BIASMAX', '15': 'MODMON', '23': 'BIASMON', '25': 'APCFAULT_N',
           '18': 'LD_MODN', '19': 'LD_MODP', '21': 'LD_BIAS', '22': 'LD_MD', '26': 'RSSI', '27': 'RIN_P', '28': 'RIN_N',
           '30': 'ONU_RD_N', '31': 'ONU_RD_P', '17': '+3V3_OPT', '20': '+3V3_OPT', '32': '+3V3_OPT',
           '5': 'GND', '8': 'GND', '16': 'GND', '24': 'GND', '29': 'GND', '33': 'GND'},
          MPN='SY88236LMG')   # DIN P/N swapped for a straight route: SerDes TX polarity inverted
    b.r('4.7k', 'LASER_DIS', '+3V3')  # TXDIS high = laser off while the FPGA is unconfigured
    b.r('4.7k', 'RX_SD', '+3V3')      # SD mode (LOS/SD_SEL low), open collector, JAM squelches on it
    b.r('5.1k', 'LOSLVL', '+3V3_OPT')
    b.add('U', LIB, 'SN65LVDT2DBV', 'SN65LVDT2DBVR', 'Package_TO_SOT_SMD:SOT-23-5',
          {'A': 'ONU_BEN_P', 'B': 'ONU_BEN_N', 'R': 'LD_BEN', 'VCC': '+3V3_OPT', 'GND': 'GND'}, MPN='SN65LVDT2DBVR')
    b.decouple('+3V3_OPT', 1)
    b.r('6.2k', 'LD_BEN_N', 'GND')    # BEN- threshold for LVTTL on BEN+ (datasheet Fig. 2e)
    for pin, r_gnd, dac in (('APCSET', '20k', 'APC_ADJ'), ('MODSET', '12k', 'MOD_ADJ')):
        b.r(r_gnd, pin, 'GND'); b.r('10k', pin, dac)
    b.r('5.6k', 'BIASMAX', 'GND')
    b.r('3.9k', 'MODMON', 'GND'); b.r('1.5k', 'BIASMON', 'GND'); b.r('1k', 'RSSI', 'GND')   # <= 2.5 V ADC, RSSI < 1.2 V
    b.r('10', 'LD_MODP', 'LD_K'); b.r('15', 'LD_MODN', '+3V3_OPT'); b.r('10', 'LD_BIAS', 'LD_K')
    for a, z in (('TIA_P', 'RIN_P'), ('TIA_N', 'RIN_N')):
        b.c('100n', a, z)
    b.fb('+3V3_OPT', '+3V3_RX'); b.decouple('+3V3_RX', 2, 1)
    b.add('U', LIB, 'BOSA_GPON', 'BOSA GPON (X100DG)', 'bridge:BOSA_GPON',
          {'LD_A': '+3V3_OPT', 'LD_K': 'LD_K', 'MPD_A': 'LD_MD', 'CASE': 'GND',
           'VAPD': 'HV_APD', 'VCC': '+3V3_RX', 'OUT+': 'TIA_P', 'OUT-': 'TIA_N', 'GND': 'GND'},
          MPN='desoldered from an X100DG')
    apd_bias(b)
    b.block = 'ddm'
    b.add('U', 'Analog', 'AD5593R', 'AD5593RBRUZ', 'Package_SO:TSSOP-16_4.4x5mm_P0.65mm',
          {'~{RESET}': 'DDM_RESET', 'A0': 'GND', 'V_{DD}': '+3V3', 'V_{LOGIC}': '+3V3', 'V_{REF}': 'DDM_VREF', 'GND': 'GND',
           'SDA': 'ONU_SDA', 'SCL': 'ONU_SCL', 'IO0': 'APC_ADJ', 'IO1': 'MOD_ADJ', 'IO2': 'APD_ADJ', 'IO3': 'BIASMON',
           'IO4': 'MODMON', 'IO5': 'APD_MON', 'IO6': 'RSSI', 'IO7': 'APCFAULT_N'}, MPN='AD5593RBRUZ')
    b.r('10k', 'DDM_RESET', '+3V3'); b.c('100n', 'DDM_VREF'); b.decouple('+3V3', 1)

def apd_bias(b, p='', r_top='300k', en=None):
    """TPS61391 APD bias; p prefixes every net (a second receiver). DAC on <p>APD_ADJ, APD current on <p>APD_MON.
    en: its enable net (else always on)."""
    b.block = 'apd' + p   # Vout = 1.2 V * (1 + 300k/10k) + (1.2 V - APD_ADJ) * 300k/20k: 55 V .. 18 V, 37 V with the DAC off
    b.fb('+3V3', f'+3V3_{p}APD'); b.decouple(f'+3V3_{p}APD', 1, 1)
    b.add('U', LIB, 'TPS61391', 'TPS61391RTER', 'Package_DFN_QFN:WQFN-16-1EP_3x3mm_P0.5mm_EP1.68x1.68mm_ThermalVias',
          {'VIN': f'+3V3_{p}APD', 'EN': en or f'+3V3_{p}APD', '14': f'+3V3_{p}APDA', '15': f'+3V3_{p}APDA', 'AGND': 'GND', 'GND': 'GND', 'EP': 'GND',
           'FB': f'{p}APD_FB', 'ISHORT': f'{p}APD_ISHORT', 'CAP': f'{p}APD_CAP', 'SW': f'{p}HV_SW', 'MONIN': f'{p}HV_MONIN', 'APD': f'{p}HV_APD',
           'MON1': f'{p}APD_MON', 'MON2': f'{p}APD_MON2'}, MPN='TPS61391RTER')
    b.add('L', 'Device', 'L', '4.7u', 'Inductor_SMD:L_Cenker_CKCS252012', {'1': f'+3V3_{p}APD', '2': f'{p}HV_SW'}, MPN='4.7 uH, Isat >= 1 A, 2520')
    b.add('D', 'Device', 'D_Schottky', 'BAT46WJ', 'Diode_SMD:D_SOD-323F', {'K': f'{p}HV_OUT', 'A': f'{p}HV_SW'}, MPN='BAT46WJ (100 V)')
    hv = dict(MPN='100 V X7R')
    b.c('100n', f'{p}HV_OUT', fp=C0603, **hv)
    b.r('100', f'{p}HV_OUT', f'{p}HV_FILT'); b.c('100n', f'{p}HV_FILT', fp=C0603, **hv)
    b.r('1k', f'{p}HV_FILT', f'{p}HV_MONIN')
    b.c('220p', f'{p}HV_APD', fp=C0603, **hv)
    b.r(r_top, f'{p}HV_OUT', f'{p}APD_FB', fp='Resistor_SMD:R_0603_1608Metric')   # 0603: 55 V across it
    b.r('10k', f'{p}APD_FB', 'GND'); b.r('20k', f'{p}APD_FB', f'{p}APD_ADJ')
    b.r('33k', f'{p}APD_ISHORT', 'GND')   # 3 mA short limit
    b.c('47n', f'{p}APD_CAP')
    b.r('10', f'+3V3_{p}APD', f'+3V3_{p}APDA'); b.c('2.2u', f'+3V3_{p}APDA')
    b.r('1.5k', f'{p}APD_MON', 'GND'); b.c('10p', f'{p}APD_MON')     # 4:5 mirror: 2 mA APD -> 2.4 V
    b.r('3.01k', f'{p}APD_MON2', 'GND'); b.c('10p', f'{p}APD_MON2')

def onu():
    b = Bridge('onu')
    c6 = [10, 11, 13, 15, 20, 22, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34]
    ds_idx = [i for i in c6 if i != 22]
    bus = {'DS_CLK': hdr(0), 'US_CLK': hdr(22), 'TX_CLK': hdr(2)}
    bus.update(lanes(ds_idx, [f'DS{i}' for i in range(16)]))
    bus.update(lanes([1, 3, 4, 5, 6, 7, 8, 9], [f'US{i}' for i in range(8)]))
    bus.update(lanes([12, 14, 16, 17, 18, 19, 21, 23], [f'BEN{i}' for i in range(8)]))
    b.header(dict(bus, SN=hdr(35)))
    b.power()
    serdes = {'HDTXP0_D0CH0': 'TX0_P', 'HDTXN0_D0CH0': 'TX0_N', 'HDRXP0_D0CH0': 'RX0_P', 'HDRXN0_D0CH0': 'RX0_N',
              'HDTXP0_D0CH1': 'BEN_P', 'HDTXN0_D0CH1': 'BEN_N', 'HDRXP0_D0CH1': 'GND', 'HDRXN0_D0CH1': 'GND',
              'REFCLKP_D0': 'REFCLK_P', 'REFCLKN_D0': 'REFCLK_N',
              'VCCHRX0_D0CH0': '+1V1_HRX', 'VCCHRX1_D0CH1': '+1V1_HRX'}
    outputs = {'DS_CLK', 'TX_CLK'} | {f'DS{i}' for i in range(16)}
    b.ecp5(bus, outputs, {'VCXO_PWM': 'U16', 'LASER_DIS': 'U17', 'RX_SD': 'T17'}, serdes, 'ONU_PROGRAMN',
           ('SN', 'CCLK', 'MOSI', 'MISO'))
    b.block = 'clock'
    b.add('Y', LIB, 'VCXO_LVDS_3225', 'SiT3821 155.52 MHz', 'Oscillator:Oscillator_SMD_SiTime_SiT9121-6Pin_3.2x2.5mm',
          {'VIN': 'VCXO_VC', 'GND': 'GND', 'VDD': '+3V3_XO', 'OUT+': 'REFCLK_P', 'OUT-': 'REFCLK_N'},
          MPN='SiT3821AI-2B2-33NB155.520000 (LVDS, +-50 ppm pull)')
    b.fb('+3V3', '+3V3_XO'); b.decouple('+3V3_XO', 1, 1)
    b.r('10k', 'VCXO_PWM', 'VCXO_RC'); b.c('1u', 'VCXO_RC')
    b.r('10k', 'VCXO_RC', 'VCXO_VC'); b.c('1u', 'VCXO_VC')
    optics(b)
    b.cable('ONU_PROGRAMN')
    b.holes()
    return b

def olt():
    b = Bridge('olt')
    ds_idx = [2, 10, 11, 12, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]
    bus = {'TX_CLK': hdr(0), 'DS_CLK': hdr(19), 'AUX': hdr(13)}
    bus.update(lanes(ds_idx, [f'DS{i}' for i in range(16)]))
    bus.update(lanes([1, 3, 4, 5, 6, 7, 8, 9], [f'US{i}' for i in range(8)]))
    # slow pins: any bank; SPI + SN at the west end (ECP5 bank 8 and the cable are west)
    slow = lanes([14, 15, 16, 17, 18, 32, 34, 35, 33],
                 ['RX_RESET', 'RSSI_TRIG', 'SCL', 'SDA', 'IRQ', 'CCLK', 'MOSI', 'MISO', 'SN'])
    b.header(dict(bus, **slow))
    b.power()
    serdes = {'HDTXP0_D0CH0': 'TX0_P', 'HDTXN0_D0CH0': 'TX0_N', 'HDRXP0_D0CH0': 'RX0_P', 'HDRXN0_D0CH0': 'RX0_N',
              'HDRXP0_D0CH1': 'GND', 'HDRXN0_D0CH1': 'GND', 'REFCLKP_D0': 'REFCLK_P', 'REFCLKN_D0': 'REFCLK_N',
              'VCCHRX0_D0CH0': '+1V1_HRX'}  # unused ch1: RX to GND, TX and VCCHRX1 open (TN-02038 3.8)
    outputs = {'TX_CLK', 'AUX'} | {f'US{i}' for i in range(8)}
    b.ecp5(bus, outputs, {}, serdes, 'OLT_PROGRAMN', ('SN', 'CCLK', 'MOSI', 'MISO'))
    b.r('10k', 'MISO', '+3V3')
    b.block = 'clock'
    for freq, oe in (('155.52', 'REF155_OE'), ('125', 'REF125_OE')):  # one enabled at a time, outputs hi-Z when off
        b.add('Y', LIB, 'XO_LVDS_3225', f'SiT9121 {freq} MHz', 'Oscillator:Oscillator_SMD_SiTime_SiT9121-6Pin_3.2x2.5mm',
              {'OE': oe, 'GND': 'GND', 'VDD': '+3V3_XO', 'OUT+': 'REFCLK_P', 'OUT-': 'REFCLK_N'},
              MPN=f'SiT9121AI-2B3-33E{freq}000 (LVDS; check code)')
        b.decouple('+3V3_XO', 1)
    b.fb('+3V3', '+3V3_XO'); b.decouple('+3V3_XO', 0, 1)
    b.r('10k', 'REF155_OE', '+3V3'); b.r('10k', 'REF125_OE', 'GND')
    b.block = 'i2c'
    b.add('U', 'Interface_Expansion', 'PCA9555PW', 'PCA9555PW', 'Package_SO:TSSOP-24_4.4x7.8mm_P0.65mm',
          {'A0': 'GND', 'A1': 'GND', 'A2': 'GND', 'VDD': '+3V3', 'VSS': 'GND', 'SCL': 'SCL', 'SDA': 'SDA', '~{INT}': 'IRQ',
           'IO0_0': 'SFP_MOD_ABS', 'IO0_1': 'SFP_TX_FAULT', 'IO0_2': 'SFP_LOS', 'IO0_3': 'SFP_TX_DIS',
           'IO0_4': 'P7_OE_N', 'IO0_5': 'P9_OE_N', 'IO0_6': 'REF155_OE', 'IO0_7': 'REF125_OE',
           'IO1_0': 'OLT_PROGRAMN', 'IO1_1': 'ONU_PROGRAMN'}, MPN='PCA9555PW')
    b.decouple('+3V3', 1)
    b.add('U', LIB, 'PCA9540B', 'PCA9540BD', 'Package_SO:SOIC-8_3.9x4.9mm_P1.27mm',
          {'SCL': 'SCL', 'SDA': 'SDA', 'VDD': '+3V3', 'VSS': 'GND', 'SC0': 'SFP_SCL', 'SD0': 'SFP_SDA',
           'SC1': 'ONU_SCL', 'SD1': 'ONU_SDA'}, MPN='PCA9540BD')
    b.decouple('+3V3', 1)
    for net, value in (('SCL', '2.2k'), ('SDA', '2.2k'), ('IRQ', '10k'), ('ONU_SCL', '4.7k'), ('ONU_SDA', '4.7k'),
                       ('P7_OE_N', '10k'), ('P9_OE_N', '10k')):
        b.r(value, net, '+3V3')
    b.block = 'sfp'
    for a, oe, out, rs in (('RX_RESET', 'P7_OE_N', 'P7_BUF', '33'), ('RSSI_TRIG', 'P9_OE_N', 'P9_BUF', '330')):
        b.add('U', '74xGxx', 'SN74LVC1G125DBV', 'SN74LVC1G125DBVR', 'Package_TO_SOT_SMD:SOT-23-5',
              {'1': oe, '2': a, 'GND': 'GND', '4': out, 'VCC': '+3V3'}, MPN='SN74LVC1G125DBVR')
        b.decouple('+3V3', 1)
        b.r(rs, out, 'SFP_P' + out[1])
    b.add('J', LIB, 'SFP_HOST', 'SFP cage', 'Connector:Connector_SFP_and_Cage',
          {'VeeT': 'GND', 'VeeR': 'GND', 'CAGE': 'GND', 'TX_FAULT': 'SFP_TX_FAULT', 'TX_DISABLE': 'SFP_TX_DIS',
           'SDA': 'SFP_SDA', 'SCL': 'SFP_SCL', 'MOD_ABS': 'SFP_MOD_ABS', 'RS0': 'SFP_P7', 'RX_LOS': 'SFP_LOS', 'RS1': 'SFP_P9',
           'RD-': 'RX0_N', 'RD+': 'RX0_P', 'VccR': '+3V3_SFPR', 'VccT': '+3V3_SFPT',
           'TD+': 'TX0_N', 'TD-': 'TX0_P'},  # P/N swapped for a straight route: ECP5 TXINVPOL = Invert
          MPN='TE 1367073-1 + cage 2227302-1')
    for rail in ('+3V3_SFPT', '+3V3_SFPR'):  # SFP MSA host filter
        b.add('L', 'Device', 'L', '1u', L0805, {'1': '+3V3', '2': rail}, MPN='1 uH >= 1 A, low DCR')
        b.decouple(rail, 1, 1)
    b.decouple('+3V3', 0, 0, 1)
    for net in ('SFP_TX_FAULT', 'SFP_TX_DIS', 'SFP_LOS', 'SFP_MOD_ABS', 'SFP_SCL', 'SFP_SDA'):
        b.r('4.7k', net, '+3V3')
    b.cable('ONU_PROGRAMN')
    b.holes()
    return b

def sfp_cage(b, name, td, rd, vcc='+3V3'):
    """SFP/SFP+ cage, own I2C bus; RS0/RS1 (OLT modules: Rx_Reset / RSSI trigger, 330 ohm for old pin-9-grounded modules)
    through series R so the FPGA keeps them hi-Z until the module is identified."""
    b.block = f'sfp:{name}'
    t, r = f'+3V3_{name}T', f'+3V3_{name}R'
    b.add('J', LIB, 'SFP_HOST', f'{name} cage', 'Connector:Connector_SFP_and_Cage',
          {'VeeT': 'GND', 'VeeR': 'GND', 'CAGE': 'GND', 'TX_FAULT': f'{name}_TX_FAULT', 'TX_DISABLE': f'{name}_TX_DIS',
           'SDA': f'{name}_SDA', 'SCL': f'{name}_SCL', 'MOD_ABS': f'{name}_MOD_ABS', 'RS0': f'{name}_P7', 'RX_LOS': f'{name}_LOS',
           'RS1': f'{name}_P9', 'RD+': f'{rd}_P', 'RD-': f'{rd}_N', 'VccR': r, 'VccT': t, 'TD+': f'{td}_P', 'TD-': f'{td}_N'},
          MPN='SFP+ cage (SFF-8432) + 20-pin host connector (SFF-8083)')
    b.r('33', f'{name}_RS0', f'{name}_P7'); b.r('330', f'{name}_RS1', f'{name}_P9')
    for rail in (t, r):  # SFP MSA host filter
        b.add('L', 'Device', 'L', '1u', L0805, {'1': vcc, '2': rail}, MPN='1 uH >= 1 A, low DCR')
        b.decouple(rail, 1, 1)
    b.decouple(vcc, 0, 0, 1)
    for n in ('TX_FAULT', 'TX_DIS', 'LOS', 'MOD_ABS', 'SCL', 'SDA'):
        b.r('4.7k', f'{name}_{n}', vcc)

def receiver(b, p, bosa, coupling, swap=False, la='SY88349NDLMG', detect='SD', dc=False):
    """BOSA receive can -> SY88349NDL -> SerDes RX <p>RX_P/N; the laser is never connected (bosa None: the caller's
    part, on <p>TIA_P/N). la: SY88349NDLMG (2.5 Gb/s, CML out) or SY88149NDLMG (1.25 Gb/s, LVPECL out).
    Auto reset, squelched between bursts (JAM); detect: 'SD' = <p>SD high during a burst (the envelope, to the FPGA),
    'LOS' = <p>LOS low during a burst (the pin's stronger edge leads).
    swap: TIA+ to the LA's DIN- (and - to +) where the layout needs it; the data comes out inverted.
    dc: LVPECL into a GTP's 50 ohm / 0.8 V termination through 180 / 124 ohm (0.74 / 0.87 V, 264 mVdpp): between bursts
    the squelched output stays a solid zero, no AC-coupling droop."""
    b.block = f'rx:{p}'
    vcc = f'+3V3_{p}OPT'
    b.fb('+3V3', vcc); b.decouple(vcc, 3, 1)
    if bosa:
        b.add('U', LIB, *bosa, {'VCC': vcc, 'VAPD': f'{p}HV_APD', 'OUT+': f'{p}TIA_P', 'OUT-': f'{p}TIA_N', 'GND': 'GND'})
    for t, i in (('P', 'DIN_N'), ('N', 'DIN_P')) if swap else (('P', 'DIN_P'), ('N', 'DIN_N')):   # AC coupled, each input 50 ohm to VREF (pin 1/4)
        b.c(coupling, f'{p}TIA_{t}', f'{p}{i}')
        b.r('49.9', f'{p}{i}', f'{p}VREF')
    b.c('100n', f'{p}VREF')
    out = f'{p}{detect}'
    b.add('U', LIB, 'SY88349NDL', la, 'Package_DFN_QFN:QFN-16-1EP_3x3mm_P0.5mm_EP1.7x1.7mm_ThermalVias',
          {'1': f'{p}DIN_P', '4': f'{p}DIN_N', '2': f'{p}VREF', '6': 'GND', '10': 'GND', '13': vcc if detect == 'SD' else 'GND',
           '14': f'{p}SDLVL', '15': out, '7': out, '12': f'{p}LA_P', '9': f'{p}LA_N', '5': vcc, '16': vcc,
           '3': 'GND', '8': 'GND', '11': 'GND', '17': 'GND'}, MPN=la)
    b.r('5.1k', f'{p}SDLVL', vcc)     # SD level: starting value, trimmed on the bench
    b.r('4.7k', out, '+3V3')          # the LVTTL output needs its pull-up
    for t in 'PN':
        if dc:
            b.r('180', f'{p}LA_{t}', f'{p}RX_{t}'); b.r('124', f'{p}RX_{t}', 'GND')
        else:
            b.c('100n', f'{p}LA_{t}', f'{p}RX_{t}')
        if la.startswith('SY88149'):
            b.r('150', f'{p}LA_{t}', 'GND')   # LVPECL emitter load

def sniffer():
    """GPON sniffer (../../../sniffer/README.md): two BOSA receive cans behind a passive tap, DS 1490 nm into
    ECP5 RX0 (CDR, 2.48832 Gb/s), US 1310 nm bursts into RX1 (2x oversampled at 2.48832 Gb/s off the VCXO, which
    the ECP5 locks to the downstream). To the DE25-Nano GPIO_0: 16 DS + 8 US bits DDR at 155.52 Mb/s per pin on a
    77.76 MHz forwarded clock (HVIO 3.3 V: 100 MHz SDR / 200 Mb/s DDR max). Config over slave SPI, trims over I2C."""
    b = Bridge('sniffer')
    c6 = [10, 11, 13, 15, 20, 22, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34]
    bus = {'FWD_CLK': hdr(0)}
    bus.update(lanes([i for i in c6 if i != 22], [f'DS{i}' for i in range(16)]))
    bus.update(lanes([1, 3, 4, 5, 6, 7, 8, 9], [f'US{i}' for i in range(8)]))
    slow = lanes([12, 14, 16, 35, 17, 18, 19], ['CCLK', 'MOSI', 'MISO', 'SN', 'PROGRAMN', 'SCL', 'SDA'])
    b.header(dict(bus, **slow))
    b.power()
    serdes = {'HDRXP0_D0CH0': 'DS_RX_P', 'HDRXN0_D0CH0': 'DS_RX_N', 'HDRXP0_D0CH1': 'US_RX_P', 'HDRXN0_D0CH1': 'US_RX_N',
              'REFCLKP_D0': 'REFCLK_P', 'REFCLKN_D0': 'REFCLK_N',
              'VCCHRX0_D0CH0': '+1V1_HRX', 'VCCHRX1_D0CH1': '+1V1_HRX'}   # both TX open (TN-02038 3.8)
    b.ecp5(bus, set(bus), {'VCXO_PWM': 'U16', 'DS_SD': 'U17', 'US_SD': 'T17'}, serdes, 'PROGRAMN',
           ('SN', 'CCLK', 'MOSI', 'MISO'))
    b.block = 'clock'
    b.add('Y', LIB, 'VCXO_LVDS_3225', 'SiT3821 155.52 MHz', 'Oscillator:Oscillator_SMD_SiTime_SiT9121-6Pin_3.2x2.5mm',
          {'VIN': 'VCXO_VC', 'GND': 'GND', 'VDD': '+3V3_XO', 'OUT+': 'REFCLK_P', 'OUT-': 'REFCLK_N'},
          MPN='SiT3821AI-2B2-33NB155.520000 (LVDS, +-50 ppm pull)')
    b.fb('+3V3', '+3V3_XO'); b.decouple('+3V3_XO', 1, 1)
    b.r('10k', 'VCXO_PWM', 'VCXO_RC'); b.c('1u', 'VCXO_RC')
    b.r('10k', 'VCXO_RC', 'VCXO_VC'); b.c('1u', 'VCXO_VC')
    # DS: continuous, 100 nF. US: bursts, 220 pF: tau = 50 ohm * 220 pF = 11 ns settles within the GPON preamble
    # and stays far above the scrambler's 7-bit runs (5.6 ns at 1.24416 Gb/s).
    receiver(b, 'DS_', ('BOSA_GPON', 'BOSA GPON (X100DG), RX can only', 'bridge:BOSA_GPON'), '100n')
    receiver(b, 'US_', ('BOSA_GPON', 'XL-BCB2111-LS-02, RX can only', 'bridge:BOSA_GPON'), '220p')
    apd_bias(b, 'DS_')
    apd_bias(b, 'US_', '330k')   # XL-BCB2111 Vbr up to 60 V: 330k gives 19..61 V over the DAC range
    b.block = 'ddm'
    b.add('U', 'Analog', 'AD5593R', 'AD5593RBRUZ', 'Package_SO:TSSOP-16_4.4x5mm_P0.65mm',
          {'~{RESET}': 'DDM_RESET', 'A0': 'GND', 'V_{DD}': '+3V3', 'V_{LOGIC}': '+3V3', 'V_{REF}': 'DDM_VREF', 'GND': 'GND',
           'SDA': 'SDA', 'SCL': 'SCL', 'IO0': 'DS_APD_ADJ', 'IO1': 'US_APD_ADJ', 'IO2': 'DS_APD_MON', 'IO3': 'US_APD_MON'},
          MPN='AD5593RBRUZ')
    b.r('10k', 'DDM_RESET', '+3V3'); b.c('100n', 'DDM_VREF'); b.decouple('+3V3', 1)
    for net in ('SCL', 'SDA'):
        b.r('2.2k', net, '+3V3')
    b.r('10k', 'MISO', '+3V3')
    b.holes()
    return b

def cutoff(b, ben, dis_fpga, dis):
    """Laser off (<dis> high) once <ben> stays high ~1.5 ms past its last rising edge, without the FPGA. Retriggered by
    every burst, so back-to-back bursts (GPON guard ~26 ns) never trip it; longest legal burst: EPON grant 1.05 ms.
    BURST_CUT is high while it holds the laser off; <dis_fpga> is the FPGA's own disable (pulled up: dark unconfigured)."""
    b.block = 'cutoff'
    b.add('U', '74xGxx', '74LVC1G123', 'SN74LVC1G123DCUR', 'Package_SO:VSSOP-8_2.3x2mm_P0.5mm',
          {'1': 'GND', '2': ben, '3': '+3V3', '5': 'CUT_Q', '6': 'CUT_C', '7': 'CUT_RC', 'VCC': '+3V3', 'GND': 'GND'},
          MPN='SN74LVC1G123DCUR')
    b.c('100n', 'CUT_C', 'CUT_RC', fp=C0603, MPN='100 nF 50 V X7R 0603')   # 0603 50 V: little DC-bias loss
    b.r('15k', 'CUT_RC', '+3V3')
    b.decouple('+3V3', 1)
    b.r('1k', ben, 'BEN_DLY'); b.c('47p', 'BEN_DLY')   # ~45 ns: Q rises (<= 13 ns) before BEN reaches the gate
    b.add('U', '74xGxx', '74LVC1G97', 'SN74LVC1G97DBVR', 'Package_TO_SOT_SMD:SOT-23-6',   # Schmitt inputs: Y = IN2 ? IN0 : IN1
          {'1': 'BEN_DLY', '3': 'GND', '6': 'CUT_Q', '4': 'BURST_CUT', 'VCC': '+3V3', 'GND': 'GND'}, MPN='SN74LVC1G97DBVR')
    b.decouple('+3V3', 1)
    b.add('U', '74xGxx', '74LVC1G32', 'SN74LVC1G32DBVR', 'Package_TO_SOT_SMD:SOT-23-5',
          {'1': dis_fpga, '2': 'BURST_CUT', '4': dis, 'VCC': '+3V3', 'GND': 'GND'}, MPN='SN74LVC1G32DBVR')
    b.decouple('+3V3', 1)
    b.r('4.7k', dis_fpga, '+3V3')   # FPGA unconfigured: laser off

def laser(b, p, data, dis, ben=None):
    """MAX3643 on the laser of the caller's BOSA (<p>LD_A, <p>LD_K, <p>MPD_A; monitor cathode on <p>LD_A). No APC:
    modulation and bias by DAC on <p>LD_MSET / <p>LD_BSET (0 V or DAC off = no current), read back on <p>LD_BCMON
    (14 mV per mA of bias) and <p>LD_MD (monitor diode). data: SerDes lane <data>_P/_N; <dis> high = dark.
    ben: LVCMOS burst enable (the monitor diode is sampled during the burst), None = lit whenever enabled.
    Values: MAX3643 typical application circuit; set-point resistors are starting values (61 mA modulation,
    40 mA burst / 61 mA continuous bias at the DAC's 2.5 V)."""
    b.block = f'laser:{p}'
    n = lambda name: f'{p}LD_{name}'
    vcc = f'+3V3_{p}LD'
    b.fb('+3V3', vcc); b.decouple(vcc, 4, 2)
    for t in 'PN':   # AC coupled: the FPGA keeps the lane toggling while dark
        b.c('100n', f'{data}_{t}', n(f'IN_{t}'))
    b.r('100', n('IN_P'), n('IN_N'))
    b.add('U', LIB, 'MAX3643', 'MAX3643ETG+', 'Package_DFN_QFN:TQFN-24-1EP_4x4mm_P0.5mm_EP2.6x2.6mm_ThermalVias',
          {'IN+': n('IN_P'), 'IN-': n('IN_N'), 'BEN+': n('BEN_P'), 'BEN-': n('BEN_N'), '~{EN}': dis, 'VMSET': n('VMSET'),
           'MODSET': n('MODSET'), 'VBSET': n('VBSET'), 'BIASSET': n('BIASSET'), 'IMAX': n('IMAX'),
           'MDIN': f'{p}MPD_A' if ben else 'GND', 'OUT+': n('OUT_P'), 'OUT-': n('OUT_N'), 'BIAS+': n('BIAS_P'), 'BIAS-': n('BIAS_N'),
           'BCMON': n('BCMON'), 'VCCA': vcc, 'VCCS': vcc, 'VCCO': vcc, 'GND': 'GND', 'EP': 'GND', **({'MDOUT': n('MDOUT')} if ben else {})},
          MPN='MAX3643ETG+')
    b.r('5.1k', n('BEN_P'), vcc)   # single-ended burst enable (datasheet Fig. 5); alone: always on
    if ben:
        b.r('3k', ben, n('BEN_P'))
    b.r('5.1k', n('BEN_N'), vcc); b.r('9.1k', n('BEN_N'), 'GND')
    for kind, r_set in (('M', '1.8k'), ('B', '2.7k' if ben else '1.8k')):
        b.r('10k', n(f'{kind}SET'), n(f'V{kind}SET')); b.r('10k', n(f'V{kind}SET'), 'GND'); b.c('10n', n(f'V{kind}SET'))
        b.r(r_set, n('MODSET' if kind == 'M' else 'BIASSET'), 'GND')
    b.r('6.8k', n('IMAX'), 'GND')    # bias + modulation limit ~90 mA
    b.r('1k', n('BCMON'), 'GND'); b.c('10n', n('BCMON'))
    b.r('15', vcc, n('A')); b.add('L', 'Device', 'L', '10n', 'Inductor_SMD:L_0402_1005Metric', {'1': vcc, '2': n('A')}, MPN='10 nH 0402')
    b.r('5.6', n('OUT_P'), n('K')); b.r('5.6', n('BIAS_P'), n('K'))
    b.r('75', n('K'), n('COMP')); b.c('27p', n('COMP'))
    for r, pin, anode in (('15', 'OUT_N', n('A')), ('10', 'BIAS_N', vcc)):   # the laser's dummy loads
        b.r(r, n(pin), n(f'{pin}_D'))
        b.add('D', 'Device', 'D', '1N4148WS', 'Diode_SMD:D_SOD-323', {'A': anode, 'K': n(f'{pin}_D')}, MPN='1N4148WS')
    if ben:   # held between bursts; MDOUT shows 1.2 V during a burst
        b.r('1k', f'{p}MPD_A', 'GND'); b.c('10p', f'{p}MPD_A')
        b.r('10k', n('MDOUT'), n('MD')); b.c('1n', n('MD'))
    else:
        b.r('3.3k', f'{p}MPD_A', 'GND')
        b.r('10k', f'{p}MPD_A', n('MD')); b.c('100n', n('MD'))

def inline():
    """In-line GPON sniffer (../FPGA-USB-GPON-sniffer/README.md): in one ONU's drop fibre, both directions regenerated
    through the Artix-7 and dumped over USB. OLT port: ONU-type BOSA (DS_ receiver, US_ burst laser); ONU port: OLT-type
    BOSA (US_ burst receiver, DS_ laser), both on the X100DG BOSA land (the OLT type assumed alike). USB: USB3320 ULPI PHY to the FPGA (LUNA serial device); the bitstream's
    flash sits in a DIP-8 socket (written in a programmer), the JTAG header is not fitted.
    The upstream laser lights only while the ONU-port receiver sees light (US_QUIET low, no FPGA in that path) and
    the max-burst cutoff ends what stays on."""
    b = Bridge('inline', 'In-line GPON sniffer: USB, Artix-7, OLT and ONU optical ports')
    b.serdes_nets = ('DS_TIA_*', 'DS_DIN_*', 'DS_LA_*', 'DS_RX_*', 'DS_TX_*', 'US_TX_*', 'REFCLK_*', 'XO_OUT_*', 'USB_D?', '*_LD_IN_*',
                     '*_LD_OUT_*', '*_LD_BIAS_*', '*_LD_K', '*_LD_A')   # the lasers' currents too
    b.power_nets = ('VBUS',)   # the whole board's current
    b.block = 'usb'
    b.add('J', 'Connector', 'USB_C_Receptacle_USB2.0_16P', 'USB-C', 'Connector_USB:USB_C_Receptacle_HRO_TYPE-C-31-M-12',
          {'VBUS': 'VBUS', 'GND': 'GND', 'SHIELD': 'GND', 'CC1': 'USB_CC1', 'CC2': 'USB_CC2', 'D+': 'USB_DP', 'D-': 'USB_DM'},
          MPN='TYPE-C-31-M-12')
    b.fb('VBUS', '+5V')
    b.r('5.1k', 'USB_CC1', 'GND'); b.r('5.1k', 'USB_CC2', 'GND')
    b.add('U', 'Power_Protection', 'USBLC6-2SC6', 'USBLC6-2SC6', 'Package_TO_SOT_SMD:SOT-23-6',
          {'I/O1': 'USB_DP', 'I/O2': 'USB_DM', 'VBUS': 'VBUS', 'GND': 'GND'}, MPN='USBLC6-2SC6')
    # USB3320 ULPI PHY in output clock mode: its reference (24 MHz) from the FPGA, which holds it in reset until then
    phy = {'CLKOUT': 'ULPI_CLK', 'DIR': 'ULPI_DIR', 'NXT': 'ULPI_NXT', 'STP': 'ULPI_STP', 'RESETB': 'ULPI_RST_N',
           'REFCLK': 'ULPI_REFCLK', 'REFSEL0': '+3V3', 'REFSEL1': '+3V3', 'REFSEL2': '+3V3', 'RBIAS': 'USB_RBIAS',
           'VBAT': '+5V', 'VDD33': 'USB_VDD33', 'VDD18': '+1V8_USB', 'VDDIO': '+3V3', 'VBUS': 'USB_VBUS_SENSE',
           'DP': 'USB_DP', 'DM': 'USB_DM', 'ID': 'USB_VDD33', 'GND': 'GND'}
    phy.update({f'DATA{i}': f'ULPI_D{i}' for i in range(8)})
    b.add('U', LIB, 'USB3320', 'USB3320C-EZK-TR', 'Package_DFN_QFN:QFN-32-1EP_5x5mm_P0.5mm_EP3.45x3.45mm_ThermalVias', phy,
          MPN='USB3320C-EZK-TR')
    b.c('2.2u', 'USB_VDD33'); b.decouple('+3V3', 1)   # VDD33: its regulator's output, 2.2 uF < 1 ohm ESR
    b.fb('+1V8', '+1V8_USB'); b.decouple('+1V8_USB', 2); b.c('1u', '+1V8_USB')
    b.r('8.06k', 'USB_RBIAS', 'GND', MPN='1 %')
    b.r('10k', 'USB_VBUS_SENSE', 'VBUS')   # RVBUS, device only (Table 5-7)
    b.r('10k', 'ULPI_RST_N', 'GND')        # PHY off while the FPGA is unconfigured
    # balls: banks 34 (USB PHY; its clock on an SRCC input) and 16 (optics) on the outer rows toward their parts once
    # the BGA is turned a quarter
    misc = {**{f'ULPI_D{i}': ball for i, ball in enumerate(('AB1', 'AB2', 'AB3', 'AB5', 'AB6', 'AB7', 'AB8', 'AA1'))},
            'ULPI_DIR': 'AA3', 'ULPI_NXT': 'AA4', 'ULPI_STP': 'AA5', 'ULPI_RST_N': 'AA6', 'ULPI_CLK': 'Y4', 'ULPI_REFCLK': 'AA8',
            'US_BEN': 'B22', 'US_LASER_DIS': 'C22', 'DS_LASER_DIS': 'D22', 'DS_LOS': 'E22', 'US_ABOVE': 'G22',
            'BURST_CUT': 'B21', 'SCL': 'D21', 'SCL2': 'E21', 'SDA': 'F21', 'LED_SYNC': 'G21', 'APD_EN': 'A21'}
    b.artix7(misc)
    # the bitstream's flash in a socket: written in a programmer, no JTAG needed (JLC places the socket only)
    b.add('U', 'Memory_Flash', 'W25Q32JVSS', 'W25Q32 DIP-8 in a socket', 'Package_DIP:DIP-8_W7.62mm_Socket',
          {'1': 'FLASH_CS', '2': 'MISO', '3': 'FLASH_WP', '4': 'GND', '5': 'MOSI', '6': 'CCLK', '7': 'FLASH_HOLD', '8': '+3V3'},
          MPN='DS1001-01-08BT1NSF6X-JKB')
    for net in ('MISO', 'FLASH_WP', 'FLASH_HOLD'):
        b.r('10k', net, '+3V3')
    b.decouple('+3V3', 1)
    b.add('D', 'Device', 'LED', 'green', 'LED_SMD:LED_0603_1608Metric', {'A': 'LED_SYNC', 'K': 'LED_K'}, MPN='green 0603')
    b.r('1k', 'LED_K', 'GND')
    b.block = 'clock'   # fixed 155.52 MHz: the downstream transmitter follows the line with its phase interpolator
    b.add('Y', LIB, 'XO_LVDS_7050', 'SX7DF155.520B20F30DNN', 'Oscillator:Oscillator_SMD_IDT_JU6-6_7.0x5.0mm_P2.54mm',
          {'OE': '+3V3_XO', 'GND': 'GND', 'VDD': '+3V3_XO', 'OUT+': 'XO_OUT_P', 'OUT-': 'XO_OUT_N'}, MPN='SX7DF155.520B20F30DNN')
    b.fb('+3V3', '+3V3_XO'); b.decouple('+3V3_XO', 1, 1)
    for t in 'PN':   # UG482: an LVDS oscillator AC coupled into MGTREFCLK
        b.c('100n', f'XO_OUT_{t}', f'REFCLK_{t}')

    # OLT port: downstream in (continuous: SY88993AV, squelched by its own LOS), upstream bursts out
    b.block = 'rx:DS_'
    b.fb('+3V3', '+3V3_DS_OPT'); b.decouple('+3V3_DS_OPT', 3, 1)
    b.add('U', LIB, 'BOSA_GPON', 'GPON ONU BOSA', 'bridge:BOSA_GPON',
          {'LD_A': 'US_LD_A', 'LD_K': 'US_LD_K', 'MPD_A': 'US_MPD_A', 'CASE': 'GND',
           'VAPD': 'DS_HV_APD', 'VCC': '+3V3_DS_OPT', 'OUT+': 'DS_TIA_P', 'OUT-': 'DS_TIA_N', 'GND': 'GND'},
          MPN='GPON ONU BOSA, APD-TIA receiver (1310 nm DFB, 1490 nm APD)')
    for t in 'PN':
        b.c('100n', f'DS_TIA_{t}', f'DS_DIN_{t}'); b.r('49.9', f'DS_DIN_{t}', 'DS_VREF')
        b.c('100n', f'DS_LA_{t}', f'DS_RX_{t}')
    b.c('100n', 'DS_VREF')
    b.add('U', LIB, 'SY88993AV', 'SY88993AVKG', 'Package_SO:MSOP-10_3x3mm_P0.5mm',
          {'DIN': 'DS_DIN_P', '/DIN': 'DS_DIN_N', 'VREF': 'DS_VREF', 'LOSLVL': 'DS_LOSLVL', '/EN': 'DS_LOS', 'LOS': 'DS_LOS',
           'DOUT': 'DS_LA_P', '/DOUT': 'DS_LA_N', 'VCC': '+3V3_DS_OPT', 'GND': 'GND'}, MPN='SY88993AVKG')
    b.r('7.5k', 'DS_LOSLVL', '+3V3_DS_OPT'); b.r('2.4k', 'DS_LOSLVL', 'DS_VREF')   # ~VCC - 1.0 V: LOS near 15 mVpp, to trim
    b.r('4.7k', 'DS_LOS', '+3V3')
    apd_bias(b, 'DS_', en='APD_EN')
    b.r('10k', 'APD_EN', 'GND')   # FPGA unconfigured: both APD supplies off (the DACs come up at 0 V: highest bias)
    b.block = 'gate'   # US_LD_BEN = US_BEN while the ONU sends, stretched ~0.7 us past its burst for the regenerator's delay
    b.add('U', '74xGxx', '74LVC1G07', 'SN74LVC1G07DBVR', 'Package_TO_SOT_SMD:SOT-23-5',
          {'2': 'US_LOS', '4': 'US_QUIET', 'VCC': '+3V3', 'GND': 'GND'}, MPN='SN74LVC1G07DBVR')
    b.r('22k', 'US_QUIET', '+3V3'); b.c('47p', 'US_QUIET'); b.decouple('+3V3', 1)
    b.add('U', '74xGxx', '74LVC1G97', 'SN74LVC1G97DBVR', 'Package_TO_SOT_SMD:SOT-23-6',
          {'1': 'US_BEN', '3': 'GND', '6': 'US_QUIET', '4': 'US_LD_BEN', 'VCC': '+3V3', 'GND': 'GND'}, MPN='SN74LVC1G97DBVR')
    b.r('4.7k', 'US_BEN', 'GND'); b.decouple('+3V3', 1)
    laser(b, 'US_', 'US_TX', 'US_LD_DIS', 'US_LD_BEN')
    cutoff(b, 'US_LD_BEN', 'US_LASER_DIS', 'US_LD_DIS')

    # ONU port: upstream bursts in, downstream out; the fixed pad is on the fibre
    b.block = 'bosa:onu'
    b.add('U', LIB, 'BOSA_GPON', 'GPON OLT BOSA', 'bridge:BOSA_GPON',
          {'LD_A': 'DS_LD_A', 'LD_K': 'DS_LD_K', 'MPD_A': 'DS_MPD_A', 'CASE': 'GND',
           'VAPD': 'US_HV_APD', 'VCC': '+3V3_US_OPT', 'OUT+': 'US_TIA_P', 'OUT-': 'US_TIA_N', 'GND': 'GND'},
          MPN='XL-BCB2111-LS-02')
    receiver(b, 'US_', None, '220p', la='SY88149NDLMG', detect='LOS', dc=True)
    # 19 mV across the termination holds the output at zero on the receiver's noise, in the 120 ns before LOS squelches
    b.r('5.1k', 'US_DIN_P', 'GND'); b.r('5.1k', 'US_DIN_N', 'GND', dnp=True)   # the other one if the BOSA's data is inverted
    apd_bias(b, 'US_', '330k', en='APD_EN')   # XL-BCB2111 Vbr up to 60 V
    b.block = 'level'   # burst level: the APD current against a DAC, read by the FPGA during each burst
    b.add('U', 'Comparator', 'TLV3501AIDBV', 'TLV3501AIDBVR', 'Package_TO_SOT_SMD:SOT-23-6',
          {'3': 'US_APD_MON', '1': 'US_RSSI_REF', '5': 'US_ABOVE', '4': '+3V3', '2': 'GND', '6': 'GND'}, MPN='TLV3501AIDBVR')
    b.c('10n', 'US_RSSI_REF'); b.decouple('+3V3', 1)
    laser(b, 'DS_', 'DS_TX', 'DS_LASER_DIS')
    b.r('4.7k', 'DS_LASER_DIS', '+3V3')   # FPGA unconfigured: dark
    b.block = 'ddm'   # set points: two DACs on one address, each its own clock (0 V at power-on: no laser current)
    for scl, outs in (('SCL', ('DS_APD_ADJ', 'US_LD_BSET', 'US_LD_MSET')), ('SCL2', ('US_APD_ADJ', 'US_RSSI_REF', 'DS_LD_BSET', 'DS_LD_MSET'))):
        b.add('U', 'Analog_DAC', 'MCP4728', 'MCP4728T-E/UN', 'Package_SO:MSOP-10_3x3mm_P0.5mm',
              {'VDD': '+3V3', 'VSS': 'GND', 'SCL': scl, 'SDA': 'SDA', '~{LDAC}': 'GND', **{f'VOUT{c}': n for c, n in zip('ABCD', outs)}},
              MPN='MCP4728T-E/UN')
        b.decouple('+3V3', 1, 1)
    b.add('U', 'Analog_ADC', 'ADS7828', 'ADS7828E/2K5', 'Package_SO:TSSOP-16_4.4x5mm_P0.65mm',   # monitors, I2C 0x48
          {'VDD': '+3V3', 'GND': 'GND', 'COM': 'GND', 'REF': 'ADC_REF', 'A0': 'GND', 'A1': 'GND', 'SCL': 'SCL', 'SDA': 'SDA',
           'CH0': 'DS_APD_MON', 'CH1': 'US_LD_MD', 'CH2': 'US_LD_BCMON', 'CH3': 'DS_LD_MD', 'CH4': 'DS_LD_BCMON',
           'CH5': 'GND', 'CH6': 'GND', 'CH7': 'GND'}, MPN='ADS7828E/2K5')
    b.c('2.2u', 'ADC_REF'); b.decouple('+3V3', 1)
    for net in ('SCL', 'SCL2', 'SDA'):
        b.r('2.2k', net, '+3V3')
    b.block = 'shield'   # a hand-made 0.2 mm can over each receiver
    for _ in range(6):
        b.add('SH', LIB, 'SHIELD_CLIP', 'ICSRC6508SFR', 'bridge:SHIELD_CLIP_6.5x0.8', {'GND': 'GND'}, MPN='ICSRC6508SFR')
    b.holes(); b.holes()
    return b

# HSMC GTS lanes (DE25-Standard: 4, lanes 0-3) and the even-row single-ended pins (bank 2 north, bank 3 south)
HSMC_LANES = {3: ('SFP0_TD', 'SFP0_RD'), 2: ('SFP1_TD', 'SFP1_RD'), 1: ('ONU_TX', 'ONU_RX'), 0: ('ONU_BEN', None)}
HSMC_SWAP = {'SFP0_RD', 'SFP1_RD', 'ONU_RX', 'ONU_TX'}   # P/N swapped at the connector for a planar route: GTS polarity
                                                # inversion on these (ONU_TX: undoes the DIN swap, so not inverted)
HSMC_IO = {'SFP0': [42, 44, 48, 50, 54, 56, 60, 62], 'SFP1': [66, 68, 72, 74, 78, 80, 84, 86],
           'ONU': [102, 104, 108, 110, 114, 116, 120, 122]}

def hsmc():
    """DE25-Standard HSMC card: two SFP+ cages (SFP0: a GPON or EPON OLT module or an Ethernet one, SFP1: Ethernet, a
    1000BASE-X module or a copper 1000BASE-T SFP over SGMII; 10G later), GPON ONU optics (BOSA of the X100DG); the upstream sniffer
    is a GPON OLT module in SFP0 (operator 2026-11-05). Power: HSMC 3.3 V (1.5 A) for the logic and optics, HSMC 12 V (1 A; the board's
    HSMC 12V Power Header must be set to pins 2-3, default off) through a buck for the two cages (10G / XGS-PON modules up to ~1.1 A each)."""
    b = Bridge('hsmc', 'HSMC card for DE25-Standard: OLT/Ethernet and Ethernet SFP+ cages, GPON ONU BOSA')
    b.serdes_w = 0.3   # all lanes left to the router: ~55 ohm on the JLC outer layers
    fn = hsmc_pinout()
    pin_of = {f: p for p, f in fn.items()}
    conn = {str(p): '+3V3' for p, f in fn.items() if f == '3.3V'}
    # three pins with only unused pins between them, so their chain fences no signal; plenty for 1 A
    conn.update({str(p): '+12V' for p in (88, 94, 100)})
    assert all(fn[p] == '12V' for p in (88, 94, 100))
    conn.update({str(p): 'GND' for p, f in fn.items() if f in ('GND', 'PSNTn')})   # PSNTn low: card present
    for lane, pair in HSMC_LANES.items():
        for d, net in zip(('TX', 'RX'), pair):
            if net:
                p, n = ('N', 'P') if net in HSMC_SWAP else ('P', 'N')
                conn.update({str(pin_of[f'XCVR_{d}p{lane}']): f'{net}_{p}', str(pin_of[f'XCVR_{d}n{lane}']): f'{net}_{n}'})
    conn.update({'6': 'GTS_REFCLK_P', '8': 'GTS_REFCLK_N', '2': 'GTS_RXREF_P', '4': 'GTS_RXREF_N'})   # XCVR_RX6 / RX7
    io = {cage: [f'{cage}_{n}' for n in ('TX_DIS', 'TX_FAULT', 'LOS', 'MOD_ABS', 'SCL', 'SDA', 'RS0', 'RS1')] for cage in ('SFP0', 'SFP1')}
    io['ONU'] = ['LASER_DIS_FPGA', 'RX_SD', 'ONU_SCL', 'ONU_SDA', 'VCXO_PWM', 'BURST_CUT', 'BEN_IO', 'VCXO156_PWM']
    for group, nets in io.items():
        conn.update({str(p): n for p, n in zip(HSMC_IO[group], nets)})
    b.block = 'hsmc'
    b.add('J', LIB, 'HSMC_CARD', 'ASP-122952-01', 'bridge:HSMC_ASP-122952-01', conn, MPN='Samtec ASP-122952-01')
    b.decouple('+3V3', 2, 2, 2)
    # GTS refclks, both VCXOs tuned by the FPGA: 155.52 MHz for every GPON lane, 156.25 MHz for 1.25 Gb/s (Ethernet, EPON:
    # the EPON ONU transmits loop-timed like a stock one, operator 2026-11-02)
    b.block = 'clock'
    # pull: absolute range (datasheet APR) minus ~30 % lost to the 20k series / 100k VIN divider must cover the line:
    # 1000BASE-PX OLT +-100 ppm (H: APR +-170), GPON OLT (E: APR +-70); 3225 package: pin 2 NC ('N')
    for mhz, v, ref, pull in (('156.25', 'VCXO156', 'GTS_REFCLK', 'H'), ('155.52', 'VCXO', 'GTS_RXREF', 'E')):
        b.add('Y', LIB, 'VCXO_LVDS_3225', f'SiT3821 {mhz} MHz', 'Oscillator:Oscillator_SMD_SiTime_SiT9121-6Pin_3.2x2.5mm',
              {'VIN': f'{v}_VC', 'GND': 'GND', 'VDD': '+3V3_XO', 'OUT+': f'{v}_P', 'OUT-': f'{v}_N'},
              MPN=f'SiT3821AI-1B2-33N{pull}{mhz}0000')
        for d in 'PN':   # LVPECL emitter load: ~1.6 Vpp, LVDS falls under the GTS 0.6 Vpp minimum
            b.r('150', f'{v}_{d}', 'GND'); b.c('100n', f'{v}_{d}', f'{ref}_AC_{d}')
        b.r('10k', f'{v}_PWM', f'{v}_RC'); b.c('1u', f'{v}_RC')
        b.r('10k', f'{v}_RC', f'{v}_VC'); b.c('1u', f'{v}_VC')
    b.fb('+3V3', '+3V3_XO'); b.decouple('+3V3_XO', 2, 1)
    b.block = 'power:+3V3_SFP'   # TPS563201: 0.768 V * (1 + 33.2k/10k) = 3.32 V, 3 A
    b.add('U', 'Regulator_Switching', 'TPS563201', 'TPS563201DDCR', 'Package_TO_SOT_SMD:SOT-23-6',
          {'VIN': '+12V', 'EN': '+12V', 'GND': 'GND', 'SW': 'SW_SFP', 'VFB': 'FB_SFP', 'VBST': 'BST_SFP'}, MPN='TPS563201DDCR')
    b.c('100n', 'BST_SFP', 'SW_SFP')
    b.add('L', 'Device', 'L', '2.2u', 'Inductor_SMD:L_Cenker_CKCS4020', {'1': 'SW_SFP', '2': '+3V3_SFP'},
          MPN='2.2 uH, Isat >= 4 A, 4020')
    b.r('33.2k', '+3V3_SFP', 'FB_SFP'); b.r('10k', 'FB_SFP', 'GND')
    for _ in range(2): b.c('10u', '+12V', fp=C0805, MPN='10 uF 25 V X5R 0805')
    b.c('100n', '+12V', fp=C0603, MPN='100 nF 25 V')
    b.decouple('+3V3_SFP', 0, 0, 3)
    for cage in ('SFP0', 'SFP1'):
        sfp_cage(b, cage, f'{cage}_TD', f'{cage}_RD', '+3V3_SFP')
    optics(b, 'ONU_TX', 'ONU_RX', None)
    b.r('4.7k', 'ONU_SCL', '+3V3'); b.r('4.7k', 'ONU_SDA', '+3V3')
    b.r('0', 'BEN_IO', 'LD_BEN', dnp=True)   # fallback if the GTS TX cannot drive BEN DC-coupled: fit it, remove the LVDS receiver
    cutoff(b, 'LD_BEN', 'LASER_DIS_FPGA', 'LASER_DIS')
    # Refclk series 20 ohm into the GTS 100 ohm termination: LVPECL 1.2-2.0 Vpp -> 0.86-1.43 Vpp, inside the GTS
    # 0.6-1.7 Vpp and 0.85 V abs max. Added last so the routed board keeps its reference numbers.
    b.block = 'clock'
    for ref in ('GTS_REFCLK', 'GTS_RXREF'):
        for d in 'PN':
            b.r('20', f'{ref}_AC_{d}', f'{ref}_{d}')
    b.block = 'mech'
    for _ in range(2):
        b.add('H', 'Mechanical', 'MountingHole', 'M3', 'MountingHole:MountingHole_3.2mm_M3', {})
    return b
