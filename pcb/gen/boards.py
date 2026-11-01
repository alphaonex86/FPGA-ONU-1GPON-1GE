# SPDX-License-Identifier: CERN-OHL-S-2.0
"""Netlists of the two DE25-Nano SerDes bridge boards (ONU on GPIO_0, OLT on GPIO_1)."""
import os, re
from kisch import Board
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

ECP5_BALLS = {p[1]: p[2] for p in pins(lib_symbol(LIB, 'LFE5UM-25F-BG381'))}
ROWS = 'ABCDEFGHJKLMNPRTUVWY'
def ball_xy(b):
    m = re.match(r'([A-Z])(\d+)', b)
    return (int(m.group(2)) - 10.5) * 0.8, (ROWS.index(m.group(1)) - 9.5) * 0.8

def assign_balls(signals, clock):
    """signals: {net: header pin}; header order west->east mapped onto rows A/B west->east.
    The forwarded clock lands on the nearest PCLKT ball."""
    io = [b for b, f in ECP5_BALLS.items() if re.match(r'P[TL]\d+[A-D]', f) and b[0] in 'AB']
    io.sort(key=lambda b: (ball_xy(b)[0], ball_xy(b)[1]))
    nets = sorted(signals, key=lambda n: (hdr_x(signals[n]), signals[n] % 2))
    assert len(nets) <= len(io), (len(nets), len(io))
    # spread the nets over the balls evenly
    step = len(io) / len(nets)
    chosen = [io[int(i * step)] for i in range(len(nets))]
    out = dict(zip(nets, chosen))
    pclk = [b for b in io if 'PCLKT' in ECP5_BALLS[b]]
    if out[clock] not in pclk:
        free = [b for b in pclk if b not in chosen]
        target = min(free or pclk, key=lambda b: abs(ball_xy(b)[0] - ball_xy(out[clock])[0]))
        if target in chosen:  # swap with the net sitting there
            other = next(n for n, b in out.items() if b == target)
            out[other] = out[clock]
        out[clock] = target
    return out

class Bridge(Board):
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

    def power(self):
        self.block = 'power'
        self.decouple('+5V', 0, 1, 1)
        self.decouple('+3V3', 0, 1, 2)
        for out in ('+1V1', '+1V1_SD'):  # TLV62569: 0.6 V * (1 + 100k/120k) = 1.10 V
            fbn = f'FB_{out[1:]}'
            sw = f'SW_{out[1:]}'
            self.block = f'power:{out}'
            self.add('U', 'Regulator_Switching', 'TLV62569DBV', 'TLV62569DBVR', 'Package_TO_SOT_SMD:SOT-23-5',
                     {'VIN': '+5V', 'EN': '+5V', 'GND': 'GND', 'SW': sw, 'FB': fbn}, MPN='TLV62569DBVR')
            self.add('L', 'Device', 'L', '2.2u', 'Inductor_SMD:L_Cenker_CKCS252012', {'1': sw, '2': out},
                     MPN='CKCS252012-2R2M')
            self.r('100k', out, fbn)
            self.r('120k', fbn, 'GND')
            self.decouple('+5V', 0, 1)
            self.decouple(out, 0, 0, 2)
        self.block = 'power:+2V5'
        self.add('U', 'Regulator_Linear', 'TLV75525PDBV', 'TLV75525PDBVR', 'Package_TO_SOT_SMD:SOT-23-5',
                 {'IN': '+3V3', 'EN': '+3V3', 'GND': 'GND', 'OUT': '+2V5'}, MPN='TLV75525PDBVR')
        self.c('1u', '+3V3')
        self.c('1u', '+2V5')

    def ecp5(self, bus, outputs, misc, serdes, programn, spi):
        """bus: {net: header pin} on rows A/B, outputs: nets driven by the ECP5 (series R pack),
        misc: {net: ball}, serdes: {ball function: net}, spi: (sn, cclk, mosi, miso)."""
        self.block = 'ecp5'
        clock = next(n for n in bus if n.endswith('_CLK') and n not in outputs)
        balls = assign_balls(bus, clock)
        conn = {}
        for b, f in ECP5_BALLS.items():
            if f == 'VCC': conn[b] = '+1V1'
            elif f == 'VCCAUX': conn[b] = '+2V5_AUX'
            elif f.startswith('VCCIO'): conn[b] = '+3V3'
            elif f == 'GND': conn[b] = 'GND'
            elif f == 'VCCA0': conn[b] = '+1V1_VCCA'
            elif f == 'VCCAUXA0': conn[b] = '+2V5_AUXA'
            elif f.startswith('VCCHTX'): conn[b] = '+1V1_HTX'
            elif f.startswith('VCCHRX') and f in serdes: conn[b] = serdes[f]
            elif f in serdes: conn[b] = serdes[f]
        cfg = {'CFG_0': 'CFG0', 'CFG_1': 'GND', 'CFG_2': 'GND', 'PROGRAMN': programn, 'INITN': 'INITN', 'DONE': 'DONE',
               'TCK': 'TCK', 'TDI': 'TDI', 'TDO': 'TDO', 'TMS': 'TMS', 'CCLK': spi[1]}
        dual = {'SN/CSN': spi[0], 'D0/MOSI/IO0': spi[2], 'D1/MISO/IO1': spi[3], 'HOLDN/DI/BUSY/CSSPIN/CEN': 'HOLDN'}
        for b, f in ECP5_BALLS.items():
            if f in cfg: conn[b] = cfg[f]
            if '/' in f and f.split('/', 1)[1] in dual: conn[b] = dual[f.split('/', 1)[1]]
        for net, b in balls.items():
            conn[b] = net + '_S' if net in outputs else net
        conn.update({b: n for n, b in misc.items()})
        self.add('U', LIB, 'LFE5UM-25F-BG381', 'LFE5UM-25F-6BG381C',
                 'Package_BGA:Lattice_caBGA-381_17x17mm_Layout20x20_P0.8mm', conn, MPN='LFE5UM-25F-6BG381C')
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
        for net, value in (('CFG0', '4.7k'), (programn, '4.7k'), ('INITN', '4.7k'), ('DONE', '4.7k'),
                           (spi[0], '4.7k'), ('HOLDN', '10k'), (spi[1], '1k'),
                           ('TDI', '4.7k'), ('TMS', '4.7k')):  # TDO is an output
            self.r(value, net, '+3V3')
        self.r('4.7k', 'TCK', 'GND')
        self.add('J', 'Connector_Generic', 'Conn_01x06', 'JTAG', 'Connector_PinHeader_2.54mm:PinHeader_1x06_P2.54mm_Vertical',
                 {'1': '+3V3', '2': 'TCK', '3': 'TDI', '4': 'TDO', '5': 'TMS', '6': 'GND'})
        return balls

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

def optics(b):
    """SY88236L burst laser driver + limiting amp, BOSA, TPS61391 APD bias, AD5593R trim/monitor (I2C 0x10).
    Set-point resistors are starting values (SY88236L curves): Imod ~23 mA, Ibias max ~40 mA, trimmed by the DACs."""
    b.block = 'optics'
    for a, z in (('TX0_P', 'ONU_TD_P'), ('TX0_N', 'ONU_TD_N'), ('ONU_RD_P', 'RX0_P'), ('ONU_RD_N', 'RX0_N')):
        b.c('100n', a, z)
    b.r('0', 'BEN_P', 'ONU_BEN_P'); b.r('0', 'BEN_N', 'ONU_BEN_N')  # DC: bursts last up to 125 us
    b.fb('+3V3', '+3V3_OPT'); b.decouple('+3V3_OPT', 4, 2)
    b.add('U', LIB, 'SY88236L', 'SY88236LMG', 'Package_DFN_QFN:QFN-32-1EP_5x5mm_P0.5mm_EP3.1x3.1mm_ThermalVias',
          {'1': 'GND', '2': 'LOSLVL', '3': 'RX_SD', '4': 'RX_SD', '6': 'ONU_TD_N', '7': 'ONU_TD_P', '9': 'LD_BEN', '10': 'LD_BEN_N',
           '11': 'LASER_DIS', '12': 'APCSET', '13': 'MODSET', '14': 'BIASMAX', '15': 'MODMON', '23': 'BIASMON', '25': 'APCFAULT_N',
           '18': 'LD_MODN', '19': 'LD_MODP', '21': 'LD_BIAS', '22': 'LD_MD', '26': 'RSSI', '27': 'RIN_P', '28': 'RIN_N',
           '30': 'ONU_RD_N', '31': 'ONU_RD_P', '17': '+3V3_OPT', '20': '+3V3_OPT', '32': '+3V3_OPT',
           '5': 'GND', '8': 'GND', '16': 'GND', '24': 'GND', '29': 'GND', '33': 'GND'},
          MPN='SY88236LMG')   # DIN P/N swapped for a straight route: ECP5 TXINVPOL = Invert
    b.r('4.7k', 'LASER_DIS', '+3V3')  # TXDIS high = laser off while the ECP5 is unconfigured
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
          {'LD_A': '+3V3_OPT', 'LD_K': 'LD_K', 'MPD_A': 'LD_MD', 'MPD_K': '+3V3_OPT',
           'VAPD': 'HV_APD', 'VCC': '+3V3_RX', 'OUT+': 'TIA_P', 'OUT-': 'TIA_N', 'GND': 'GND'},
          MPN='desoldered from an X100DG; pad order to measure')
    b.block = 'apd'   # Vout = 1.2 V * (1 + 300k/10k) + (1.2 V - APD_ADJ) * 300k/20k: 55 V .. 18 V, 37 V with the DAC off
    b.fb('+3V3', '+3V3_APD'); b.decouple('+3V3_APD', 1, 1)
    b.add('U', LIB, 'TPS61391', 'TPS61391RTER', 'Package_DFN_QFN:WQFN-16-1EP_3x3mm_P0.5mm_EP1.68x1.68mm_ThermalVias',
          {'VIN': '+3V3_APD', 'EN': '+3V3_APD', '14': '+3V3_APDA', '15': '+3V3_APDA', 'AGND': 'GND', 'GND': 'GND', 'EP': 'GND',
           'FB': 'APD_FB', 'ISHORT': 'APD_ISHORT', 'CAP': 'APD_CAP', 'SW': 'HV_SW', 'MONIN': 'HV_MONIN', 'APD': 'HV_APD',
           'MON1': 'APD_MON', 'MON2': 'APD_MON2'}, MPN='TPS61391RTER')
    b.add('L', 'Device', 'L', '4.7u', 'Inductor_SMD:L_Cenker_CKCS252012', {'1': '+3V3_APD', '2': 'HV_SW'}, MPN='4.7 uH, Isat >= 1 A, 2520')
    b.add('D', 'Device', 'D_Schottky', 'BAT46WJ', 'Diode_SMD:D_SOD-323F', {'K': 'HV_OUT', 'A': 'HV_SW'}, MPN='BAT46WJ (100 V)')
    hv = dict(MPN='100 V X7R')
    b.c('100n', 'HV_OUT', fp=C0603, **hv)
    b.r('100', 'HV_OUT', 'HV_FILT'); b.c('100n', 'HV_FILT', fp=C0603, **hv)
    b.r('1k', 'HV_FILT', 'HV_MONIN')
    b.c('220p', 'HV_APD', fp=C0603, **hv)
    b.r('300k', 'HV_OUT', 'APD_FB', fp='Resistor_SMD:R_0603_1608Metric')   # 0603: 55 V across it
    b.r('10k', 'APD_FB', 'GND'); b.r('20k', 'APD_FB', 'APD_ADJ')
    b.r('33k', 'APD_ISHORT', 'GND')   # 3 mA short limit
    b.c('47n', 'APD_CAP')
    b.r('10', '+3V3_APD', '+3V3_APDA'); b.c('2.2u', '+3V3_APDA')
    b.r('1.5k', 'APD_MON', 'GND'); b.c('10p', 'APD_MON')     # 4:5 mirror: 2 mA APD -> 2.4 V
    b.r('3.01k', 'APD_MON2', 'GND'); b.c('10p', 'APD_MON2')
    b.block = 'ddm'
    b.add('U', 'Analog', 'AD5593R', 'AD5593RBRUZ', 'Package_SO:TSSOP-16_4.4x5mm_P0.65mm',
          {'~{RESET}': 'DDM_RESET', 'A0': 'GND', 'V_{DD}': '+3V3', 'V_{LOGIC}': '+3V3', 'V_{REF}': 'DDM_VREF', 'GND': 'GND',
           'SDA': 'ONU_SDA', 'SCL': 'ONU_SCL', 'IO0': 'APC_ADJ', 'IO1': 'MOD_ADJ', 'IO2': 'APD_ADJ', 'IO3': 'BIASMON',
           'IO4': 'MODMON', 'IO5': 'APD_MON', 'IO6': 'RSSI', 'IO7': 'APCFAULT_N'}, MPN='AD5593RBRUZ')
    b.r('10k', 'DDM_RESET', '+3V3'); b.c('100n', 'DDM_VREF'); b.decouple('+3V3', 1)

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
          MPN='SiT3821AI-2B2-33EB155.520000 (LVDS, +-50 ppm pull; check code)')
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
