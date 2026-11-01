# SPDX-License-Identifier: CERN-OHL-S-2.0
"""HSMC connector pinout (HSMC specification v1.7, differential host table) and the DE25-Standard signal names."""
import re

def pinout():
    """{pin: function}; 161-172 are the ground bars."""
    pins = {}
    for k in range(8):   # bank 1: transceivers, lane 7 first
        b = 1 + 4 * (7 - k)
        pins.update({b: f'XCVR_TXp{k}', b + 1: f'XCVR_RXp{k}', b + 2: f'XCVR_TXn{k}', b + 3: f'XCVR_RXn{k}'})
    pins.update({33: 'SDA', 34: 'SCL', 35: 'JTAG_TCK', 36: 'JTAG_TMS', 37: 'JTAG_TDO', 38: 'JTAG_TDI',
                 39: 'CLKOUT0', 40: 'CLKIN0', 41: 'D0', 42: 'D1', 43: 'D2', 44: 'D3'})
    quad = lambda p, n: pins.update({p: f'LVDS_TXp{n}', p + 1: f'LVDS_RXp{n}', p + 2: f'LVDS_TXn{n}', p + 3: f'LVDS_RXn{n}'})
    for g in range(9):   # bank 2: (3.3 V, 12 V, 4 signals) x 9, the last 4 being clock 1
        p = 45 + 6 * g
        pins.update({p: '3.3V', p + 1: '12V'})
        quad(p + 2, g) if g < 8 else pins.update({p + 2: 'CLKOUT1p', p + 3: 'CLKIN1p', p + 4: 'CLKOUT1n', p + 5: 'CLKIN1n'})
    pins.update({99: '3.3V', 100: '12V'})
    for g in range(9):   # bank 3: (4 signals, 3.3 V, 12 V) x 9, then clock 2
        p = 101 + 6 * g
        quad(p, 8 + g)
        pins.update({p + 4: '3.3V', p + 5: '12V'})
    pins.update({155: 'CLKOUT2p', 156: 'CLKIN2p', 157: 'CLKOUT2n', 158: 'CLKIN2n', 159: '3.3V', 160: 'PSNTn'})
    pins.update({p: 'GND' for p in range(161, 173)})
    return pins

def de25_name(fn):
    """DE25-Standard user manual (Table 3-18) name of an HSMC function."""
    fixed = {'D0': 'HSMC_D[0]', 'D1': 'HSMC_D[1]', 'D2': 'HSMC_D[2]', 'D3': 'HSMC_D[3]', 'CLKOUT0': 'HSMC_D[72]',
             'CLKOUT1p': 'HSMC_D[73]', 'CLKOUT1n': 'HSMC_D[74]', 'CLKIN1n': 'HSMC_D[75]', 'CLKOUT2p': 'HSMC_D[76]',
             'CLKOUT2n': 'HSMC_B5B_D[0]', 'CLKIN2n': 'HSMC_B5B_D[1]', 'CLKIN0': 'HSMC_CLKIN0', 'CLKIN1p': 'HSMC_CLKIN1',
             'CLKIN2p': 'HSMC_CLKIN2', 'SDA': 'FPGA_I2C_SDA', 'SCL': 'FPGA_I2C_SCL',
             'XCVR_RXp6': 'HSMC_GTS_REFCLK_P', 'XCVR_RXn6': 'HSMC_GTS_REFCLK_N',
             'XCVR_RXp7': 'HSMC_GTS_RX_REFCLK_P', 'XCVR_RXn7': 'HSMC_GTS_RX_REFCLK_N'}
    if fn in fixed:
        return fixed[fn]
    m = re.fullmatch(r'LVDS_(TX|RX)([pn])(\d+)', fn)
    if m:
        return f'HSMC_D[{4 + 4 * int(m.group(3)) + (2 if m.group(1) == "RX" else 0) + (m.group(2) == "n")}]'
    m = re.fullmatch(r'XCVR_(TX|RX)([pn])([0-3])', fn)
    if m:
        return f'HSMC_GTS_{m.group(1)}_{m.group(2).upper()}[{m.group(3)}]'
    return ''
