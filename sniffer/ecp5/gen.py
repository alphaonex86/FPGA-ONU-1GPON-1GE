#!/usr/bin/env python3
# SPDX-License-Identifier: CERN-OHL-S-2.0
"""Writes the ECP5 build inputs derived from code: dcu_params.vh (the DCUA parameters, one table for both lanes)
and sniffer.lpf (every pin's ball read from the routed card). Usage: gen.py <board.kicad_pcb> <out dir>"""
import re
import sys

# 2.48832 Gb/s from the 155.52 MHz VCXO: x16, no divider; 20-bit words (1:2 gear) at 124.416 MHz.
# Values after LiteICLink's serdes_ecp5.py (10BSER, hardware-proven), raw: no 8b10b, comma, word aligner or CTC.
DUAL = {
    'D_MACROPDB': '0b1', 'D_IB_PWDNB': '0b1', 'D_TXPLL_PWDNB': '0b1',
    'D_REFCK_MODE': '0b010', 'D_TX_MAX_RATE': '2.5', 'D_RX_MAX_RATE': '2.5', 'D_TX_VCO_CK_DIV': '0b000',
    'D_BITCLK_LOCAL_EN': '0b1',
    'D_CMUSETBIASI': '0b00', 'D_CMUSETI4CPP': '0d3', 'D_CMUSETI4CPZ': '0d3', 'D_CMUSETI4VCO': '0b00',
    'D_CMUSETICP4P': '0b01', 'D_CMUSETICP4Z': '0b101', 'D_CMUSETINITVCT': '0b00', 'D_CMUSETISCL4VCO': '0b000',
    'D_CMUSETP1GM': '0b000', 'D_CMUSETP2AGM': '0b000', 'D_CMUSETZGM': '0b000', 'D_SETIRPOLY_AUX': '0b01',
    'D_SETICONST_AUX': '0b01', 'D_SETIRPOLY_CH': '0b01', 'D_SETICONST_CH': '0b10', 'D_SETPLLRC': '0d1',
    'D_RG_EN': '0b0', 'D_RG_SET': '0b00', 'D_REQ_ISET': '0b011', 'D_PD_ISET': '0b11',
    'D_LOW_MARK': '0d4', 'D_HIGH_MARK': '0d12',
}
LANE = {
    'PROTOCOL': '10BSER', 'UC_MODE': '0b1', 'ENC_BYPASS': '0b1', 'DEC_BYPASS': '0b1', 'CTC_BYPASS': '0b1',
    'LSM_DISABLE': '0b1', 'ENABLE_CG_ALIGN': '0b0', 'WA_BYPASS': '0b1',
    'RPWDNB': '0b1', 'REQ_EN': '0b0', 'RX_RATE_SEL': '0d10', 'RTERM_RX': '0d19', 'RXIN_CM': '0b11',
    'RXTERM_CM': '0b11', 'CDR_MAX_RATE': '2.5', 'RX_DCO_CK_DIV': '0b000', 'RX_GEAR_MODE': '0b1',
    'FF_RX_H_CLK_EN': '0b1', 'FF_RX_F_CLK_DIS': '0b1', 'SEL_SD_RX_CLK': '0b1',
    'AUTO_FACQ_EN': '0b1', 'AUTO_CALIB_EN': '0b1', 'PDEN_SEL': '0b1',  # phase detector off on LOS
    'DCOATDCFG': '0b00', 'DCOATDDLY': '0b00', 'DCOBYPSATD': '0b1', 'DCOCALDIV': '0b000', 'DCOCTLGI': '0b011',
    'DCODISBDAVOID': '0b0', 'DCOFLTDAC': '0b00', 'DCOFTNRG': '0b001', 'DCOIOSTUNE': '0b010', 'DCOITUNE': '0b00',
    'DCOITUNE4LSB': '0b010', 'DCOIUPDNX2': '0b1', 'DCONUOFLSB': '0b100', 'DCOSCALEI': '0b01',
    'DCOSTARTVAL': '0b010', 'DCOSTEP': '0b11',
    'RLOS_SEL': '0b1', 'RX_LOS_EN': '0b1', 'RX_LOS_LVL': '0b100', 'RX_LOS_CEQ': '0b11',
    # TX: only CH0's, for its clock (the VCXO domain); the pins are open on the card
    'TPWDNB': '0b0', 'TX_GEAR_MODE': '0b1', 'FF_TX_H_CLK_EN': '0b1', 'FF_TX_F_CLK_DIS': '0b1',
    'RTERM_TX': '0d19', 'TXAMPLITUDE': '0d400',
}
PER_LANE = {0: {'TPWDNB': '0b1'}, 1: {}}

PINS = re.compile(r'^(FWD_CLK|DS\d+|US\d+)_S$|^(VCXO_PWM|DS_SD)$')
SERDES = ['DS_RX_P', 'DS_RX_N', 'US_RX_P', 'US_RX_N', 'REFCLK_P', 'REFCLK_N'] # dedicated pads: no IO type


def board_pins(pcb):
    """ECP5 ball of each signal net, from the footprint with the LFE5UM value."""
    text = open(pcb).read()
    at = text.find('LFE5UM')
    if at < 0:
        sys.exit(f'{pcb}: no LFE5UM footprint')
    start = text.rfind('(footprint ', 0, at)
    depth, end = 0, start
    for end in range(start, len(text)):
        depth += {'(': 1, ')': -1}.get(text[end], 0)
        if depth == 0:
            break
    pads = re.findall(r'\(pad "([A-Z0-9]+)".*?\(net (?:\d+ )?"([^"]+)"\)', text[start:end], re.S)
    return {net: ball for ball, net in pads if PINS.match(net) or net in SERDES}


def port(net):
    """Top-level port of a net: DS3_S -> DS_S[3]."""
    m = re.match(r'^(DS|US)(\d+)_S$', net)
    return f'{m[1]}_S[{m[2]}]' if m else net


def main(pcb, out):
    pins = board_pins(pcb)
    want = ['FWD_CLK_S', 'VCXO_PWM', 'DS_SD'] + [f'DS{i}_S' for i in range(16)] + [f'US{i}_S' for i in range(8)]
    missing = [n for n in want + SERDES if n not in pins]
    if missing:
        sys.exit(f'{pcb}: no ECP5 ball for {missing}')
    with open(f'{out}/sniffer.lpf', 'w') as f:
        for net in want:
            f.write(f'LOCATE COMP "{port(net)}" SITE "{pins[net]}";\n')
            f.write(f'IOBUF PORT "{port(net)}" IO_TYPE=LVCMOS33;\n')
        for net in SERDES:
            f.write(f'LOCATE COMP "{net}" SITE "{pins[net]}";\n')
        f.write('FREQUENCY NET "ref_clk" 124.416 MHZ;\nFREQUENCY NET "ds_clk" 124.416 MHZ;\n'
                'FREQUENCY NET "out_clk" 77.76 MHZ;\n')
    params = dict(DUAL)
    for lane, extra in PER_LANE.items():
        params.update({f'CH{lane}_{k}': v for k, v in dict(LANE, **extra).items()})
    with open(f'{out}/dcu_params.vh', 'w') as f:
        f.write(',\n'.join(f'  .{k}("{v}")' for k, v in params.items()) + '\n')


if __name__ == '__main__':
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    main(*sys.argv[1:])
