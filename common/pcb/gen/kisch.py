# SPDX-License-Identifier: CERN-OHL-S-2.0
"""Tiny KiCad 10 writer: box symbols, netlist parts, label-per-pin schematic."""
import uuid, collections
from sexp import Sym as S, dump, find, lib_symbol, pins

NS = uuid.UUID('6f1c1a52-6a0e-4f43-9a3e-7d9b2f6f1e01')
def uid(*key):
    return str(uuid.uuid5(NS, '/'.join(map(str, key))))

def font(size=1.27):
    return [S('effects'), [S('font'), [S('size'), size, size]]]

def prop(name, value, x=0.0, y=0.0, hide=False, angle=0):
    e = font()
    if hide:
        e.append([S('hide'), S('yes')])
    return [S('property'), name, value, [S('at'), x, y, angle], e]

def box_symbol(name, units, ref='U', value=None, footprint='', descr=''):
    """units: [(left_pins, right_pins)], pin = (number, name, type)."""
    sym = [S('symbol'), name, [S('pin_names'), [S('offset'), 1.016]],
           [S('exclude_from_sim'), S('no')], [S('in_bom'), S('yes')], [S('on_board'), S('yes')],
           prop('Reference', ref, 0, 3.81),
           prop('Value', value or name, 0, -max(len(units[0][0]), len(units[0][1])) * 2.54 - 1.27),
           prop('Footprint', footprint, 0, 0, True), prop('Datasheet', '', 0, 0, True),
           prop('Description', descr, 0, 0, True)]
    for u, (left, right) in enumerate(units, 1):
        longest = max([len(p[1]) for p in left + right] + [4])
        w = max(10.16, round(longest * 1.27 * (2 if left and right else 1) / 2.54 + 2) * 2.54)
        h = max(len(left), len(right)) * 2.54 + 2.54
        body = [S('symbol'), f'{name}_{u}_1',
                [S('rectangle'), [S('start'), -w / 2, 2.54], [S('end'), w / 2, 2.54 - h],
                 [S('stroke'), [S('width'), 0.254], [S('type'), S('default')]], [S('fill'), [S('type'), S('background')]]]]
        for side, plist in ((-1, left), (1, right)):
            for i, (num, pname, ptype) in enumerate(plist):
                body.append([S('pin'), S(ptype), S('line'), [S('at'), side * (w / 2 + 2.54), -i * 2.54, 0 if side < 0 else 180],
                             [S('length'), 2.54], [S('name'), pname, font()], [S('number'), num, font()]])
        sym.append(body)
    return sym

class Part:
    def __init__(self, ref, lib_id, sym, value, footprint, conns, fields, dnp, on_board, block):
        self.ref, self.lib_id, self.sym, self.value, self.footprint = ref, lib_id, sym, value, footprint
        self.conns, self.fields, self.dnp, self.on_board, self.block = conns, fields, dnp, on_board, block
        self.pins = pins(sym)

class Board:
    def __init__(self, name):
        self.name, self.parts, self.count, self.block = name, [], collections.Counter(), 'misc'

    def add(self, prefix, lib, symname, value, footprint, conns, sym=None, dnp=False, on_board=True, **fields):
        """conns: {pin number or unique pin name: net}; other pins get a no-connect flag."""
        sym = sym or lib_symbol(lib, symname)
        self.count[prefix] += 1
        ref = f'{prefix}{self.count[prefix]}'
        part = Part(ref, f'{lib.split("/")[-1].replace(".kicad_sym", "")}:{symname}', sym, value, footprint,
                    {}, fields, dnp, on_board, self.block)
        by_name = collections.defaultdict(list)
        for p in part.pins:
            by_name[p[2]].append(p[1])
        numbers = {p[1] for p in part.pins}
        for key, net in conns.items():
            if key in numbers:
                part.conns[key] = net
            elif len(by_name.get(key, [])) == 1:
                part.conns[by_name[key][0]] = net
            elif key in by_name:  # same name on several pins (GND, VCC...): all of them
                for n in by_name[key]:
                    part.conns[n] = net
            else:
                raise KeyError(f'{ref} {symname}: no pin {key}')
        self.parts.append(part)
        return ref

    def nets(self):
        out = collections.defaultdict(list)
        for p in self.parts:
            for pin, net in p.conns.items():
                out[net].append((p.ref, pin))
        return out

LABEL_JUSTIFY = {0: 'left', 90: 'left', 180: 'right', 270: 'right'}

def write_schematic(board, path, power_nets, paper=(1189, 841)):
    root = uid(board.name, 'sheet')
    lib_syms, seen = [], set()
    for p in board.parts:
        if p.lib_id not in seen:
            seen.add(p.lib_id)
            s = [x for x in p.sym]
            s[1] = p.lib_id
            lib_syms.append(s)
    items = []
    # flow layout: blocks in order, units left to right, wrap at paper width
    x0, y0, x, y, row_h = 30.0, 30.0, 30.0, 30.0, 0.0
    block = None
    pwr_flags = []
    for p in board.parts:
        units = sorted({pin[0] for pin in p.pins}) or [1]  # a pinless part (mounting hole) still needs its symbol
        for u in units:
            upins = [pin for pin in p.pins if pin[0] == u]
            xs = [pin[4] for pin in upins] or [0]
            ys = [pin[5] for pin in upins] or [0]
            lab = lambda side: max([len(p.conns.get(pin[1], '')) for pin in upins if (pin[4] - (min(xs) + max(xs)) / 2) * side > 0] + [0]) * 1.1 + 5
            w = max(xs) - min(xs) + lab(-1) + lab(1) + 10
            h = max(ys) - min(ys) + 15
            if p.block != block or x + w > paper[0] - 20:
                x, y, row_h = x0, y + row_h + 10, 0.0
                if p.block != block and block is not None:
                    y += 10
                block = p.block
            sx = round((x + lab(-1) - min(xs) + 5) / 2.54) * 2.54
            sy = round((y + max(ys) + 8) / 2.54) * 2.54
            x += w
            row_h = max(row_h, h)
            in_bom = (find(p.sym, 'in_bom') or [[None, S('yes')]])[0][1]  # as the library says (holes: no)
            inst = [S('symbol'), [S('lib_id'), p.lib_id], [S('at'), sx, sy, 0], [S('unit'), u],
                    [S('exclude_from_sim'), S('no')], [S('in_bom'), in_bom],
                    [S('on_board'), S('yes' if p.on_board else 'no')], [S('dnp'), S('yes' if p.dnp else 'no')],
                    [S('uuid'), uid(board.name, p.ref, u)],
                    prop('Reference', p.ref, sx, sy - max(ys) - 3.81),
                    prop('Value', p.value, sx, sy - min(ys) + 3.81),
                    prop('Footprint', p.footprint, sx, sy, True), prop('Datasheet', '', sx, sy, True)]
            for k, v in p.fields.items():
                inst.append(prop(k, v, sx, sy, True))
            for pin in p.pins:
                inst.append([S('pin'), pin[1], [S('uuid'), uid(board.name, p.ref, 'pin', pin[1])]])
            inst.append([S('instances'), [S('project'), board.name,
                         [S('path'), '/' + root, [S('reference'), p.ref], [S('unit'), u]]]])
            items.append(inst)
            for (unit, num, name, ptype, px, py, ang, ln) in upins:
                ex, ey = sx + px, sy - py
                net = p.conns.get(num)
                if net is None:
                    if ptype != 'no_connect':
                        items.append([S('no_connect'), [S('at'), ex, ey], [S('uuid'), uid(board.name, p.ref, 'nc', num)]])
                    continue
                a = int((ang + 180) % 360)
                items.append([S('global_label'), net, [S('shape'), S('passive')], [S('at'), ex, ey, a],
                              font() + [[S('justify'), S(LABEL_JUSTIFY[a])]],
                              [S('uuid'), uid(board.name, p.ref, 'lbl', num)]])  # global: net named as on the PCB
    # PWR_FLAG on every rail so ERC knows the rails are driven
    driven = {p.conns[pin[1]] for p in board.parts for pin in p.pins if pin[3] == 'power_out' and pin[1] in p.conns}
    power_nets = set(power_nets) - driven
    flag = lib_symbol('power', 'PWR_FLAG')
    flag_id = 'power:PWR_FLAG'
    fs = [x for x in flag]; fs[1] = flag_id
    lib_syms.append(fs)
    x, y = x0, y + row_h + 30
    for i, net in enumerate(sorted(power_nets)):
        sx, sy = round((x + i * 30) / 2.54) * 2.54, round(y / 2.54) * 2.54
        items.append([S('symbol'), [S('lib_id'), flag_id], [S('at'), sx, sy, 0], [S('unit'), 1],
                      [S('exclude_from_sim'), S('no')], [S('in_bom'), S('no')], [S('on_board'), S('no')], [S('dnp'), S('no')],
                      [S('uuid'), uid(board.name, 'flag', net)],
                      prop('Reference', f'#FLG{i + 1:02d}', sx, sy - 5, True), prop('Value', 'PWR_FLAG', sx, sy - 7),
                      prop('Footprint', '', sx, sy, True), prop('Datasheet', '', sx, sy, True),
                      [S('pin'), '1', [S('uuid'), uid(board.name, 'flagpin', net)]],
                      [S('instances'), [S('project'), board.name,
                       [S('path'), '/' + root, [S('reference'), f'#FLG{i + 1:02d}'], [S('unit'), 1]]]]])
        items.append([S('global_label'), net, [S('shape'), S('passive')], [S('at'), sx, sy, 270],
                      font() + [[S('justify'), S('right')]], [S('uuid'), uid(board.name, 'flaglbl', net)]])
    height = max(paper[1], y + 40)
    doc = [S('kicad_sch'), [S('version'), 20250114], [S('generator'), 'eeschema'], [S('generator_version'), '10.0'],
           [S('uuid'), root], [S('paper'), 'User', paper[0], round(height)],
           [S('title_block'), [S('title'), f'{board.name.upper()} SerDes bridge for DE25-Nano'],
            [S('comment'), 1, 'SPDX-License-Identifier: CERN-OHL-S-2.0']],
           [S('lib_symbols')] + lib_syms] + items + \
          [[S('sheet_instances'), [S('path'), '/', [S('page'), '1']]], [S('embedded_fonts'), S('no')]]
    open(path, 'w').write(dump(doc) + '\n')
