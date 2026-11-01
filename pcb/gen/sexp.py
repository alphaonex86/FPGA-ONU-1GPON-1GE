# SPDX-License-Identifier: CERN-OHL-S-2.0
import re
TOK = re.compile(r'\(|\)|"(?:[^"\\]|\\.)*"|[^\s()"]+')

class Sym(str):
    """Unquoted atom."""

def parse(text):
    stack, cur = [], []
    for m in TOK.finditer(text):
        t = m.group(0)
        if t == '(':
            stack.append(cur); cur = []
        elif t == ')':
            done = cur; cur = stack.pop(); cur.append(done)
        elif t[0] == '"':
            cur.append(t[1:-1].replace('\\"', '"').replace('\\\\', '\\'))
        else:
            cur.append(Sym(t))
    return cur[0]

def dump(e, ind=0):
    if isinstance(e, list):
        inner = [dump(x, ind + 1) for x in e]
        flat = '(' + ' '.join(inner) + ')'
        if len(flat) < 100 and '\n' not in flat:
            return flat
        return '(' + inner[0] + ''.join('\n' + '  ' * (ind + 1) + x for x in inner[1:]) + ')'
    if isinstance(e, Sym):
        return str(e)
    if isinstance(e, bool):
        return 'yes' if e else 'no'
    if isinstance(e, (int, float)):
        return ('%.4f' % e).rstrip('0').rstrip('.') if isinstance(e, float) else str(e)
    return '"' + str(e).replace('\\', '\\\\').replace('"', '\\"') + '"'

def find(e, key):
    return [x for x in e if isinstance(x, list) and x and x[0] == key]

_libs = {}
def lib_symbol(lib, name, root='/usr/share/kicad/symbols'):
    path = lib if lib.endswith('.kicad_sym') else f'{root}/{lib}.kicad_sym'
    if path not in _libs:
        _libs[path] = {s[1]: s for s in find(parse(open(path).read()), 'symbol')}
    syms = _libs[path]
    s = syms[name]
    ext = find(s, 'extends')
    if ext:  # flatten "extends": parent graphics/pins, child properties
        parent = [list(x) for x in syms[ext[0][1]]]
        props = {p[1]: p for p in find(s, 'property')}
        out = [Sym('symbol'), name]
        for x in parent[2:]:
            if isinstance(x, list) and x[0] == 'property' and x[1] in props:
                out.append(props.pop(x[1]))
            elif isinstance(x, list) and x[0] == 'symbol':
                out.append([x[0], x[1].replace(ext[0][1], name, 1)] + x[2:])
            else:
                out.append(x)
        out += list(props.values())
        return out
    return s

def pins(sym):
    """[(unit, number, name, type, x, y, angle, length)] of a symbol."""
    out = []
    for sub in find(sym, 'symbol'):
        unit = int(sub[1].rsplit('_', 2)[-2])
        for p in find(sub, 'pin'):
            at = find(p, 'at')[0]
            out.append((unit, find(p, 'number')[0][1], find(p, 'name')[0][1], str(p[1]),
                        float(at[1]), float(at[2]), float(at[3]) if len(at) > 3 else 0.0,
                        float(find(p, 'length')[0][1])))
    return out
