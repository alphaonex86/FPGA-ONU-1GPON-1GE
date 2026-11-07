# SPDX-License-Identifier: CERN-OHL-S-2.0
"""FPGA usage pictures of a fit, rewritten by fit.sh after every PonBoard or GHRD fit so they never go stale:
<out>/<name>-resources.svg (every resource, used and free), <name>-blocks.svg (ALMs and M20K per block, free
included), <name>-onu.svg / <name>-olt.svg (the cores' sub-blocks), and <out>/README.md showing them.

    python3 usage_svg.py <fit report base, e.g. DIR/PonBoard> <name> <out dir>
"""
import html, os, re, sys

LIGHT = ['#2a78d6', '#eb6834', '#1baf7a', '#eda100', '#e87ba4', '#008300', '#6250d6']   # validated categorical order
DARK = ['#3987e5', '#d95926', '#199e70', '#c98500', '#d55181', '#008300', '#9085e9']
def rules(surface, ink, ink2, track, series):
    return (f'.bg {{ fill: {surface}; }} text {{ fill: {ink}; }} .t2 {{ fill: {ink2}; }} .track {{ fill: {track}; }} '
            + ' '.join(f'.s{i} {{ fill: {c}; }}' for i, c in enumerate(series)))
STYLE = ('<style>text { font: 13px system-ui, sans-serif; } .h { font-size: 16px; font-weight: 600; } '   # literal colors:
         + rules('#fcfcfb', '#0b0b0b', '#52514e', '#e6e5e0', LIGHT)                                       # librsvg has no var()
         + ' @media (prefers-color-scheme: dark) { ' + rules('#1a1a19', '#f0efec', '#c3c2b7', '#33332f', DARK) + ' }</style>')
W, LABEL, PAD = 760, 190, 16

# category -> (core, children) of the entity tree; 'Platform & other' takes the rest of the used logic
CATEGORIES = [('ONU PON MAC', 'onuCore', ('pon_mac', 'epon_mac')),
              ('ONU acceleration', 'onuCore', ('bridge', 'bridgeQueues_lan', 'bridgeQueues_lanControl',
                                              'bridgeQueues_wan', 'bridgeQueues_wanControl')),
              ('OLT PON MAC', 'oltCore', ('pon_mac', 'epon_mac', 'epon_rx')),
              ('OLT acceleration', 'oltCore', ('bridge', 'bridgeQueues_lan', 'bridgeQueues_lanControl',
                                              'bridgeQueues_wan', 'bridgeQueues_wanControl')),
              ('Ethernet ports', None, ('ethCore', 'ethLane', 'oltCore/epon_eth', 'oltCore/epon_ethQueues')),
              ('Sniffer', 'onuCore', ('pon_tap',)),
              ('Platform & other', None, ())]
NAMES = {'pon_mac': 'GPON MAC', 'epon_mac': 'EPON MAC', 'epon_rx': 'EPON RX', 'pon_tap': 'Sniffer',
         'queues': 'CPU queues (OMCI)', 'epon_eth': 'SFP0 Ethernet port', 'epon_ethQueues': 'SFP0 Ethernet queues',
         'bridgeQueues_lan': 'Miss queue LAN', 'bridgeQueues_wan': 'Miss queue WAN',
         'bridgeQueues_lanControl': 'Control queue LAN', 'bridgeQueues_wanControl': 'Control queue WAN',
         'board': 'Board registers', 'eponVcxo_loop_tracker': 'VCXO tracker EPON', 'gponVcxo_loop_tracker': 'VCXO tracker GPON'}

class Node:
    def __init__(self, name, alm, m20k):
        self.name, self.alm, self.m20k, self.kids = name, alm, m20k, []
    def find(self, name):   # first node of that name, depth first
        if self.name == name:
            return self
        return next((f for k in self.kids if (f := k.find(name))), None)
    def child(self, path):
        node = self
        for part in path.split('/'):
            node = next((k for k in node.kids if k.name == part), None) if node else None
        return node

def entity_tree(rpt):
    lines = open(rpt).read().split('\n')
    start = next(i for i, l in enumerate(lines) if l.startswith('; Fitter Resource Utilization by Entity'))
    head = [c.strip() for c in lines[start + 2].split(';')]
    ia, im = head.index('ALMs needed [=A-B+C]'), head.index('M20Ks')
    root, stack = None, []
    for line in lines[start + 4:]:
        if line.startswith('+'):
            break
        cells = line.split(';')
        depth = len(cells[1]) - len(cells[1].lstrip(' |')) if cells[1].strip(' |') else 0   # the root's name is empty
        node = Node(cells[1].strip(' |'), float(cells[ia].split()[0]), int(cells[im]))
        while stack and stack[-1][0] >= depth:
            stack.pop()
        if stack:
            stack[-1][1].kids.append(node)
        else:
            root = node
        stack.append((depth, node))
    return root

def summary(path):
    text = open(path).read()
    res = [(m[1].strip(), int(m[2].replace(',', '')), int(m[3].replace(',', '')))
           for m in re.finditer(r'^(?:Total )?(.+?) : ([\d,]+) / ([\d,]+)', text, re.M)]
    date = re.search(r'Fitter Status : \S+ - (.+)', text)
    device = re.search(r'Device : (\S+)', text)
    return {n: (u, t) for n, u, t in res}, date[1] if date else '?', device[1] if device else '?'

def svg(height, body, title, sub):
    body = text(PAD, 28, title, 'h') + text(PAD, 48, sub, 't2') + body
    return (f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{height}" viewBox="0 0 {W} {height}" '
            f'role="img"><title>{html.escape(title)}</title>{STYLE}'
            f'<rect width="{W}" height="{height}" class="bg"/>{body}</svg>\n')

def text(x, y, s, cls='', anchor='start'):
    return f'<text x="{x}" y="{y}" class="{cls}" text-anchor="{anchor}">{html.escape(s)}</text>'

def bar(x, y, w, h, cls, tip):   # 4 px rounded ends; native tooltip on hover
    return f'<rect x="{x:.1f}" y="{y}" width="{max(w, 0):.1f}" height="{h}" rx="4" class="{cls}"><title>{html.escape(tip)}</title></rect>'

def pct(u, t):
    return f'({100 * u / t:.0f} %)' if t else ''

def resources_svg(res, title, sub):
    rows = [(n, u, t) for n, (u, t) in res.items() if t]
    y, body, span = 70, [], W - LABEL - 2 * PAD - 160
    for name, used, total in rows:
        body += [text(PAD, y + 13, name[0].upper() + name[1:]), bar(LABEL, y, span, 18, 'track', f'{name}: {total - used:,} free'),
                 bar(LABEL, y, span * used / total, 18, 's0', f'{name}: {used:,} used'),
                 text(LABEL + span + 10, y + 13, f'{used:,} / {total:,}  {pct(used, total)}', 't2')]
        y += 30
    return svg(y + 10, ''.join(body), title, sub)

def blocks_svg(cats, totals, title, sub):
    body, y, span = [], 70, W - LABEL - 2 * PAD
    for key, unit in (('alm', 'ALMs'), ('m20k', 'M20K RAM')):
        total, x = totals[key], LABEL
        body.append(text(PAD, y + 17, unit))
        for i, (name, v) in enumerate(cats):
            w = span * v[key] / total
            body.append(bar(x, y, w - 2, 24, f's{i}', f'{name}: {v[key]:,.0f} {unit} ({pct(v[key], total)})'))
            x += w
        free = total - sum(v[key] for _, v in cats)
        body.append(bar(x, y, LABEL + span - x, 24, 'track', f'Free: {free:,.0f} {unit} ({pct(free, total)})'))
        y += 40
    y += 10   # legend = table view: every value in text
    body += [text(LABEL + 230, y, 'ALMs', 't2', 'end'), text(LABEL + 380, y, 'M20K', 't2', 'end')]
    for i, (name, v) in enumerate(cats + [('Free', {k: totals[k] - sum(c[k] for _, c in cats) for k in totals})]):
        y += 22
        body += [f'<rect x="{PAD}" y="{y - 11}" width="12" height="12" rx="3" class="{f"s{i}" if name != "Free" else "track"}"/>',
                 text(PAD + 20, y, name),
                 text(LABEL + 230, y, f'{v["alm"]:,.0f}  {pct(v["alm"], totals["alm"])}', 't2', 'end'),
                 text(LABEL + 380, y, f'{v["m20k"]:,.0f}  {pct(v["m20k"], totals["m20k"])}', 't2', 'end')]
    return svg(y + 16, ''.join(body), title, sub)

def core_svg(core, device_alm, title, sub):
    rows = []
    for k in core.kids:
        if k.name == 'bridge':   # split the acceleration: flow engine (classifiers + rewriters) and the L2 bridge
            flows = [g for g in k.kids if re.search(r'_flows$|^flowRewrite', g.name)]
            fa, fm = sum(g.alm for g in flows), sum(g.m20k for g in flows)
            rows += [('Flow engine (NAT, PPPoE)', fa, fm), ('L2 bridge, VLAN, queues', k.alm - fa, k.m20k - fm)]
        else:
            rows.append((NAMES.get(k.name, k.name), k.alm, k.m20k))
    rows.sort(key=lambda r: -r[1])
    big = [r for r in rows if r[1] >= 100]
    small = [r for r in rows if r[1] < 100]
    if small:
        big.append(('Other', sum(r[1] for r in small), sum(r[2] for r in small)))
    y, body, span = 70, [], W - LABEL - 2 * PAD - 190
    top = max(r[1] for r in big)
    for name, alm, m20k in big:
        body += [text(PAD, y + 13, name), bar(LABEL, y, span * alm / top, 18, 's0', f'{name}: {alm:,.0f} ALMs, {m20k} M20K'),
                 text(LABEL + span * alm / top + 8, y + 13, f'{alm:,.0f} ALMs {pct(alm, device_alm)}  {m20k} M20K', 't2')]
        y += 28
    return svg(y + 10, ''.join(body), title, sub)

def main(base, name, out):
    rpt, summ = f'{base}.fit.place.rpt', f'{base}.fit.summary'
    for f in (rpt, summ):
        if not os.path.exists(f):
            sys.exit(f'usage_svg: no {f}')
    root, (res, date, device) = entity_tree(rpt), summary(summ)
    totals = {'alm': res['Logic utilization (in ALMs)'][1], 'm20k': res['RAM Blocks'][1]}
    used = {'alm': res['Logic utilization (in ALMs)'][0], 'm20k': res['RAM Blocks'][0]}
    cats = []
    def lookup(core, path):   # 'core/child' or a top-level name, in core when given
        first, _, rest = (f'{core}/{path}' if core else path).partition('/')
        node = root.find(first)
        return node.child(rest) if node and rest else node
    for cat, core, kids in CATEGORIES[:-1]:
        nodes = [lookup(core, k) for k in kids]
        missing = [k for k, n in zip(kids, nodes) if n is None]
        if missing:
            sys.exit(f'usage_svg: {cat}: no {missing} in {rpt} (update CATEGORIES)')
        cats.append((cat, {'alm': sum(n.alm for n in nodes), 'm20k': sum(n.m20k for n in nodes)}))
    cats.append((CATEGORIES[-1][0], {k: used[k] - sum(v[k] for _, v in cats) for k in used}))
    os.makedirs(out, exist_ok=True)
    sub = f'{name} fit, {device}, {date}'
    pictures = {'resources': resources_svg(res, 'FPGA resources: used and free', sub),
                'blocks': blocks_svg(cats, totals, 'Logic and RAM per block, free included', sub)}
    for core in ('onuCore', 'oltCore'):
        node = root.find(core)
        pictures[core[:3]] = core_svg(node, totals['alm'], f'{core[:3].upper()} core: {node.alm:,.0f} ALMs '
                                      f'{pct(node.alm, totals["alm"])}, {node.m20k} M20K {pct(node.m20k, totals["m20k"])}', sub)
    for kind, content in pictures.items():
        open(f'{out}/{name}-{kind}.svg', 'w').write(content)
    sets = sorted({f.rsplit('-', 1)[0] for f in os.listdir(out) if f.endswith('.svg')})
    open(f'{out}/README.md', 'w').write('# FPGA usage\n\nWritten by `agilex5/gts/usage_svg.py` at the end of every '
        '`fit.sh` PonBoard or ghrd fit: do not edit.\n' + ''.join(
            f'\n## {s}\n\n' + ''.join(f'![{s} {k}]({s}-{k}.svg)\n\n' for k in ('resources', 'blocks', 'onu', 'olt'))
            for s in sets))
    print(f'usage_svg: {out}/{name}-*.svg')

if __name__ == '__main__':
    if len(sys.argv) != 4:
        sys.exit(__doc__)
    main(*sys.argv[1:])
