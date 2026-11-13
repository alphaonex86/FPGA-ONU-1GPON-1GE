# SPDX-License-Identifier: CERN-OHL-S-2.0
"""JLCPCB order package of a routed board: <board dir>/fab/ with the gerber zip, BOM and CPL in JLC's columns and a
README of what JLC cannot place (plus <board dir>/order-notes.md); a part with a `Verify` field stops it. Parts come from <board dir>/lcsc.csv (value,
footprint, MPN -> LCSC code, or 'consign' / 'hand'); a part missing there or a DRC error stops it.

    python3 common/pcb/gen/fab.py hsmc [--stock]     (--stock: current JLC stock of every code, network)
"""
import collections, csv, json, os, shutil, subprocess, sys, tempfile, urllib.request
from paths import board_dir

def cli(*args):
    r = subprocess.run(['kicad-cli', *args], capture_output=True, text=True)
    if r.returncode:
        sys.exit(f'kicad-cli {args[0]} {args[1]} failed:\n{r.stdout}{r.stderr}')
    return r.stdout

def jlc_stock(code):
    req = urllib.request.Request(
        'https://jlcpcb.com/api/overseas-pcb-order/v1/shoppingCart/smtGood/selectSmtComponentList',
        data=json.dumps({'keyword': code, 'currentPage': 1, 'pageSize': 5}).encode(),
        headers={'Content-Type': 'application/json', 'User-Agent': 'Mozilla/5.0'})
    rows = json.load(urllib.request.urlopen(req, timeout=30))['data']['componentPageInfo']['list'] or []
    return next((c['stockCount'] for c in rows if (c.get('lcscGoodsUrl') or '').endswith(f'_{code}.html')), None)

name, stock = sys.argv[1], '--stock' in sys.argv
d = board_dir(name)
pcb, sch, out = f'{d}/{name}.kicad_pcb', f'{d}/{name}.kicad_sch', f'{d}/fab'
with tempfile.TemporaryDirectory() as tmp:
    cli('pcb', 'drc', '--schematic-parity', '--severity-error', '--format', 'json', '-o', f'{tmp}/drc.json', pcb)
    drc = json.load(open(f'{tmp}/drc.json'))
    errors = sum(len(drc.get(k, [])) for k in ('violations', 'unconnected_items', 'schematic_parity'))
    if errors:
        sys.exit(f'{name}: {errors} DRC errors, unconnected items or parity issues: not packaged')
    cli('sch', 'export', 'bom', '--exclude-dnp', '--fields', 'Reference,Value,Footprint,MPN,Verify',
        '--group-by', 'Value,Footprint,MPN', '-o', f'{tmp}/bom.csv', sch)
    groups = list(csv.DictReader(open(f'{tmp}/bom.csv')))
    unsure = [f"  {g['Reference']}: {g['Verify']}" for g in groups if g['Verify']]
    if unsure:   # a part marked to check first (boards.py: a `Verify` field)
        sys.exit(f'{name}: not packaged, to verify first:\n' + '\n'.join(unsure))
    cli('pcb', 'export', 'pos', '--format', 'csv', '--units', 'mm', '--side', 'both', '--exclude-dnp', '-o', f'{tmp}/pos.csv', pcb)
    pos = {r['Ref']: r for r in csv.DictReader(open(f'{tmp}/pos.csv'))}
    shutil.rmtree(out, ignore_errors=True)
    os.makedirs(f'{out}/gerber')
    cli('pcb', 'export', 'gerbers', '--layers', 'F.Cu,In1.Cu,In2.Cu,B.Cu,F.Paste,B.Paste,F.SilkS,B.SilkS,F.Mask,B.Mask,Edge.Cuts',
        '-o', f'{out}/gerber', pcb)   # gerbers, drill and CPL all on the page origin
    cli('pcb', 'export', 'drill', '--format', 'excellon', '--drill-origin', 'plot', '--excellon-separate-th',
        '-o', f'{out}/gerber/', pcb)

parts = {(r['value'], r['footprint'], r['mpn']): r for r in csv.DictReader(open(f'{d}/lcsc.csv'))}
missing = [g for g in groups if (g['Value'], g['Footprint'], g['MPN']) not in parts]
if missing:
    sys.exit(f'{name}: not in {d}/lcsc.csv:\n' + '\n'.join(f"  {g['Value']},{g['Footprint']},{g['MPN']} ({g['Reference']})" for g in missing))

def refs(text):   # "R13-R18,R21" -> ['R13', ..., 'R18', 'R21']
    out = []
    for item in text.split(','):
        if '-' in item:
            a, b = item.split('-')
            prefix = a.rstrip('0123456789')
            out += [f'{prefix}{i}' for i in range(int(a[len(prefix):]), int(b[len(prefix):]) + 1)]
        else:
            out.append(item)
    return out

bom, cpl, offboard = [], [], collections.defaultdict(list)
for g in groups:
    part, rs = parts[(g['Value'], g['Footprint'], g['MPN'])], refs(g['Reference'])
    if part['lcsc'] in ('consign', 'hand'):
        offboard[part['lcsc']].append((g['Reference'], g['MPN'] or g['Value'], part['note']))
        if part['lcsc'] == 'hand':
            continue
    code = '' if part['lcsc'] == 'consign' else part['lcsc']
    same = next((row for row in bom if code and row[3] == code), None)   # one BOM line per JLC part
    if same:
        same[1] += ',' + ','.join(rs)
    else:
        bom.append([g['Value'], ','.join(rs), g['Footprint'].split(':')[1], code])
    for r in rs:
        p = pos[r]
        cpl.append([r, f"{float(p['PosX']):.4f}mm", f"{float(p['PosY']):.4f}mm", 'Top' if p['Side'] == 'top' else 'Bottom',
                    f"{float(p['Rot']):.1f}"])
with open(f'{out}/{name}-bom.csv', 'w', newline='') as f:
    csv.writer(f).writerows([['Comment', 'Designator', 'Footprint', 'LCSC Part #'], *bom])
with open(f'{out}/{name}-cpl.csv', 'w', newline='') as f:
    csv.writer(f).writerows([['Designator', 'Mid X', 'Mid Y', 'Layer', 'Rotation'], *cpl])
shutil.make_archive(f'{out}/{name}-gerber', 'zip', f'{out}/gerber')

lines = [f'# {name}: JLCPCB order', '',
         'Upload `' + f'{name}-gerber.zip' + '`: 4 layers, 1.6 mm, JLC04161H-7628 stackup (the SerDes widths assume it), '
         'ENIG (BGA / 0.5 mm pitch parts). PCBA: both sides; '
         f'`{name}-bom.csv` and `{name}-cpl.csv`. Check every rotation in JLC\'s placement preview: KiCad and JLC '
         'footprint zero orientations differ for some packages.', '']
for kind, title in (('consign', 'Supplied by us (consigned, or JLC global sourcing)'), ('hand', 'Soldered by hand after assembly')):
    if offboard[kind]:
        lines += [f'## {title}', ''] + [f'- {r}: {m} - {n}' for r, m, n in offboard[kind]] + ['']
notes = [(g['Reference'], parts[(g['Value'], g['Footprint'], g['MPN'])]['note']) for g in groups
         if parts[(g['Value'], g['Footprint'], g['MPN'])]['lcsc'].startswith('C') and 'by hand' in parts[(g['Value'], g['Footprint'], g['MPN'])]['note']]
if notes:
    lines += ['## Extra parts to buy with the order', ''] + [f'- {r}: {n}' for r, n in notes] + ['']
if os.path.exists(f'{d}/order-notes.md'):
    lines.append(open(f'{d}/order-notes.md').read())
open(f'{out}/README.md', 'w').write('\n'.join(lines))
print(f'{name}: {len(bom)} BOM lines, {len(cpl)} placements, {sum(map(len, offboard.values()))} off the JLC line -> {out}')

if stock:
    for row in bom:
        if row[3]:
            n = jlc_stock(row[3])
            need = len(row[1].split(','))
            print(f"{row[3]:9s} {'?' if n is None else n:>9} {'LOW' if n is None or n < need else '':4s} {row[0]} ({row[1]})")
