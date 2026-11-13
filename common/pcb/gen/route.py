# SPDX-License-Identifier: CERN-OHL-S-2.0
"""Route a placed board several ways and keep the best (operator 2026-11-05). Each strategy works on its own copy
of the board, all in parallel at nice 19 (every router is single-threaded); finish.py (outer pours, dangling copper,
DRC) closes each. Score, lowest first: unconnected, DRC errors, vias, track length (route_score.py); the input board
competes too, so a worse result never replaces it. Routing is the last step: once nothing moves pins any more.

    python3 common/pcb/gen/route.py [olt] [onu] [hsmc] [sniffer] [<path>.kicad_pcb]    (routers: FPGAcommon/hw, HW=<dir>)
    STRATEGIES=fr-engine FREEROUTING_HEAP_GB=4 python3 common/pcb/gen/route.py inline    (one strategy, a small heap:
    by default all of them run at once and each freerouting waits for 17 GB of free memory)
"""
import concurrent.futures, glob, os, shutil, subprocess, sys, tempfile
GEN = os.path.dirname(os.path.abspath(__file__))
HW = os.environ.get('HW', os.path.join(GEN, '../../../../FPGAcommon/hw'))
sys.path.insert(0, HW)
from route_score import score
from paths import board_dir
NICE = ['nice', '-n', '19']
failures = []   # steps that exited with an error: reported, their logs kept

def run(log, cmd, cwd, ok=(0,)):
    with open(log, 'a') as out:
        out.write('$ ' + ' '.join(cmd) + '\n'); out.flush()
        rc = subprocess.run(NICE + cmd, cwd=cwd, stdout=out, stderr=subprocess.STDOUT).returncode
    if rc not in ok:
        failures.append(f'{os.path.basename(cwd)}: {os.path.basename(cmd[1] if cmd[0] == "python3" else cmd[0])} exited {rc}')
    return rc

def freerouting(*args, iterations=4):
    def step(work, board, log):
        fr = os.path.join(work, 'fr')
        shutil.rmtree(fr, ignore_errors=True)
        fr_log = fr + '.log'
        open(fr_log, 'w').close()
        rc = run(fr_log, ['python3', f'{HW}/autoroute.py', board, '--passes', os.environ.get('PASSES', '20'),
                          '--iterations', str(iterations), '--work', fr, *args], work, ok=(0, 1))
        open(log, 'a').write(open(fr_log).read())
        if rc == 1 and 'nothing to save' not in open(fr_log).read():   # 1 also means: no iteration improved
            failures.append(f'{os.path.basename(work)}: autoroute.py exited 1')
        if 'not accepting' in open(fr_log).read():   # a rejected iteration: keep it minus the bad segments
            rejected = sorted(glob.glob(f'{fr}/it*.kicad_pcb'), key=lambda p: int(p.split('it')[-1].split('.')[0]))
            if rejected:
                run(log, ['python3', f'{GEN}/salvage.py', rejected[-1], board], work)
    return step

def krt(work, board, log):
    run(log, ['python3', f'{HW}/krt_route.py', board, '--layers', 'F.Cu', 'B.Cu', '--track-width', '0.1', '--rip',
              '--open-bga', '--unfill', '--apply'], work)

def mlroute(work, board, log):
    report, out = os.path.join(work, 'drc.json'), os.path.join(work, 'ml.kicad_pcb')
    run(log, ['kicad-cli', 'pcb', 'drc', '--format', 'json', '-o', report, board], work)
    if run(log, ['python3', f'{HW}/mlroute.py', board, report, out], work) == 0 and os.path.exists(out):
        shutil.move(out, board)

def engine(work, board, log):   # IA-PCB's router on what is still open, the copper already there kept
    repo = os.environ.get('IAPCB_REPO', '/mnt/data/perso/confiared/software/IA-PCB-pinout-position-route')
    out = os.path.join(work, 'engine.kicad_pcb')
    rc = run(log, [f'{repo}/krtenv/bin/python', '-u', f'{repo}/router/py_router/route.py', board, out, '--keep-input-copper'],
             f'{repo}/router')
    if rc == 0 and os.path.exists(out):
        shutil.move(out, board)

def finish(work, board, log):
    return run(log, ['python3', f'{GEN}/finish.py', board, home], work, ok=(0, 1)) == 0   # 1: not clean

def if_open(step):   # only after a finish that left the board unclean
    return ('open', step)

STRATEGIES = {   # each a list of steps; the first is the former route.sh
    'fr-krt-fr': [freerouting(), krt, finish, if_open(freerouting(iterations=2)), finish],
    'fr50-krt-fr': [freerouting('--via-cost', '50'), krt, finish, if_open(freerouting('--via-cost', '50', iterations=2)),
                    finish],
    'krt-fr': [krt, finish, if_open(freerouting()), finish],
    'fr-krt-ml': [freerouting(), krt, finish, if_open(mlroute), finish],
    # the engine alone necks power and SerDes tracks: only as the closer
    'fr-engine': [freerouting(), engine, finish, if_open(freerouting(iterations=2)), if_open(engine), finish],
}

def strategy(name, steps, src, root):
    work = os.path.join(root, name)
    os.makedirs(work)
    board = os.path.join(work, os.path.basename(src))
    for f in (src, src.replace('.kicad_pcb', '.kicad_pro')):
        shutil.copy(f, work)
    log, clean = os.path.join(work, 'log'), False
    for s in steps:
        when, step = s if isinstance(s, tuple) else ('always', s)
        if when == 'open' and clean:
            continue
        done = step(work, board, log)
        if step is finish:
            clean = done
    return board

def key(s):   # (tracks, vias, length, layers, violations, unconnected); -1: the DRC failed
    tracks, vias, length, layers, violations, unconnected = s
    worst = float('inf')
    return (unconnected if unconnected >= 0 else worst, violations if violations >= 0 else worst, vias, length)

for name in sys.argv[1:] or ['olt', 'onu']:
    src = os.path.abspath(name) if name.endswith('.kicad_pcb') else os.path.join(board_dir(name), f'{name}.kicad_pcb')
    name, home = os.path.basename(src)[:-len('.kicad_pcb')], os.path.dirname(src)
    root = tempfile.mkdtemp(prefix=f'route_{name}_')
    with concurrent.futures.ThreadPoolExecutor(len(STRATEGIES)) as pool:
        jobs = {s: pool.submit(strategy, s, STRATEGIES[s], src, root) for s in os.environ.get('STRATEGIES', ','.join(STRATEGIES)).split(',')}
    results = {'input': src}
    for s, job in jobs.items():
        try:
            results[s] = job.result()
        except Exception as e:
            print(f'{name} {s}: failed ({e}), log in {root}/{s}/log')
    print(f'{name}:')
    scores = {}
    for s, path in results.items():
        print(f'{s:>12}', end='')
        scores[s] = key(score(path))
    best = min(scores, key=scores.get)
    print(f'{name}: best {best} {scores[best]}')
    if best != 'input':
        shutil.copy(results[best], src)
    if failures:
        print(f'{name}: failed steps, logs kept in {root}:', *failures, sep='\n  ')
        failures.clear()
    else:
        shutil.rmtree(root)
