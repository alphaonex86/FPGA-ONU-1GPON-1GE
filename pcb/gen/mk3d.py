# SPDX-License-Identifier: CERN-OHL-S-2.0
"""3D models the KiCad library lacks (its footprints name them, kicad-packages3d has none), as
faceted STEP into pcb/lib/3d (finish.py points the boards' footprints at them). Balls, pads and pins
come from the KiCad footprint itself; heights marked ~ are typical values, not a vendor drawing.

    python3 mk3d.py
"""
import math, os
import pcbnew
from pcbgen import ROOT, fp_dir

OUT = os.path.join(ROOT, 'lib', '3d')
BLACK, SUBSTRATE, SOLDER, GOLD = (0.12, 0.12, 0.13), (0.30, 0.25, 0.12), (0.78, 0.78, 0.80), (0.86, 0.66, 0.22)
STEEL, CERAMIC, MARK = (0.80, 0.80, 0.83), (0.92, 0.88, 0.78), (0.55, 0.55, 0.55)

def loft(rings):
    """Closed solid through rings [(z, [(x, y), ...] counter-clockwise)]: faces as point loops, outward."""
    faces = [[(x, y, rings[0][0]) for x, y in reversed(rings[0][1])]]
    for (za, a), (zb, b) in zip(rings, rings[1:]):
        n = len(a)
        faces += [[(*a[i], za), (*a[(i + 1) % n], za), (*b[(i + 1) % n], zb), (*b[i], zb)] for i in range(n)]
    faces.append([(x, y, rings[-1][0]) for x, y in rings[-1][1]])
    return faces

def prism(poly, z0, z1):
    return loft([(z0, poly), (z1, poly)])

def box(x0, y0, z0, x1, y1, z1):
    return prism([(x0, y0), (x1, y0), (x1, y1), (x0, y1)], z0, z1)

def circle(x, y, r, n=12):
    return [(x + r * math.cos(2 * math.pi * k / n), y + r * math.sin(2 * math.pi * k / n)) for k in range(n)]

def ball(x, y, d, z0, z1, n=8, bands=3):
    """Solder ball: a sphere of diameter d cut flat at z0 (pad) and z1 (package)."""
    zc, r = (z0 + z1) / 2, d / 2
    zs = [z0 + (z1 - z0) * k / bands for k in range(bands + 1)]
    return loft([(z, circle(x, y, math.sqrt(max(r * r - (z - zc) ** 2, 0.01 * r * r)), n)) for z in zs])

class Step:
    def __init__(self):
        self.lines, self.solids, self.styles, self.dirs = [], [], {}, {}

    def add(self, text):
        ref = f'#{len(self.lines) + 100}'   # #1..#21: the fixed header entities in write()
        self.lines.append(f'{ref}={text}')
        return ref

    def solid(self, faces, color):
        pts = {}
        def point(p):
            if p not in pts:
                pts[p] = self.add("CARTESIAN_POINT('',(%s));" % ','.join(f'{v:.4f}' for v in p))
            return pts[p]
        def direction(v):
            n = math.sqrt(sum(c * c for c in v))
            key = tuple(round(c / n, 4) + 0.0 for c in v)
            if key not in self.dirs:
                self.dirs[key] = self.add("DIRECTION('',(%s));" % ','.join(f'{c:.4f}' for c in key))
            return self.dirs[key]
        refs = []
        for f in faces:   # faceted B-rep (ISO 10303-42): planar FACE_SURFACE bounded by a POLY_LOOP
            f = [tuple(round(v, 4) for v in p) for p in f]
            normal = [sum((a[i] - b[i]) * (a[j] + b[j]) for a, b in zip(f, f[1:] + f[:1])) for i, j in ((1, 2), (2, 0), (0, 1))]
            axis = self.add(f"AXIS2_PLACEMENT_3D('',{point(f[0])},{direction(normal)},{direction([b - a for a, b in zip(f[0], f[1])])});")
            plane = self.add(f"PLANE('',{axis});")
            loop = self.add("POLY_LOOP('',(%s));" % ','.join(point(p) for p in f))
            bound = self.add(f"FACE_OUTER_BOUND('',{loop},.T.);")
            refs.append(self.add(f"FACE_SURFACE('',({bound}),{plane},.T.);"))
        shell = self.add(f"CLOSED_SHELL('',({','.join(refs)}));")
        brep = self.add(f"FACETED_BREP('',{shell});")
        self.solids.append((brep, color))

    def write(self, path, name):
        head = [
            "#1=APPLICATION_CONTEXT('automotive design');",
            "#2=APPLICATION_PROTOCOL_DEFINITION('international standard','automotive_design',2000,#1);",
            "#3=PRODUCT_CONTEXT('',#1,'mechanical');",
            f"#4=PRODUCT('{name}','{name}','',(#3));",
            "#5=PRODUCT_DEFINITION_FORMATION('','',#4);",
            "#6=PRODUCT_DEFINITION_CONTEXT('part definition',#1,'design');",
            "#7=PRODUCT_DEFINITION('design','',#5,#6);",
            "#8=PRODUCT_DEFINITION_SHAPE('','',#7);",
            "#9=SHAPE_DEFINITION_REPRESENTATION(#8,#20);",
            "#10=(LENGTH_UNIT()NAMED_UNIT(*)SI_UNIT(.MILLI.,.METRE.));",
            "#11=(NAMED_UNIT(*)PLANE_ANGLE_UNIT()SI_UNIT($,.RADIAN.));",
            "#12=(NAMED_UNIT(*)SI_UNIT($,.STERADIAN.)SOLID_ANGLE_UNIT());",
            "#13=UNCERTAINTY_MEASURE_WITH_UNIT(LENGTH_MEASURE(1.E-07),#10,'distance_accuracy_value','confusion accuracy');",
            "#14=(GEOMETRIC_REPRESENTATION_CONTEXT(3)GLOBAL_UNCERTAINTY_ASSIGNED_CONTEXT((#13))"
            "GLOBAL_UNIT_ASSIGNED_CONTEXT((#10,#11,#12))REPRESENTATION_CONTEXT('',''));",
            "#15=CARTESIAN_POINT('',(0.,0.,0.));", "#16=DIRECTION('',(0.,0.,1.));", "#17=DIRECTION('',(1.,0.,0.));",
            "#18=AXIS2_PLACEMENT_3D('',#15,#16,#17);",
            f"#20=FACETED_BREP_SHAPE_REPRESENTATION('',(#18,{','.join(b for b, _ in self.solids)}),#14);",
            "#21=PRODUCT_RELATED_PRODUCT_CATEGORY('part',$,(#4));"]
        styled = []
        for brep, color in self.solids:
            if color not in self.styles:
                c = self.add("COLOUR_RGB('',%.3f,%.3f,%.3f);" % color)
                fill = self.add(f"FILL_AREA_STYLE('',({self.add(f"FILL_AREA_STYLE_COLOUR('',{c});")}));")
                side = self.add(f"SURFACE_SIDE_STYLE('',({self.add(f'SURFACE_STYLE_FILL_AREA({fill});')}));")
                usage = self.add(f"SURFACE_STYLE_USAGE(.BOTH.,{side});")
                self.styles[color] = self.add(f"PRESENTATION_STYLE_ASSIGNMENT(({usage}));")
            styled.append(self.add(f"STYLED_ITEM('color',({self.styles[color]}),{brep});"))
        self.add(f"MECHANICAL_DESIGN_GEOMETRIC_PRESENTATION_REPRESENTATION('',({','.join(styled)}),#14);")
        with open(path, 'w') as f:
            f.write("ISO-10303-21;\nHEADER;\nFILE_DESCRIPTION(('KiCad 3D model'),'2;1');\n"
                    f"FILE_NAME('{name}.step','2026-10-31T00:00:00',(''),(''),'mk3d.py','mk3d.py','');\n"
                    "FILE_SCHEMA(('AUTOMOTIVE_DESIGN { 1 0 10303 214 1 1 1 1 }'));\nENDSEC;\nDATA;\n")
            f.write('\n'.join(head + self.lines) + '\nENDSEC;\nEND-ISO-10303-21;\n')

def pads(lib, name):
    """Footprint pads in model coordinates (KiCad y points down, the model's y up)."""
    fp = pcbnew.FootprintLoad(fp_dir(lib), name)
    if fp is None:
        raise SystemExit(f'no footprint {lib}:{name}')
    return [(p.GetNumber(), pcbnew.ToMM(p.GetPosition().x), -pcbnew.ToMM(p.GetPosition().y),
             pcbnew.ToMM(p.GetSize().x), pcbnew.ToMM(p.GetSize().y), pcbnew.ToMM(p.GetDrillSize().x)) for p in fp.Pads()]

def cabga381(s):
    stand, substrate, top = 0.30, 0.36, 1.36   # ~: ball standoff, laminate, total seated height
    for _, x, y, *_ in pads('Package_BGA', 'Lattice_caBGA-381_17x17mm_Layout20x20_P0.8mm'):
        s.solid(ball(x, y, 0.45, 0, stand), SOLDER)
    s.solid(box(-8.5, -8.5, stand, 8.5, 8.5, stand + substrate), SUBSTRATE)
    s.solid(box(-8.45, -8.45, stand + substrate, 8.45, 8.45, top), BLACK)
    s.solid(prism(circle(-7.3, 7.3, 0.35), top, top + 0.01), MARK)   # pin A1 corner

def sit9121(s):
    base, lid = 0.55, 0.75   # 3.2 x 2.5 x 0.75 mm (SiT9121 datasheet), ceramic base + metal lid
    for num, x, y, w, h, _ in pads('Oscillator', 'Oscillator_SMD_SiTime_SiT9121-6Pin_3.2x2.5mm'):
        sx = 1.25 if x > 0 else -1.25   # pad from under the body out to its edge, as on the part
        s.solid(box(min(x - w / 2 + 0.25, sx), y - h / 2 + 0.1, 0, max(x + w / 2 - 0.25, sx), y + h / 2 - 0.1, 0.03), GOLD)
    s.solid(box(-1.25, -1.6, 0.03, 1.25, 1.6, base), CERAMIC)
    s.solid(box(-1.15, -1.5, base, 1.15, 1.5, lid), STEEL)
    s.solid(prism(circle(-0.75, 1.1, 0.15), lid, lid + 0.01), MARK)   # pin 1

def sfp_cage(s):
    front, back, half, h, t = -48.75, 0.0, 7.25, 8.9, 0.25   # ~ height; outline from the footprint Fab layer
    s.solid(box(front, -half, h - t, back, half, h), STEEL)
    for y0 in (-half, half - t):
        s.solid(box(front, y0, 0, back, y0 + t, h - t), STEEL)
    s.solid(box(back - t, -half + t, 0, back, half - t, h - t), STEEL)
    s.solid(box(front, -half + t, 0, -16.0, half - t, t), STEEL)   # floor up to the connector
    for y0, y1 in ((-half - 0.5, -half), (half, half + 0.5)):   # EMI springs around the bezel opening
        s.solid(box(front + 0.5, y0, 1.0, front + 3.0, y1, h - 1.0), STEEL)
    s.solid(box(front + 0.5, -half + 1.0, h, front + 3.0, half - 1.0, h + 0.5), STEEL)
    body = (-9.95, -5.65, -2.45, 5.65)   # 20-pin host connector, Fab outline; ~4 mm high, card slot open to the front
    s.solid(box(body[0], body[1], 0, body[2], body[3], 1.6), BLACK)
    s.solid(box(body[0], body[1], 1.6, body[2], -4.3, 4.0), BLACK)
    s.solid(box(body[0], 4.3, 1.6, body[2], body[3], 4.0), BLACK)
    s.solid(box(body[0] + 2.0, -4.3, 1.6, body[2], 4.3, 4.0), BLACK)
    s.solid(box(body[0], -4.3, 3.2, body[0] + 2.0, 4.3, 4.0), BLACK)
    for num, x, y, w, hh, drill in pads('Connector', 'Connector_SFP_and_Cage'):
        if num == 'CAGE':   # press-fit pins through the board
            s.solid(prism(circle(x, y, drill / 2 * 0.9, 8), -1.6, 0.0), STEEL)
        elif num:   # SMT contact tails
            s.solid(box(x - w / 2 + 0.2, y - 0.15, 0, x + w / 2 - 0.2, y + 0.15, 0.2), GOLD)
        else:   # connector alignment pegs
            s.solid(prism(circle(x, y, drill / 2 * 0.9, 12), -1.6, 0.0), BLACK)

def along_x(y, z, r, x0, x1):
    """Cylinder on the x axis: a z cylinder with its axes turned (x, y, z) -> (z, x, y), handedness kept."""
    return [[(c, a, b) for a, b, c in f] for f in prism(circle(y, z, r), x0, x1)]

def along_y(x, z, r, y0, y1):
    return [[(b, c, a) for a, b, c in f] for f in prism(circle(z, x, r), y0, y1)]

def strip(x0, y0, x1, y1, w, z0, z1):
    """Flat lead from (x0, y0) to (x1, y1)."""
    dx, dy = x1 - x0, y1 - y0
    k = w / 2 / math.hypot(dx, dy)
    n = (-dy * k, dx * k)
    return prism([(x0 - n[0], y0 - n[1]), (x1 - n[0], y1 - n[1]), (x1 + n[0], y1 + n[1]), (x0 + n[0], y0 + n[1])], z0, z1)

def bosa(s):
    """~ GPON BOSA lying on the board (pcb/gen/mksym.py footprint): TX TO-56 at the back, RX TO-46 on the side."""
    zc, lead = 3.5, 0.45
    s.solid(along_x(0, zc, 2.8, 0.0, 3.0), STEEL)                  # TX can
    s.solid(along_x(0, zc, 3.4, 3.0, 12.0), STEEL)                 # housing
    s.solid(along_x(0, zc, 2.0, 12.0, 14.0), STEEL)                # receptacle
    s.solid(along_x(0, zc, 1.6, 14.0, 22.0), BLACK)                # fiber boot, over the board edge
    s.solid(along_y(6.0, zc, 2.35, 3.0, 7.0), STEEL)               # RX can
    for num, x, y, *_ in pads('bridge', 'BOSA_GPON'):
        s.solid(prism(circle(x, y, lead / 2, 8), -1.6, zc), GOLD)
        to = (0.0, y * 0.5) if num[0] == 'T' else (6.0 + (x - 6.0) * 0.4, 7.0)
        s.solid(strip(x, y, *to, lead, zc - lead / 2, zc + lead / 2), GOLD)

MODELS = {'Lattice_caBGA-381_17x17mm_Layout20x20_P0.8mm': cabga381,
          'Oscillator_SMD_SiTime_SiT9121-6Pin_3.2x2.5mm': sit9121,
          'Connector_SFP_and_Cage': sfp_cage,
          'BOSA_GPON': bosa}

if __name__ == '__main__':
    os.makedirs(OUT, exist_ok=True)
    for name, make in MODELS.items():
        s = Step()
        make(s)
        s.write(os.path.join(OUT, name + '.step'), name)
        print(name, len(s.solids), 'solids')
