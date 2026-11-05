# SPDX-License-Identifier: CERN-OHL-S-2.0
"""Patch a copy of the Terasic GHRD top (golden_top.v) to hold PonBoard: its pins become top ports, its HPS
lightweight windows (lwOlt, lwOnu, lwEth) take the AXI bridges that qsys_top.tcl exports from qsys_top, its
interrupts go to f2h_irq1 (bit k = k-th interrupt port of PonBoard, in port order), its management clock and reset
are the GHRD's 100 MHz system clock and reset, clock_50 is CLOCK1_50 (same pin). Writes pon_irqs.h (f2h_irq1 bits).
Usage: ghrd_top.py GOLDEN_TOP_V PONBOARD_V PIN_PORT..."""
import re
import sys

LW = ("lwOlt", "lwOnu", "lwEth")
IRQ_BITS = 32


def ports(verilog):
    head = re.search(r"module PonBoard \((.*?)\);", verilog, re.S)
    if not head:
        sys.exit("ERROR: no PonBoard module")
    found = re.findall(r"^\s*(input|output|inout)\s+wire\s+(\[[^]]+\]\s+)?(\w+)", head.group(1), re.M)
    return [(d, (w or "").strip(), n) for d, w, n in found]


def width(w):
    m = re.match(r"\[(\d+):0\]", w)
    return int(m.group(1)) + 1 if m else 1


def axi_name(name):  # lwOlt_aw_payload_addr -> lwOlt_awaddr, lwOlt_aw_valid -> lwOlt_awvalid
    lw, channel, rest = name.split("_", 2)
    return f"{lw}_{channel}{rest.replace('payload_', '')}"


def patch(text, anchor, insert, before=False):
    if text.count(anchor) != 1:
        sys.exit(f"ERROR: anchor {anchor!r} found {text.count(anchor)} times in golden_top.v")
    return text.replace(anchor, insert + anchor if before else anchor + insert)


def main():
    golden_path, pon_path, *pins = sys.argv[1:]
    golden = open(golden_path, newline="").read()
    pon = ports(open(pon_path).read())
    pin_ports = [p for p in pon if p[2] in pins or p[2].endswith("_n") and p[2][:-2] + "_p" in pins]
    irqs = [p for p in pon if p[0] == "output" and p[2].endswith("Irq")]
    irq_bits = sum(width(w) for _, w, _ in irqs)
    if irq_bits > IRQ_BITS:
        sys.exit(f"ERROR: {irq_bits} interrupts, f2h_irq1 has {IRQ_BITS}")
    top_ports = "".join(f"\n      {d:6} {w:9} {n}," for d, w, n in pin_ports)
    wires, qsys, conns = [], [], []
    for d, w, n in pon:
        if n.startswith(LW):
            wires.append(f"wire {w} pon_{n};")
            qsys.append(f"\n.{axi_name(n)} (pon_{n}),")
            conns.append(f".{n} (pon_{n})")
        elif (d, w, n) in pin_ports:
            conns.append(f".{n} ({n})")
        elif n == "clock_50":
            conns.append(".clock_50 (CLOCK1_50)")
        elif n == "clk":
            conns.append(".clk (system_clk_100_internal)")
        elif n == "reset":
            conns.append(".reset (~system_reset_n)")
        elif (d, w, n) in irqs:
            wires.append(f"wire {w} pon_{n};")
            conns.append(f".{n} (pon_{n})")
        else:
            sys.exit(f"ERROR: PonBoard port {n} has no connection rule")
    irq_vector = ", ".join(f"pon_{n}" for _, _, n in reversed(irqs))
    wires.append(f"wire [{IRQ_BITS - 1}:0] pon_irqs = {{{IRQ_BITS - irq_bits}'b0, {irq_vector}}};")
    qsys.append("\n.f2h_irq1_in_irq (pon_irqs),")
    instance = "\n// PON board (agilex5/hw/spinal/pon/PonBoard.scala)\n" + "\n".join(wires) + \
        "\nPonBoard pon (\n" + ",\n".join(conns) + "\n);\n\n"
    golden = patch(golden, "module golden_top(", top_ports)
    golden = patch(golden, "qsys_top soc_inst (", "".join(qsys))
    golden = golden[:golden.rindex("endmodule")] + instance + "endmodule\n"
    open(golden_path, "w", newline="").write(golden)
    print("f2h_irq1 bits:", " ".join(f"{n}{w}" for _, w, n in irqs))
    with open("pon_irqs.h", "w") as header:  # for the device tree (sniffer-uio.dtso)
        bit = 0
        for _, w, n in irqs:
            header.write(f"#define PON_IRQ_{n} {bit}\n")
            bit += width(w)


main()
