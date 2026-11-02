# Realistic view: prompt for Nano Banana Pro

A photo of the finished device, installed on the customer side (operator, 2026-11-02): the ISP's
OLT is never in the picture, only the wall fibre outlet, our ONU and the sniffer between them.

Attach as references: `sniffer-diagram.png` (topology), the photo of an ONU with its BOSAs
(`../../../convert-ONU-to-OLT/WhatsApp Image 2026-11-01 at 8.55.28 AM.jpeg`, real BOSA look) and,
if at hand, a photo of the DE25-Nano itself (its exact colour and layout).

## Prompt

Photograph of a finished, working hardware device taken with a phone camera at home, natural
light, slightly off-axis 3/4 top view, sharp focus on the device, realistic everyday background.

Scene: a white wall-mounted FTTH fibre outlet box (customer fibre rosette) near the floor or on a
desk wall, with a yellow single-mode drop cord coming out of it. On the desk below, a white home
GPON ONU router with green status LEDs. Between them, the sniffer device:

- A Terasic DE25-Nano FPGA development board (credit-card sized, Altera Agilex 5 SoC with a small
  heatsink, RJ45 Gigabit Ethernet jack, USB-C ports, microSD slot, 2x20-pin 2.54 mm headers),
  standing on four brass M3 standoffs.
- Plugged on top of its 40-pin GPIO header, a small custom daughter card about 64 x 66 mm, green
  solder mask, white silkscreen "GPON SNIFFER", ENIG pads: a Lattice ECP5 BGA chip (17 mm), a tiny
  3.2 x 2.5 mm oscillator, two 3x3 mm QFN chips, two small inductors, rows of 0402 parts, a 6-pin
  JTAG header. Real assembly details: solder fillets, slight flux residue, part markings.
- Two GPON BOSA optical sub-assemblies on the card edge: small metal cylinders about 6 mm wide and
  28 mm long, gold TO-can leads soldered into the card, each with a thin white 0.9 mm fibre pigtail.
- The optical tap, neatly tied with velcro next to the boards: two thin stainless-steel 1x2 FBT
  splitters (about 3 x 54 mm, one fibre on one side and two on the other; small labels "50:50" and
  "90:10") back to back in the yellow cord between the wall outlet and the ONU; two short pigtails go to the
  BOSAs, one of them through a small inline SC attenuator marked "5 dB". Green SC/APC connectors
  and mating adapters.
- A USB-C power cable, and a blue Ethernet cable from the DE25-Nano to a laptop partly in frame
  showing Wireshark with two capture interfaces named "downstream" and "upstream".

No glowing lasers; the device only listens. Style: genuine photo, not a render: real materials (FR4
texture, fibre jacket sheen, cable slack, a little dust), natural shadows, no infographic, no
cartoon, readable text only on the silkscreen and labels named above, 4:3.

## Variant: annotated

Same photo with thin white callout lines and small sans-serif labels: "wall fibre outlet (ISP)",
"1x2 50:50 #1", "1x2 90:10 #2", "our ONU", "downstream tap 1490 nm", "upstream tap 1310 nm",
"5 dB attenuator", "BOSA RX only", "ECP5 SerDes card", "DE25-Nano (Agilex 5 + HPS)",
"pcapng to Wireshark".
