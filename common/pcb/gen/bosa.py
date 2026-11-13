# SPDX-License-Identifier: CERN-OHL-S-2.0
"""The GPON BOSA of the X100DG: both TO cans stand under the body, leads straight down. Lands measured on the
operator's 1200 dpi scans of a stripped X100DG (both sides, scale checked on a 2.54 mm pin row) and cross-checked
on an X111W (same pattern). Top view, origin on the RX can's axis, the TX can on +x.

LEADS maps each pad to its function; MEASURED stays False until the operator's multimeter check confirms it: the
footprints then carry a `Verify` field and fab.py refuses the board. From the copper only C (GND, on the plane) and
the pair A/B (they leave together) are sure; which of A/B is OUT+ is unknown and set so the pair reaches the limiting
amplifier without a crossing (place.py `tia_pair`): the lane's polarity is found on the first board."""

RX_PCD, TX_PCD, TX_X = 2.79, 2.97, 11.41
# the RX can's 5 leads: B alone at -y, A and C 57.6 degrees each side of it, D and E 41.5 degrees each side of +y
RX_ANGLES = {'A': -147.6, 'B': -90.0, 'C': -32.4, 'E': 48.5, 'D': 131.5}
# the TX can's 4 leads: T1 on B's side, T2 away from the RX can, T3 opposite T1, T4 toward the RX can
TX_ANGLES = {'T1': -90.0, 'T2': 0.0, 'T3': 90.0, 'T4': 180.0}
RX_DRILL, RX_PAD, TX_DRILL, TX_PAD = 0.75, 1.15, 0.85, 1.4
RX_CAN, TX_CAN = 2.7, 3.0          # can radii (TO-46 flange, TO-56): silkscreen
BODY = (-4.5, -3.5, TX_X + 4.5, 3.5)   # ~ body over both cans; the fibre leaves one end (which one: unknown)
BOOT = 8.0                            # fibre boot past either end, ~3 mm above the board: no tall part under it

MEASURED = False
LEADS = {'A': 'OUT+', 'B': 'OUT-', 'C': 'GND', 'D': 'VAPD', 'E': 'VCC',
         'T1': 'LD_A', 'T2': 'LD_K', 'T3': 'MPD_A', 'T4': 'MPD_K'}
VERIFY = {} if MEASURED else {'Verify': 'BOSA lead functions provisional (common/pcb/gen/bosa.py): measure them'}
