// SPDX-License-Identifier: CERN-OHL-S-2.0
// Dump rules of the in-line sniffer (RuleStore, common/hw/spinal/pon/sniffer/DumpRules.scala): which Ethernet frames
// of the kept Port-IDs become records. The first rule that matches a frame decides.
#ifndef RULES_H
#define RULES_H

#include <stdint.h>

#define RULES_MAX 16
#define RULES_BYTES 20 // of the frame, from its destination address

enum rules_action { RULES_KEEP, RULES_DROP, RULES_HEAD };

struct rule {
    unsigned action;
    int ds, us;
    int port; // Port-ID, -1 any
    uint8_t value[RULES_BYTES], mask[RULES_BYTES];
};

struct rules {
    struct rule list[RULES_MAX];
    unsigned count;
    unsigned other; // the action for a frame no rule matches
    unsigned snap;  // bytes RULES_HEAD keeps
};

#define RULES_DEFAULT {.other = RULES_KEEP, .snap = 64}
#define RULES_SYNTAX                                                                                \
    "ACTION[,FIELD]...  ACTION: keep, drop or head (the first -S bytes); every FIELD must match:\n" \
    "             ds | us          one direction (default both)\n"                                   \
    "             port=N           GEM Port-ID\n"                                                    \
    "             multicast        group destination address (broadcast included)\n"                \
    "             vlan=N           802.1Q tag (8100) with VLAN N after the addresses\n"             \
    "             OFFSET=HEX[/MASK] frame bytes from OFFSET (0-19), e.g. 12=0806 or 0=01/01\n"

// keep, drop or head; -1 for anything else (logged).
int rules_action(const char *text, unsigned *action);
// Appends the rule written as RULES_SYNTAX; -1 on an error (logged).
int rules_add(struct rules *r, const char *text);
// Every rule slot of the device (the unused ones off), the default action and the snap length; -1 when a write failed.
int rules_write(const struct rules *r, void *ctx, int (*write)(void *ctx, unsigned offset, uint32_t value));

#endif
