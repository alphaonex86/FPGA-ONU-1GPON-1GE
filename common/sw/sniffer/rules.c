// SPDX-License-Identifier: CERN-OHL-S-2.0
#include "rules.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef uint32_t u32;
#include "inline_regs.h"

_Static_assert(RULES_MAX == (INLINE_RULE_SELECT_RULE_MASK >> INLINE_RULE_SELECT_RULE_SHIFT) + 1, "rule slots differ");

int rules_action(const char *text, unsigned *action)
{
    static const char *const names[] = {"keep", "drop", "head"};
    for (unsigned i = 0; i < 3; i++)
        if (strcmp(text, names[i]) == 0) {
            *action = i;
            return 0;
        }
    fprintf(stderr, "sniffer: action '%s': keep, drop or head\n", text);
    return -1;
}

static int number(const char *text, unsigned long max, unsigned long *value)
{
    char *end;
    *value = strtoul(text, &end, 0);
    return *text && !*end && *value <= max ? 0 : -1;
}

// `digits` (hex, whole bytes) into `out`; the count of bytes, -1 when they are not that or over RULES_BYTES.
static int hex_bytes(const char *digits, size_t length, uint8_t *out)
{
    if (length == 0 || length % 2 || length / 2 > RULES_BYTES)
        return -1;
    for (size_t i = 0; i < length; i++) {
        const char *hex = "0123456789abcdef", *found = strchr(hex, digits[i] | 0x20);
        if (!digits[i] || !found)
            return -1;
        unsigned nibble = (unsigned)(found - hex);
        out[i / 2] = (uint8_t)(i % 2 ? out[i / 2] | nibble : nibble << 4);
    }
    return (int)(length / 2);
}

static void bytes_field(struct rule *rule, unsigned at, const uint8_t *value, const uint8_t *mask, unsigned count)
{
    for (unsigned i = 0; i < count; i++) {
        rule->value[at + i] = (uint8_t)((rule->value[at + i] & ~mask[i]) | (value[i] & mask[i]));
        rule->mask[at + i] |= mask[i];
    }
}

static int field(struct rule *rule, const char *text)
{
    unsigned long value;
    const char *equal = strchr(text, '=');
    if (strcmp(text, "ds") == 0 || strcmp(text, "us") == 0) {
        rule->ds = text[0] == 'd';
        rule->us = !rule->ds;
    } else if (strcmp(text, "multicast") == 0) {
        bytes_field(rule, 0, (const uint8_t[]){1}, (const uint8_t[]){1}, 1);
    } else if (strncmp(text, "port=", 5) == 0 && number(text + 5, 4095, &value) == 0) {
        rule->port = (int)value;
    } else if (strncmp(text, "vlan=", 5) == 0 && number(text + 5, 4095, &value) == 0) {
        bytes_field(rule, 12, (const uint8_t[]){0x81, 0x00, (uint8_t)(value >> 8), (uint8_t)value}, (const uint8_t[]){0xFF, 0xFF, 0x0F, 0xFF}, 4);
    } else if (equal && text[0] >= '0' && text[0] <= '9') {
        uint8_t bytes[RULES_BYTES], mask[RULES_BYTES];
        char offset[4] = {0};
        const char *slash = strchr(equal, '/');
        size_t digits = slash ? (size_t)(slash - equal - 1) : strlen(equal + 1);
        if ((size_t)(equal - text) >= sizeof(offset))
            return -1;
        memcpy(offset, text, (size_t)(equal - text));
        if (number(offset, RULES_BYTES - 1, &value) < 0)
            return -1;
        int count = hex_bytes(equal + 1, digits, bytes);
        memset(mask, 0xFF, sizeof(mask));
        if (count < 0 || value + (unsigned)count > RULES_BYTES || (slash && hex_bytes(slash + 1, strlen(slash + 1), mask) != count))
            return -1;
        bytes_field(rule, (unsigned)value, bytes, mask, (unsigned)count);
    } else {
        return -1;
    }
    return 0;
}

int rules_add(struct rules *r, const char *text)
{
    char copy[256], *rest = copy;
    if (r->count == RULES_MAX || strlen(text) >= sizeof(copy)) {
        fprintf(stderr, "sniffer: rule '%s': %s\n", text, r->count == RULES_MAX ? "all rule slots are taken" : "too long");
        return -1;
    }
    strcpy(copy, text);
    struct rule *rule = &r->list[r->count];
    *rule = (struct rule){.ds = 1, .us = 1, .port = -1};
    if (rules_action(strsep(&rest, ","), &rule->action) < 0)
        return -1;
    for (const char *token; (token = strsep(&rest, ",")) != NULL;)
        if (field(rule, token) < 0) {
            fprintf(stderr, "sniffer: rule '%s': field '%s' is not one of\n%s", text, token, RULES_SYNTAX);
            return -1;
        }
    r->count++;
    return 0;
}

// The last compared byte's number + 1: a shorter first fragment cannot match.
static unsigned need(const struct rule *rule)
{
    unsigned bytes = RULES_BYTES;
    while (bytes && !rule->mask[bytes - 1])
        bytes--;
    return bytes;
}

uint32_t rules_filter(const struct rules *r, int us_fec)
{
    return r->other << INLINE_FILTER_ACTION_SHIFT | r->snap << INLINE_FILTER_SNAP_SHIFT | (us_fec ? INLINE_FILTER_USFEC_MASK : 0);
}

int rules_write(const struct rules *r, void *ctx, int (*write)(void *ctx, unsigned offset, uint32_t value))
{
    for (unsigned i = 0; i < RULES_MAX; i++) {
        const struct rule *rule = &r->list[i];
        uint32_t select = i << INLINE_RULE_SELECT_RULE_SHIFT, config = 0;
        if (i < r->count) {
            for (unsigned w = 0; w < RULES_BYTES / 4; w++) {
                uint32_t value = 0, mask = 0;
                for (unsigned b = 0; b < 4; b++) {
                    value = value << 8 | rule->value[4 * w + b];
                    mask = mask << 8 | rule->mask[4 * w + b];
                }
                if (write(ctx, INLINE_RULE_SELECT, select | w) < 0 || write(ctx, INLINE_RULE_VALUE, value) < 0 ||
                    write(ctx, INLINE_RULE_MASK, mask) < 0)
                    return -1;
            }
            config = (rule->ds ? INLINE_RULE_DS_MASK : 0) | (rule->us ? INLINE_RULE_US_MASK : 0) |
                     (rule->port >= 0 ? INLINE_RULE_ONPORT_MASK | (uint32_t)rule->port << INLINE_RULE_PORT_SHIFT : 0) |
                     rule->action << INLINE_RULE_ACTION_SHIFT | need(rule) << INLINE_RULE_NEED_SHIFT;
        }
        if (write(ctx, INLINE_RULE_SELECT, select) < 0 || write(ctx, INLINE_RULE, config) < 0)
            return -1;
    }
    return write(ctx, INLINE_FILTER, rules_filter(r, 0));
}

int rules_keep_port(unsigned port, void *ctx, int (*write)(void *ctx, unsigned offset, uint32_t value))
{
    const uint32_t config = INLINE_RULE_DS_MASK | INLINE_RULE_US_MASK | INLINE_RULE_ONPORT_MASK | port << INLINE_RULE_PORT_SHIFT |
                            RULES_KEEP << INLINE_RULE_ACTION_SHIFT;
    return write(ctx, INLINE_RULE_SELECT, 0) < 0 ? -1 : write(ctx, INLINE_RULE, config);
}
