// SPDX-License-Identifier: CERN-OHL-S-2.0
// Host test of the in-line link: a pseudo terminal whose master side is the device (InlineDump's commands, replies
// and padded records).
#define _GNU_SOURCE
#include "inline.h"

#include <fcntl.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

typedef uint32_t u32;
#include "inline_regs.h"

struct device {
    int fd;
    uint32_t regs[64];
    uint8_t alloc_keep[4096], port_keep[4096];
    unsigned reads, strays; // strays: writes to a register the decoder must leave alone
    int enabled, failed;
};

static volatile sig_atomic_t stopping;
static int failures;

#define CHECK(cond)                                                       \
    do {                                                                  \
        if (!(cond)) {                                                    \
            fprintf(stderr, "%s:%d: CHECK(%s) failed\n", __FILE__, __LINE__, #cond); \
            failures++;                                                   \
        }                                                                 \
    } while (0)

static void record(struct device *d, unsigned kind, uint64_t time, const uint8_t *payload, unsigned length)
{
    uint8_t bytes[12 + 64 + 3] = {(uint8_t)(0xA0 | kind), 0, (uint8_t)length, 0};
    for (int i = 0; i < 8; i++)
        bytes[4 + i] = (uint8_t)(time >> (8 * i));
    memcpy(bytes + 12, payload, length);
    size_t total = (12 + length + 3) & ~3u;
    if (write(d->fd, bytes, total) != (ssize_t)total)
        d->failed = 1;
}

static void execute(struct device *d, unsigned number, uint32_t value)
{
    unsigned reg = number & 0x7F;
    if (!(number & 0x80)) {
        static const uint32_t fixed[64] = {
            [INLINE_ID / 4] = 0x47530001,
            [INLINE_STATUS / 4] = INLINE_STATUS_SYNCED_MASK | INLINE_STATUS_DSSIGNAL_MASK | 0x8000u << INLINE_STATUS_DUTY_SHIFT,
            [INLINE_DS_LOST / 4] = 0x000D000A, // bytes a terminal that is not raw would change
        };
        value = fixed[reg % 64] ? fixed[reg % 64] : d->regs[reg % 64];
        const uint8_t reply[5] = {(uint8_t)reg, (uint8_t)value, (uint8_t)(value >> 8), (uint8_t)(value >> 16), (uint8_t)(value >> 24)};
        record(d, SNIFFER_REPLY, 0, reply, sizeof(reply));
        d->reads++;
        return;
    }
    if (reg == INLINE_ALLOC_MAP / 4)
        d->alloc_keep[value & 0xFFF] = value >> 12 & 1;
    else if (reg == INLINE_PORT_MAP / 4)
        d->port_keep[value & 0xFFF] = value >> 12 & 1;
    else if (reg == INLINE_CTRL / 4 || reg == INLINE_DELIMITER / 4)
        d->regs[reg] = value;
    else
        d->strays++;
    if (reg != INLINE_CTRL / 4)
        return;
    if ((value & INLINE_CTRL_ENABLE_MASK) && !d->enabled) {
        // an Upstream_Overhead with its delimiter, then a burst of ONU 5 (18 bytes: padded)
        const uint8_t ploam[12] = {0xFF, 0x01, 4, 0, 0, 0, 0x12, 0x34, 0x56};
        const uint8_t burst[18] = {0x5A, 5};
        record(d, SNIFFER_PLOAM, 10, ploam, sizeof(ploam));
        record(d, SNIFFER_BURST, 30, burst, sizeof(burst));
    }
    d->enabled = value & INLINE_CTRL_ENABLE_MASK;
    if ((value & INLINE_CTRL_ONUID_MASK) >> INLINE_CTRL_ONUID_SHIFT == 5)
        stopping = 1; // the decoder has taken both records
}

static void *device(void *ctx)
{
    struct device *d = ctx;
    // a previous run's cut record, sent once the port is open and raw
    static const uint8_t stale[24] = {0xA2, 0, 64, 0, 1, 2, 3, 4, 5, 6, 7, 8, 0x0D, 0x0A, 0x11, 0x13};
    uint8_t bytes[256], shift[5];
    unsigned index = 0;
    int first = 1;
    ssize_t got;
    while ((got = read(d->fd, bytes, sizeof(bytes))) > 0) {
        if (first && write(d->fd, stale, sizeof(stale)) != sizeof(stale))
            d->failed = 1;
        first = 0;
        for (ssize_t i = 0; i < got; i++) {
            if (index == 0) {
                index = bytes[i] == 0x5A;
                continue;
            }
            shift[index - 1] = bytes[i];
            if (++index == 6) {
                index = 0;
                execute(d, shift[0], shift[1] | (uint32_t)shift[2] << 8 | (uint32_t)shift[3] << 16 | (uint32_t)shift[4] << 24);
            }
        }
    }
    return NULL; // the port closed
}

int main(void)
{
    static struct device d;
    static struct sniffer s;
    char *text = NULL, *dump = NULL;
    size_t text_size = 0, dump_size = 0;
    // Alloc-ID 10: a line feed in its command
    struct sniffer_options opt = {.onu_id = -1, .delimiter = 0xAB5983, .allocs = {6, 10}, .alloc_count = 2};
    opt.text = open_memstream(&text, &text_size);
    opt.dump = open_memstream(&dump, &dump_size);
    d.fd = posix_openpt(O_RDWR | O_NOCTTY);
    const char *port = d.fd >= 0 && grantpt(d.fd) == 0 && unlockpt(d.fd) == 0 ? ptsname(d.fd) : NULL;
    pthread_t thread;
    if (!opt.text || !opt.dump || !port || pthread_create(&thread, NULL, device, &d) != 0) {
        perror("test_inline: setup");
        return 1;
    }
    d.alloc_keep[77] = d.port_keep[77] = 1; // a previous run's
    d.regs[INLINE_CTRL / 4] = INLINE_CTRL_ENABLE_MASK;

    CHECK(inline_run(&s, port, &opt, &stopping) == 0);
    CHECK(pthread_join(thread, NULL) == 0);
    CHECK(fclose(opt.text) == 0 && fclose(opt.dump) == 0);
    CHECK(!d.failed && d.strays == 0);
    CHECK(d.reads >= 1 + 2 * 5); // the identity, two reports
    CHECK(!d.alloc_keep[77] && !d.port_keep[77] && d.alloc_keep[6] && d.alloc_keep[10]);
    CHECK(d.regs[INLINE_DELIMITER / 4] == 0x123456); // from the Upstream_Overhead
    CHECK(s.onu_id == 5 && d.alloc_keep[5]);         // from the burst
    CHECK(d.regs[INLINE_CTRL / 4] == 0);             // stopped
    CHECK(strstr(text, "Upstream_Overhead") && strstr(text, "US BURST onu=5"));
    CHECK(dump_size == 24 + 32);
    free(text);
    free(dump);
    if (failures)
        fprintf(stderr, "%d check(s) failed\n", failures);
    else
        printf("test_inline: ok\n");
    return failures != 0;
}
