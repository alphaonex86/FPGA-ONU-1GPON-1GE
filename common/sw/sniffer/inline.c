// SPDX-License-Identifier: CERN-OHL-S-2.0
// In-line sniffer over its USB serial port (CDC-ACM): the record stream in, the registers written and
// read by 6-byte commands (the protocol: InlineDump's header, common/hw/spinal/pon/sniffer/InlineSniffer.scala).
// When its SuperSpeed link has a host (register LINK) the records come there (wide.h), the replies stay here.
#include "inline.h"
#include "levels.h"
#include "wide.h"

#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <string.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

typedef uint32_t u32;
#include "inline_regs.h"
#include "sniffer_regs.h"

// the decoder writes them by the passive tap's offsets (FilterRegs)
_Static_assert(INLINE_CTRL == SNIFFER_CTRL && INLINE_DELIMITER == SNIFFER_DELIMITER &&
               INLINE_ALLOC_MAP == SNIFFER_ALLOC_MAP && INLINE_PORT_MAP == SNIFFER_PORT_MAP, "filter registers differ");

#define MAGIC 0x47530001u
#define WRITE 0x80u               // command number bit
#define REPLY_MS 1000
#define REPORT_MS 1000
#define QUIET_MS 100              // over the command parser's timeout (2^22 clocks: 54 ms)
#define STALE_BYTES (1u << 20)
#define DUTY_MARGIN 0x0CCDu       // 5 %: the VCXO control is at a rail

struct stream {
    uint8_t bytes[1 << 17]; // received, parsed up to `at`
    size_t at, have, skipped;
};

struct link {
    int fd;
    struct sniffer *s;
    struct wide *wide;      // the SuperSpeed side, or NULL
    int started;            // the decoder takes the records
    int failed;             // a command was not sent, or the decoder failed on a SuperSpeed record (logged)
    int stale;              // SuperSpeed bytes are a previous run's: counted in stale_bytes, not parsed
    size_t stale_bytes;
    struct stream serial, usb;
    uint32_t state, counters[4]; // as last reported
};

static int open_port(const char *path)
{
    struct termios raw;
    // not blocking: a serial port waits for its carrier otherwise
    int fd = open(path, O_RDWR | O_NOCTTY | O_NONBLOCK);
    if (fd >= 0 && tcgetattr(fd, &raw) == 0) {
        cfmakeraw(&raw);
        raw.c_cflag = (raw.c_cflag & ~(tcflag_t)CRTSCTS) | CLOCAL | CREAD;
        raw.c_cc[VMIN] = 1;
        raw.c_cc[VTIME] = 0;
        int flags = fcntl(fd, F_GETFL);
        if (tcsetattr(fd, TCSANOW, &raw) == 0 && flags >= 0 && fcntl(fd, F_SETFL, flags & ~O_NONBLOCK) == 0)
            return fd;
    }
    fprintf(stderr, "sniffer: %s: %s\n", path, strerror(errno));
    if (fd >= 0 && close(fd) < 0)
        perror("sniffer: close");
    return -1;
}

static int send_bytes(struct link *l, const uint8_t *bytes, size_t size)
{
    while (size) {
        ssize_t sent = write(l->fd, bytes, size);
        if (sent < 0 && errno == EINTR)
            continue;
        if (sent < 0) {
            perror("sniffer: link write");
            l->failed = 1;
            return -1;
        }
        bytes += sent;
        size -= (size_t)sent;
    }
    return 0;
}

static int command(struct link *l, unsigned number, uint32_t value)
{
    const uint8_t bytes[6] = {0x5A, (uint8_t)number, (uint8_t)value, (uint8_t)(value >> 8), (uint8_t)(value >> 16), (uint8_t)(value >> 24)};
    return send_bytes(l, bytes, sizeof(bytes));
}

static void link_write(void *ctx, unsigned offset, uint32_t value)
{
    if (offset <= INLINE_PORT_MAP) // the others are the passive tap's burst reset
        command(ctx, WRITE | offset / 4, value);
}

static int now_ms(int64_t *ms)
{
    struct timespec now;
    if (clock_gettime(CLOCK_MONOTONIC, &now) < 0) {
        perror("sniffer: clock");
        return -1;
    }
    *ms = (int64_t)now.tv_sec * 1000 + now.tv_nsec / 1000000;
    return 0;
}

// The stream's whole records go to the decoder. Returns 1 at the reply of register number `wanted` (its value in
// *value), 0 when the rest is no whole record yet, -1 on an error (logged).
static int parse(struct link *l, struct stream *in, int wanted, uint32_t *value)
{
    while (in->at < in->have) {
        struct sniffer_record r;
        int used = sniffer_parse(in->bytes + in->at, in->have - in->at, &r);
        if (used == 0)
            break;
        if (used < 0) {
            in->at++;
            in->skipped++;
            continue;
        }
        if (in->skipped) {
            fprintf(stderr, "sniffer: %zu link bytes that are no record, skipped\n", in->skipped);
            in->skipped = 0;
        }
        in->at += (size_t)used;
        if (r.kind != SNIFFER_REPLY) {
            if (l->started && sniffer_handle(l->s, &r) < 0)
                return -1;
        } else if (r.length == 5 && r.payload[0] == wanted) {
            *value = r.payload[1] | (uint32_t)r.payload[2] << 8 | (uint32_t)r.payload[3] << 16 | (uint32_t)r.payload[4] << 24;
            return 1;
        } else {
            fprintf(stderr, "sniffer: a reply not asked for (%u bytes)\n", r.length);
        }
    }
    memmove(in->bytes, in->bytes + in->at, in->have - in->at);
    in->have -= in->at;
    in->at = 0;
    return 0;
}

// wide.h's sink
static void wide_bytes(void *ctx, const uint8_t *bytes, size_t size)
{
    struct link *l = ctx;
    struct stream *in = &l->usb;
    if (l->stale) {
        l->stale_bytes += size;
        return;
    }
    while (size && !l->failed) {
        size_t part = sizeof(in->bytes) - in->have < size ? sizeof(in->bytes) - in->have : size;
        memcpy(in->bytes + in->have, bytes, part);
        in->have += part;
        bytes += part;
        size -= part;
        if (parse(l, in, -1, NULL) < 0)
            l->failed = 1;
    }
}

// The records received go to the decoder. Returns 1 at the reply of register number `wanted` (its value in *value),
// 0 after `ms` or, when no reply is wanted, at a signal; -1 on an error (logged).
static int pump(struct link *l, int wanted, uint32_t *value, int ms)
{
    int64_t now, end;
    if (now_ms(&end) < 0)
        return -1;
    end += ms;
    for (;;) {
        int found = parse(l, &l->serial, wanted, value);
        if (found || l->failed)
            return l->failed ? -1 : found;
        if (now_ms(&now) < 0)
            return -1;
        if (now >= end)
            return 0;
        int ready = wide_poll(l->wide, l->fd, (int)(end - now));
        if (ready == 0)
            continue;
        ssize_t got = ready > 0 ? read(l->fd, l->serial.bytes + l->serial.have, sizeof(l->serial.bytes) - l->serial.have) : ready;
        if (got < 0 && errno == EINTR) {
            if (wanted < 0)
                return 0;
            continue;
        }
        if (got <= 0) {
            fprintf(stderr, "sniffer: link: %s\n", got ? strerror(errno) : "closed");
            return -1;
        }
        l->serial.have += (size_t)got;
    }
}

static int reg_write(void *ctx, unsigned offset, uint32_t value) { return command(ctx, WRITE | offset / 4, value); }
static int reg_ask(void *ctx, unsigned offset) { return command(ctx, offset / 4, 0); }

static int reg_answer(void *ctx, unsigned offset, uint32_t *value)
{
    int got = pump(ctx, (int)(offset / 4), value, REPLY_MS);
    if (got == 0)
        fprintf(stderr, "sniffer: register 0x%02x not answered\n", offset);
    return got == 1 ? 0 : -1;
}

static int link_read(struct link *l, unsigned offset, uint32_t *value)
{
    return reg_ask(l, offset) < 0 ? -1 : reg_answer(l, offset, value);
}

// What a previous run left on its way, until the link is quiet.
static int drain(int fd)
{
    static uint8_t stale[4096];
    for (size_t total = 0; total < STALE_BYTES;) {
        struct pollfd wait = {fd, POLLIN, 0};
        int ready = poll(&wait, 1, QUIET_MS);
        if (ready == 0)
            return 0;
        ssize_t got = ready > 0 ? read(fd, stale, sizeof(stale)) : ready;
        if (got < 0 && errno == EINTR)
            continue;
        if (got <= 0) {
            fprintf(stderr, "sniffer: link: %s\n", got ? strerror(errno) : "closed");
            return -1;
        }
        total += (size_t)got;
    }
    fprintf(stderr, "sniffer: the port keeps sending with the records disabled: not an in-line sniffer?\n");
    return -1;
}

// The SuperSpeed side when it has a host: emptied of a previous run's bytes, then given the records.
static int start_wide(struct link *l)
{
    uint32_t state;
    int64_t now, end;
    if (link_read(l, INLINE_LINK, &state) < 0)
        return -1;
    if (!(state & INLINE_LINK_UP_MASK))
        return 0;
    l->wide = wide_open(wide_bytes, l);
    if (!l->wide) {
        fprintf(stderr, "sniffer: the SuperSpeed link is up but not usable: records on the serial port\n");
        return 0;
    }
    l->stale = 1;
    for (size_t seen = 1; seen != l->stale_bytes;) {
        seen = l->stale_bytes;
        if (seen >= STALE_BYTES) {
            fprintf(stderr, "sniffer: the SuperSpeed link keeps sending with the records disabled\n");
            return -1;
        }
        if (now_ms(&end) < 0)
            return -1;
        for (now = end, end += QUIET_MS; now < end;)
            if ((wide_poll(l->wide, -1, (int)(end - now)) < 0 && errno != EINTR) || now_ms(&now) < 0)
                return -1;
    }
    l->stale = 0;
    fprintf(stderr, "sniffer: records on the SuperSpeed link\n");
    return command(l, WRITE | INLINE_LINK / 4, INLINE_LINK_WIDE_MASK);
}

static int start(struct link *l, const struct sniffer_options *opt)
{
    // zeros end a command a killed run left half sent, without starting one
    static const uint8_t resync[5] = {0};
    uint32_t id;
    if (send_bytes(l, resync, sizeof(resync)) < 0 || command(l, WRITE | INLINE_CTRL / 4, 0) < 0 ||
        command(l, WRITE | INLINE_LINK / 4, 0) < 0 || drain(l->fd) < 0 || link_read(l, INLINE_ID, &id) < 0)
        return -1;
    if (id != MAGIC) {
        fprintf(stderr, "sniffer: identity %08x, an in-line sniffer's is %08x\n", id, MAGIC);
        return -1;
    }
    if (start_wide(l) < 0)
        return -1;
    const struct sniffer_regs regs = {l, NULL, link_write};
    if (sniffer_start(l->s, &regs, opt) < 0)
        return -1;
    l->started = 1;
    return 0;
}

// Prints the line's state and the error and lost counters when they change.
static int report(struct link *l)
{
    static const struct { unsigned offset; const char *name; } counters[4] = {
        {INLINE_ERRORS, "downstream errors (hec | plend << 16 | sync losses << 24)"},
        {INLINE_DS_LOST, "DS lost (drops | truncations << 16)"},
        {INLINE_US_LOST, "US lost (drops | truncations << 16)"},
        {INLINE_US_OVERFLOWS, "US bursts left out or cut"},
    };
    uint32_t value;
    if (link_read(l, INLINE_STATUS, &value) < 0)
        return -1;
    unsigned duty = value >> INLINE_STATUS_DUTY_SHIFT;
    int railed = duty < DUTY_MARGIN || duty > 0xFFFF - DUTY_MARGIN;
    uint32_t state = (value & ~INLINE_STATUS_DUTY_MASK) | (uint32_t)railed << INLINE_STATUS_DUTY_SHIFT;
    if (state != l->state) {
        l->state = state;
        fprintf(stderr, "sniffer: OLT port %s, downstream %s%s, %s to the ONU, VCXO control %u %%%s\n",
                value & INLINE_STATUS_DSSIGNAL_MASK ? "lit" : "dark", value & INLINE_STATUS_SYNCED_MASK ? "synced" : "not synced",
                value & INLINE_STATUS_FEC_MASK ? " with FEC" : "", value & INLINE_STATUS_DSON_MASK ? "sent" : "not sent",
                duty * 100 / 65536, railed ? " (at its limit)" : "");
    }
    for (int i = 0; i < 4; i++) {
        if (link_read(l, counters[i].offset, &value) < 0)
            return -1;
        if (value != l->counters[i]) {
            l->counters[i] = value;
            fprintf(stderr, "sniffer: %s %08x\n", counters[i].name, value);
        }
    }
    return 0;
}

int inline_run(struct sniffer *s, const char *port, const char *level_file, const struct sniffer_options *opt,
               const volatile sig_atomic_t *stopping)
{
    static struct link l;
    static struct levels lv;
    const struct levels_link regs = {&l, reg_write, reg_ask, reg_answer};
    if (level_file && levels_load(&lv, level_file) < 0)
        return -1;
    memset(&l, 0, sizeof(l));
    l.s = s;
    l.state = ~0u;
    l.fd = open_port(port);
    if (l.fd < 0)
        return -1;
    int result = start(&l, opt);
    if (!result && level_file)
        result = levels_start(&lv, &regs);
    while (!result && !*stopping)
        result = report(&l) < 0 || (level_file && levels_step(&lv) < 0) ? -1 : pump(&l, -1, NULL, REPORT_MS);
    if (l.started) {
        sniffer_stop(s);
        if (!result) // the records on their way, then the last counts
            result = report(&l);
    }
    if (l.wide) {
        if (command(&l, WRITE | INLINE_LINK / 4, 0) < 0)
            result = -1;
        wide_close(l.wide);
    }
    if (close(l.fd) < 0) {
        perror("sniffer: close");
        result = -1;
    }
    return result;
}
