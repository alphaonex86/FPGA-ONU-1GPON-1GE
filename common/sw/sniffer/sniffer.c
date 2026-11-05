// SPDX-License-Identifier: CERN-OHL-S-2.0
#include "sniffer.h"

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

typedef uint32_t u32;
#include "sniffer_regs.h"

#define PLOU_CLOCKS 6           // BIP, ONU-ID, Ind: a burst record's time to its first StartTime byte
#define MIN_OFFSET 10000        // an ONU answers its grants after ~35 us (G.984.3 response time)
#define MAX_OFFSET (4 * SNIFFER_FRAME_CLOCKS)
#define LENGTH_SLACK 4          // burst bytes: trailing zeros look like the squelch
#define VOTE_SLACK 8            // clocks
#define DEFAULT_DELIMITER_ERRORS 1

static const char *const ploam_names[] = {
    NULL, "Upstream_Overhead", "Serial_Number_Mask", "Assign_ONU-ID", "Ranging_Time", "Deactivate_ONU-ID",
    "Disable_Serial_Number", "Configure_VP/VC", "Encrypted_Port-ID", "Request_Password", "Assign_Alloc-ID",
    "No_Message", "POPUP", "Request_Key", "Configure_Port-ID", "PEE", "Change_Power_Level", "PST", "BER_Interval",
    "Key_Switching_Time", "Extended_Burst_Length", "PON-ID", "Swift_POPUP", "Ranging_Adjustment", "Sleep_Allow",
};

static const char *const omci_names[32] = {
    [4] = "Create", [6] = "Delete", [8] = "Set", [9] = "Get", [11] = "Get_all_alarms", [12] = "Get_all_alarms_next",
    [13] = "MIB_upload", [14] = "MIB_upload_next", [15] = "MIB_reset", [16] = "Alarm", [17] = "AVC", [18] = "Test",
    [19] = "Start_SW_download", [20] = "Download_section", [21] = "End_SW_download", [22] = "Activate_SW",
    [23] = "Commit_SW", [24] = "Synchronize_time", [25] = "Reboot", [26] = "Get_next", [27] = "Test_result",
    [28] = "Get_current_data", [29] = "Set_table",
};

static unsigned be16(const uint8_t *p) { return (unsigned)p[0] << 8 | p[1]; }
static uint32_t be32(const uint8_t *p) { return (uint32_t)p[0] << 24 | (uint32_t)p[1] << 16 | (uint32_t)p[2] << 8 | p[3]; }

static void reg_write(struct sniffer *s, unsigned offset, uint32_t value)
{
    if (s->regs.write)
        s->regs.write(s->regs.ctx, offset, value);
}

static void say(struct sniffer *s, const char *format, ...) __attribute__((format(printf, 2, 3)));
static void say(struct sniffer *s, const char *format, ...)
{
    if (!s->opt.text)
        return;
    va_list args;
    va_start(args, format);
    vfprintf(s->opt.text, format, args);
    va_end(args);
}

int sniffer_parse(const uint8_t *bytes, size_t size, struct sniffer_record *record)
{
    if (size < 4)
        return 0;
    if ((bytes[0] & 0xF8) != 0xA0 || (bytes[0] & 7) > SNIFFER_BURST)
        return -1;
    unsigned length = bytes[2] | (unsigned)bytes[3] << 8;
    size_t total = (12 + length + 3) & ~(size_t)3;
    if (size < total)
        return 0;
    record->kind = bytes[0] & 7;
    record->flags = bytes[1];
    record->length = length;
    record->time = 0;
    for (int i = 7; i >= 0; i--)
        record->time = record->time << 8 | bytes[4 + i];
    record->payload = bytes + 12;
    return (int)total;
}

static void keep(struct sniffer *s, unsigned map, unsigned id, int kept)
{
    reg_write(s, map, (id & SNIFFER_ALLOC_MAP_ID_MASK) | (kept ? SNIFFER_ALLOC_MAP_KEEP_MASK : 0));
}

static void write_ctrl(struct sniffer *s)
{
    reg_write(s, SNIFFER_CTRL, s->ctrl);
}

static void set_onu(struct sniffer *s, unsigned id)
{
    s->onu_id = (int)id;
    s->ctrl = (s->ctrl & ~SNIFFER_CTRL_ONUID_MASK) | id << SNIFFER_CTRL_ONUID_SHIFT;
    write_ctrl(s);
    keep(s, SNIFFER_ALLOC_MAP, id, 1); // the default Alloc-ID is the ONU-ID
    s->vote_count = 0;
    say(s, "# ONU-ID %u: its PLOAMs and default Alloc-ID kept\n", id);
}

static void add_omci_port(struct sniffer *s, unsigned port)
{
    for (unsigned i = 0; i < s->omci_port_count; i++)
        if (s->omci_ports[i] == port)
            return;
    if (s->omci_port_count == SNIFFER_MAX_IDS) {
        fprintf(stderr, "sniffer: more than %u OMCI Port-IDs, %u not decoded\n", SNIFFER_MAX_IDS, port);
        return;
    }
    s->omci_ports[s->omci_port_count++] = (uint16_t)port;
}

int sniffer_start(struct sniffer *s, const struct sniffer_regs *regs, const struct sniffer_options *opt)
{
    memset(s, 0, sizeof(*s));
    s->regs = *regs;
    s->opt = *opt;
    s->onu_id = -1;
    s->synced = -1;
    s->offset = -1;
    reg_write(s, SNIFFER_CTRL, 0);
    for (unsigned id = 0; id < 4096; id++) {
        keep(s, SNIFFER_ALLOC_MAP, id, 0);
        keep(s, SNIFFER_PORT_MAP, id, 0);
    }
    for (unsigned i = 0; i < opt->alloc_count; i++)
        keep(s, SNIFFER_ALLOC_MAP, opt->allocs[i], 1);
    for (unsigned i = 0; i < opt->port_count; i++) {
        keep(s, SNIFFER_PORT_MAP, opt->ports[i], 1);
        add_omci_port(s, opt->ports[i]);
    }
    reg_write(s, SNIFFER_DELIMITER, opt->delimiter & SNIFFER_DELIMITER_DELIMITER_MASK);
    reg_write(s, SNIFFER_RESET_TIMING, 0); // no pulse until the offset is learned
    s->ctrl = SNIFFER_CTRL_ENABLE_MASK | (opt->all_ploams ? SNIFFER_CTRL_ALLPLOAMS_MASK : 0) |
              0xFFu << SNIFFER_CTRL_ONUID_SHIFT | DEFAULT_DELIMITER_ERRORS << SNIFFER_CTRL_DELIMITERERRORS_SHIFT;
    if (opt->onu_id >= 0)
        set_onu(s, (unsigned)opt->onu_id);
    else
        write_ctrl(s);
    return 0;
}

void sniffer_stop(struct sniffer *s)
{
    reg_write(s, SNIFFER_CTRL, 0);
    if (s->opt.dump && fflush(s->opt.dump) != 0)
        perror("sniffer: dump");
    if (s->opt.text)
        fflush(s->opt.text);
}

static unsigned burst_bytes(const struct sniffer_entry *e)
{
    unsigned bytes = 3 + ((e->stop - e->start + 1) & 0xFFFF);
    return e->fec ? bytes + 16 * ((bytes + 238) / 239) : bytes;
}

static int compare_votes(const void *a, const void *b)
{
    int64_t x = *(const int64_t *)a, y = *(const int64_t *)b;
    return (x > y) - (x < y);
}

// Settles when 3/4 of the last votes agree; the pulses start with the first offset.
static void vote(struct sniffer *s, int64_t offset)
{
    s->votes[s->vote_next] = offset;
    s->vote_next = (s->vote_next + 1) % SNIFFER_VOTES;
    if (s->vote_count < SNIFFER_VOTES) {
        s->vote_count++;
        return;
    }
    int64_t sorted[SNIFFER_VOTES];
    memcpy(sorted, s->votes, sizeof(sorted));
    qsort(sorted, SNIFFER_VOTES, sizeof(sorted[0]), compare_votes);
    int64_t median = sorted[SNIFFER_VOTES / 2];
    unsigned agree = 0;
    for (unsigned i = 0; i < SNIFFER_VOTES; i++)
        agree += llabs(sorted[i] - median) <= VOTE_SLACK;
    if (agree < SNIFFER_VOTES * 3 / 4 || (s->offset >= 0 && llabs(median - s->offset) <= VOTE_SLACK))
        return;
    int first = s->offset < 0;
    s->offset = median;
    reg_write(s, SNIFFER_RESET_OFFSET, (uint32_t)median);
    if (first)
        reg_write(s, SNIFFER_RESET_TIMING, (s->opt.lead & 0xFFFF) | (s->opt.width & 0xFFFF) << 16);
    say(s, "# RESET_OFFSET %lld clocks (%.3f us)\n", (long long)median, median / SNIFFER_CLOCK_MHZ);
}

// A burst starts at an entry not contiguous with its frame's previous one (as BurstReset).
static void learn_entry(struct sniffer *s, uint64_t time, const uint8_t *p)
{
    unsigned flags = (p[1] & 0xF) << 8 | p[2], start = be16(p + 3), stop = be16(p + 5);
    int contiguous = s->entry_count && time - s->previous_time < SNIFFER_FRAME_CLOCKS / 2 &&
                     start == ((s->previous_stop + 1) & 0xFFFF);
    if (contiguous) {
        s->entries[(s->entry_next + SNIFFER_ENTRIES - 1) % SNIFFER_ENTRIES].stop = stop;
    } else {
        s->entries[s->entry_next] = (struct sniffer_entry){time, start, stop, flags >> 9 & 1};
        s->entry_next = (s->entry_next + 1) % SNIFFER_ENTRIES;
        if (s->entry_count < SNIFFER_ENTRIES)
            s->entry_count++;
    }
    s->previous_time = time;
    s->previous_stop = stop;
}

// The burst's entry: the one whose grant has the burst's length, else (equal grants) the latest, both possible.
static void learn_burst(struct sniffer *s, const struct sniffer_record *r)
{
    int64_t best = -1;
    int best_matched = 0;
    for (unsigned i = 0; i < s->entry_count; i++) {
        const struct sniffer_entry *e = &s->entries[i];
        if (r->time <= e->time)
            continue;
        int64_t offset = (int64_t)(r->time - e->time) + PLOU_CLOCKS - 2 * (int64_t)e->start;
        if (offset < MIN_OFFSET || offset > MAX_OFFSET)
            continue;
        int matched = abs((int)burst_bytes(e) - (int)r->length) <= LENGTH_SLACK;
        if (matched > best_matched || (matched == best_matched && (best < 0 || offset < best))) {
            best = offset;
            best_matched = matched;
        }
    }
    if (best >= 0)
        vote(s, best);
}

static void handle_ploam(struct sniffer *s, const struct sniffer_record *r)
{
    const uint8_t *p = r->payload;
    unsigned onu = p[0], msg = p[1];
    int bad = r->flags & 1;
    say(s, " onu=%u msg=0x%02x %s", onu, msg, msg < sizeof(ploam_names) / sizeof(ploam_names[0]) && ploam_names[msg] ? ploam_names[msg] : "?");
    for (int i = 2; i < 12; i++)
        say(s, "%s%02x", i == 2 ? " " : "", p[i]);
    if (bad) {
        say(s, " bad-crc\n");
        return;
    }
    int own = s->onu_id >= 0 && onu == (unsigned)s->onu_id;
    switch (msg) {
    case 0x01: {
        uint32_t delimiter = (uint32_t)p[6] << 16 | p[7] << 8 | p[8];
        say(s, " delimiter=%06x", delimiter);
        if (delimiter != s->opt.delimiter) {
            s->opt.delimiter = delimiter;
            reg_write(s, SNIFFER_DELIMITER, delimiter);
        }
        break;
    }
    case 0x03:
        say(s, " onu-id=%u sn=%.4s%08x", p[2], (const char *)p + 3, be32(p + 7));
        if (s->opt.serial && !memcmp(p + 3, s->opt.serial, 8) && (int)p[2] != s->onu_id) {
            say(s, "\n");
            set_onu(s, p[2]);
            return;
        }
        break;
    case 0x04:
        say(s, " eqd=%u%s", be32(p + 3), p[2] & 1 ? " secondary" : "");
        if (own) // the bursts move: learn again
            s->vote_count = 0;
        break;
    case 0x0A: {
        unsigned id = p[2] << 4 | p[3] >> 4;
        say(s, " alloc-id=%u type=%u", id, p[4]);
        if (own)
            keep(s, SNIFFER_ALLOC_MAP, id, p[4] != 0xFF);
        break;
    }
    case 0x0E: {
        unsigned port = p[3] << 4 | p[4] >> 4;
        say(s, " port-id=%u %s", port, p[2] & 1 ? "on" : "off");
        if (own) {
            keep(s, SNIFFER_PORT_MAP, port, p[2] & 1);
            if (p[2] & 1)
                add_omci_port(s, port);
        }
        break;
    }
    }
    say(s, "\n");
}

static void handle_gem(struct sniffer *s, const struct sniffer_record *r)
{
    const uint8_t *p = r->payload;
    unsigned port = be16(p) & 0xFFF, pli = be16(p + 2) & 0xFFF;
    uint64_t counter = 0;
    for (int i = 4; i < 10; i++)
        counter = counter << 8 | p[i];
    say(s, " port=%u pli=%u pti=%u counter=%llx%s", port, pli, r->flags & 7, (unsigned long long)counter, r->flags & 8 ? " cut" : "");
    const uint8_t *data = p + 10;
    unsigned bytes = r->length - 10;
    int omci = 0;
    for (unsigned i = 0; i < s->omci_port_count; i++)
        omci |= s->omci_ports[i] == port;
    if (omci && bytes >= 8) {
        unsigned type = data[2] & 0x1F;
        say(s, " omci tci=%04x %s%s%s me=%u/%u", be16(data), omci_names[type] ? omci_names[type] : "?",
            data[2] & 0x40 ? " ar" : "", data[2] & 0x20 ? " ak" : "", be16(data + 4), be16(data + 6));
    }
    say(s, " ");
    for (unsigned i = 0; i < bytes; i++)
        say(s, "%02x", data[i]);
    say(s, "\n");
}

int sniffer_handle(struct sniffer *s, const struct sniffer_record *r)
{
    static const unsigned min_length[] = {12, 7, 10, 3};
    size_t total = (12 + r->length + 3) & ~(size_t)3;
    if (s->opt.dump && fwrite(r->payload - 12, 1, total, s->opt.dump) != total) {
        perror("sniffer: dump");
        return -1;
    }
    static const char *const kinds[] = {"DS PLOAM", "DS ALLOC", "DS GEM", "US BURST"};
    say(s, "%.3f %s%s", r->time / SNIFFER_CLOCK_MHZ, kinds[r->kind], r->flags & 0x80 ? " truncated" : "");
    if (r->length < min_length[r->kind]) {
        say(s, " short (%u bytes)\n", r->length);
        return 0;
    }
    const uint8_t *p = r->payload;
    switch (r->kind) {
    case SNIFFER_PLOAM:
        handle_ploam(s, r);
        break;
    case SNIFFER_ALLOC: {
        unsigned id = p[0] << 4 | p[1] >> 4, flags = (p[1] & 0xF) << 8 | p[2];
        say(s, " alloc-id=%u flags=%03x start=%u stop=%u%s\n", id, flags, be16(p + 3), be16(p + 5), r->flags & 1 ? " bad-crc" : "");
        if (!(r->flags & 1))
            learn_entry(s, r->time, p);
        break;
    }
    case SNIFFER_GEM:
        handle_gem(s, r);
        break;
    case SNIFFER_BURST:
        say(s, " onu=%u ind=%02x bytes=%u%s\n", p[1], p[2], r->length, r->flags & 1 ? " long" : "");
        if (p[1] != 0xFF && (int)p[1] != s->onu_id)
            set_onu(s, p[1]);
        if (!(r->flags & 0x81) && (int)p[1] == s->onu_id)
            learn_burst(s, r);
        break;
    }
    return 0;
}

static int feed(struct sniffer *s, struct sniffer_stream *st, uint32_t word)
{
    for (int i = 0; i < 4; i++)
        st->bytes[st->have++] = (uint8_t)(word >> (24 - 8 * i));
    struct sniffer_record record;
    int used = sniffer_parse(st->bytes, st->have, &record);
    if (used < 0) {
        fprintf(stderr, "sniffer: %s word %08x is no record start, dropped\n", st == &s->ds ? "DS" : "US", word);
        s->desyncs++;
        st->have = 0;
        return 0;
    }
    if (used == 0)
        return 0;
    st->have = 0;
    return sniffer_handle(s, &record);
}

int sniffer_poll(struct sniffer *s)
{
    uint32_t level = s->regs.read(s->regs.ctx, SNIFFER_LEVEL);
    unsigned ds = (level & SNIFFER_LEVEL_DSWORDS_MASK) >> SNIFFER_LEVEL_DSWORDS_SHIFT;
    unsigned us = (level & SNIFFER_LEVEL_USWORDS_MASK) >> SNIFFER_LEVEL_USWORDS_SHIFT;
    int synced = !!(level & SNIFFER_LEVEL_SYNCED_MASK);
    if (synced != s->synced) {
        s->synced = synced;
        say(s, "# downstream %s\n", synced ? "synced" : "lost");
    }
    // downstream first: a burst's grants were recorded before it closed
    for (unsigned i = 0; i < ds; i++)
        if (feed(s, &s->ds, s->regs.read(s->regs.ctx, SNIFFER_DS_DATA)) < 0)
            return -1;
    for (unsigned i = 0; i < us; i++)
        if (feed(s, &s->us, s->regs.read(s->regs.ctx, SNIFFER_US_DATA)) < 0)
            return -1;
    return (int)(ds + us);
}

void sniffer_report(struct sniffer *s)
{
    static const unsigned offsets[3] = {SNIFFER_DS_LOST, SNIFFER_US_LOST, SNIFFER_RESET_LOST};
    static const char *const names[3] = {"DS lost (drops | truncations << 16)", "US lost (drops | truncations << 16)", "burst resets lost"};
    for (int i = 0; i < 3; i++) {
        uint32_t value = s->regs.read(s->regs.ctx, offsets[i]);
        if (value != s->lost[i]) {
            s->lost[i] = value;
            fprintf(stderr, "sniffer: %s %08x\n", names[i], value);
        }
    }
}
