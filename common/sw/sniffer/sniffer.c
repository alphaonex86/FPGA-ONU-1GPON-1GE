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

static const char *const ploamu_names[] = {
    NULL, "Serial_Number_ONU", "Password", "Dying_Gasp", "No_Message", "Encryption_Key", "Physical_Equipment_Error",
    "PST", "REI", "Acknowledge", "Sleep_Request",
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

#define NAME(table, i) ((i) < sizeof(table) / sizeof(table[0]) && table[i] ? table[i] : "?")

uint8_t sniffer_crc8(const uint8_t *bytes, size_t size)
{
    unsigned crc = 0;
    for (size_t i = 0; i < size; i++)
        for (int bit = 7; bit >= 0; bit--)
            crc = ((crc << 1) & 0xFF) ^ (((crc >> 7 ^ bytes[i] >> bit) & 1) ? 0x07 : 0);
    return (uint8_t)crc;
}

uint64_t sniffer_gem_header(unsigned pli, unsigned port, unsigned pti)
{
    uint64_t data = (uint64_t)(pli & 0xFFF) << 15 | (port & 0xFFF) << 3 | (pti & 7);
    unsigned remainder = 0;
    for (int i = 26; i >= 0; i--)
        remainder = ((remainder << 1) & 0xFFF) ^ (((remainder >> 11 ^ (unsigned)(data >> i)) & 1) ? 0x539 : 0);
    uint64_t code = data << 12 | remainder;
    return code << 1 | (__builtin_popcountll(code) & 1);
}

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

static void say_hex(struct sniffer *s, const uint8_t *bytes, unsigned size)
{
    for (unsigned i = 0; i < size; i++)
        say(s, "%02x", bytes[i]);
}

static int omci_index(const struct sniffer *s, unsigned port)
{
    for (unsigned i = 0; i < s->omci_port_count; i++)
        if (s->omci_ports[i] == port)
            return (int)i;
    return -1;
}

// OMCI message header (G.988 11.2): TCI, type, device, ME class / instance
static void say_omci(struct sniffer *s, const uint8_t *m)
{
    say(s, " omci tci=%04x %s%s%s me=%u/%u", be16(m), NAME(omci_names, m[2] & 0x1Fu), m[2] & 0x40 ? " ar" : "",
        m[2] & 0x20 ? " ak" : "", be16(m + 4), be16(m + 6));
}

// From the message type to the contents' end (TCI and trailer left out), 0 when malformed.
static unsigned omci_body(const struct sniffer_omci *m)
{
    if (m->length < 10)
        return 0;
    if (m->bytes[3] == 0x0A) // baseline: 32 bytes of contents
        return m->length >= 40 ? 38 : 0;
    unsigned contents = be16(m->bytes + 8);
    if (m->bytes[3] == 0x0B) // extended: its contents length
        return m->length >= 10 + contents ? 8 + contents : 0;
    return 0;
}

// One line per new pair: request body, answer body, the request's name and ME (the loader keeps the last answer).
static void profile_pair(struct sniffer *s, const struct sniffer_omci *request, const struct sniffer_omci *answer)
{
    static char line[4 * SNIFFER_OMCI_BYTES + 64];
    unsigned length = 0;
    for (int side = 0; side < 2; side++) {
        const struct sniffer_omci *m = side ? answer : request;
        unsigned body = omci_body(m);
        for (unsigned i = 0; i < body; i++)
            length += (unsigned)sprintf(line + length, "%02x", m->bytes[2 + i]);
        line[length++] = side ? '\0' : ' ';
    }
    const uint8_t *r = request->bytes;
    snprintf(line + length - 1, sizeof(line) - length + 1, " # %s %u/%u\n", NAME(omci_names, r[2] & 0x1Fu), be16(r + 4), be16(r + 6));
    uint64_t hash = 14695981039346656037ull; // FNV-1a
    for (const char *c = line; *c; c++)
        hash = (hash ^ (uint8_t)*c) * 1099511628211ull;
    hash |= 1; // 0: free slot
    if (s->profile_line_count < SNIFFER_PROFILE_LINES * 3 / 4) {
        unsigned slot = (unsigned)hash % SNIFFER_PROFILE_LINES;
        while (s->profile_lines[slot] && s->profile_lines[slot] != hash)
            slot = (slot + 1) % SNIFFER_PROFILE_LINES;
        if (s->profile_lines[slot])
            return;
        s->profile_lines[slot] = hash;
        s->profile_line_count++;
    }
    if (fputs(line, s->opt.profile) < 0) {
        perror("sniffer: profile");
        s->failed = 1;
    }
}

// A whole OMCI message: the OLT's requests wait for the answer with their TCI and type.
static void omci_message(struct sniffer *s, int upstream, const struct sniffer_omci *m)
{
    if (!omci_body(m)) {
        fprintf(stderr, "sniffer: malformed %s OMCI message (%u bytes)\n", upstream ? "US" : "DS", m->length);
        return;
    }
    if (!upstream) {
        s->pending[s->pending_next] = *m;
        s->pending_next = (s->pending_next + 1) % SNIFFER_PENDING;
        return;
    }
    if (!(m->bytes[2] & 0x20)) // not an answer: alarm, AVC
        return;
    for (unsigned k = 1; k <= SNIFFER_PENDING; k++) {
        struct sniffer_omci *request = &s->pending[(s->pending_next + SNIFFER_PENDING - k) % SNIFFER_PENDING];
        if (request->length && be16(request->bytes) == be16(m->bytes) && (request->bytes[2] & 0x1F) == (m->bytes[2] & 0x1F)) {
            if (s->opt.profile)
                profile_pair(s, request, m);
            request->length = 0;
            return;
        }
    }
}

// A GEM fragment: OMCI Port-IDs are reassembled, a cut message is lost.
static void omci_fragment(struct sniffer *s, int upstream, unsigned port, const uint8_t *data, unsigned size, int last, int cut)
{
    int i = omci_index(s, port);
    if (i < 0)
        return;
    struct sniffer_omci *m = &s->omci[upstream][i];
    if (cut || m->length + size > SNIFFER_OMCI_BYTES) {
        if (!cut)
            fprintf(stderr, "sniffer: OMCI message over %u bytes on Port-ID %u, dropped\n", SNIFFER_OMCI_BYTES, port);
        m->length = 0;
        return;
    }
    memcpy(m->bytes + m->length, data, size);
    m->length += (uint16_t)size;
    if (last) {
        omci_message(s, upstream, m);
        m->length = 0;
    }
}

// The OMCI header on a message's first fragment.
static void say_fragment(struct sniffer *s, int upstream, unsigned port, const uint8_t *data, unsigned size)
{
    int i = omci_index(s, port);
    if (i >= 0 && !s->omci[upstream][i].length && size >= 8)
        say_omci(s, data);
    say(s, " ");
    say_hex(s, data, size);
    say(s, "\n");
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
    if (opt->profile && fputs("# OMCI profile (common/sw/sniffer -P): request answer, hex from the message type to the contents' end\n",
                              opt->profile) < 0) {
        perror("sniffer: profile");
        return -1;
    }
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

static int uses_fec(const struct sniffer_entry *e) { return e->grants[0].flags >> 9 & 1; }

static unsigned burst_bytes(const struct sniffer_entry *e)
{
    unsigned bytes = 3 + ((e->stop - e->grants[0].start + 1) & 0xFFFF);
    return uses_fec(e) ? bytes + 16 * ((bytes + 238) / 239) : bytes;
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
    struct sniffer_grant grant = {(uint16_t)(p[0] << 4 | p[1] >> 4), (uint16_t)((p[1] & 0xF) << 8 | p[2]),
                                  (uint16_t)be16(p + 3), (uint16_t)be16(p + 5)};
    int contiguous = s->entry_count && time - s->previous_time < SNIFFER_FRAME_CLOCKS / 2 &&
                     grant.start == ((s->previous_stop + 1) & 0xFFFF);
    struct sniffer_entry *e;
    if (contiguous) {
        e = &s->entries[(s->entry_next + SNIFFER_ENTRIES - 1) % SNIFFER_ENTRIES];
        if (e->count == SNIFFER_GRANTS)
            fprintf(stderr, "sniffer: a burst of more than %u grants, the rest not decoded\n", SNIFFER_GRANTS);
        else
            e->grants[e->count++] = grant;
    } else {
        e = &s->entries[s->entry_next];
        *e = (struct sniffer_entry){.time = time, .count = 1, .grants = {grant}};
        s->entry_next = (s->entry_next + 1) % SNIFFER_ENTRIES;
        if (s->entry_count < SNIFFER_ENTRIES)
            s->entry_count++;
    }
    e->stop = grant.stop;
    s->previous_time = time;
    s->previous_stop = grant.stop;
}

// Learning: the burst's entry is the one whose grant has the burst's length, else (equal grants) the latest, both
// possible. Returns the entry at the learned offset, else the one of the burst's length, else NULL.
static const struct sniffer_entry *burst_entry(struct sniffer *s, const struct sniffer_record *r)
{
    int learn = !(r->flags & 0x81);
    int64_t best = -1;
    int best_matched = 0;
    const struct sniffer_entry *found = NULL, *best_entry = NULL;
    for (unsigned i = 0; i < s->entry_count; i++) {
        const struct sniffer_entry *e = &s->entries[i];
        if (r->time <= e->time)
            continue;
        int64_t offset = (int64_t)(r->time - e->time) + PLOU_CLOCKS - 2 * (int64_t)e->grants[0].start;
        if (offset < MIN_OFFSET || offset > MAX_OFFSET)
            continue;
        if (s->offset >= 0 && llabs(offset - s->offset) <= VOTE_SLACK)
            found = e;
        int matched = learn && abs((int)burst_bytes(e) - (int)r->length) <= LENGTH_SLACK;
        if (matched > best_matched || (matched == best_matched && (best < 0 || offset < best))) {
            best = offset;
            best_matched = matched;
            best_entry = e;
        }
    }
    if (learn && best >= 0)
        vote(s, best);
    return found ? found : best_matched ? best_entry : NULL;
}

// Returns the bytes after it.
static unsigned decode_ploamu(struct sniffer *s, const uint8_t *data, unsigned at, unsigned end)
{
    if (at + 13 > end) {
        say(s, "  ploamu cut\n");
        return at + 13;
    }
    const uint8_t *m = data + at;
    say(s, "  ploamu onu=%u msg=0x%02x %s ", m[0], m[1], NAME(ploamu_names, m[1]));
    say_hex(s, m + 2, 10);
    if (m[1] == 0x01)
        say(s, " sn=%.4s%08x", (const char *)m + 2, be32(m + 6));
    say(s, "%s\n", sniffer_crc8(m, 12) == m[12] ? "" : " bad-crc");
    return at + 13;
}

static void decode_gem(struct sniffer *s, const uint8_t *data, unsigned at, unsigned end)
{
    static const uint8_t mask[5] = {0xB6, 0xAB, 0x31, 0xE0, 0x55};
    while (at + 5 <= end) {
        uint64_t header = 0;
        for (int i = 0; i < 5; i++)
            header = header << 8 | (data[at + i] ^ mask[i]);
        at += 5;
        if (header == 0) // idle
            continue;
        unsigned pli = (unsigned)(header >> 28), port = (header >> 16) & 0xFFF, pti = (header >> 13) & 7;
        if (sniffer_gem_header(pli, port, pti) != header) {
            say(s, "  gem bad-hec %010llx\n", (unsigned long long)header);
            return;
        }
        unsigned bytes = pli < end - at ? pli : end - at;
        say(s, "  gem port=%u pli=%u pti=%u%s", port, pli, pti, bytes < pli ? " cut" : "");
        say_fragment(s, 1, port, data + at, bytes);
        omci_fragment(s, 1, port, data + at, bytes, pti & 1, bytes < pli);
        at += pli;
    }
}

// Each grant: PLOAMu, PLSu, DBRu as its flags say, then GEM frames (G.984.3 8.2); FEC parity left out first.
static void decode_burst(struct sniffer *s, const struct sniffer_record *r, const struct sniffer_entry *e)
{
    static const unsigned dbru_bytes[4] = {0, 2, 3, 5};
    static uint8_t clear[65536];
    const uint8_t *data = r->payload;
    unsigned n = r->length;
    if (uses_fec(e)) {
        unsigned kept = 0;
        for (unsigned at = 0; at < n; at += 255) {
            unsigned chunk = n - at < 255 ? n - at : 255;
            if (chunk > 16) {
                memcpy(clear + kept, data + at, chunk - 16);
                kept += chunk - 16;
            }
        }
        data = clear;
        n = kept;
    }
    for (unsigned i = 0; i < e->count; i++) {
        const struct sniffer_grant *g = &e->grants[i];
        unsigned at = 3 + ((g->start - e->grants[0].start) & 0xFFFF), end = 3 + ((g->stop - e->grants[0].start + 1) & 0xFFFF);
        if (end > n)
            end = n;
        say(s, "  grant alloc-id=%u flags=%03x\n", g->alloc_id, g->flags);
        if (g->flags & 0x400)
            at = decode_ploamu(s, data, at, end);
        if (g->flags & 0x800)
            at += 120;
        unsigned dbru = dbru_bytes[g->flags >> 7 & 3];
        if (dbru && at + dbru <= end) {
            say(s, "  dbru ");
            say_hex(s, data + at, dbru - 1);
            say(s, "%s\n", sniffer_crc8(data + at, dbru - 1) == data[at + dbru - 1] ? "" : " bad-crc");
        }
        decode_gem(s, data, at + dbru, end);
    }
}

static void handle_ploam(struct sniffer *s, const struct sniffer_record *r)
{
    const uint8_t *p = r->payload;
    unsigned onu = p[0], msg = p[1];
    int bad = r->flags & 1;
    say(s, " onu=%u msg=0x%02x %s", onu, msg, NAME(ploam_names, msg));
    say(s, " ");
    say_hex(s, p + 2, 10);
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
    say_fragment(s, 0, port, data, bytes);
    omci_fragment(s, 0, port, data, bytes, r->flags & 1, r->flags & 0x88);
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
        if (p[1] == 0xFF) { // serial number answer: a PLOAMu alone
            decode_ploamu(s, p, 3, r->length);
            break;
        }
        if ((int)p[1] != s->onu_id)
            set_onu(s, p[1]);
        const struct sniffer_entry *e = burst_entry(s, r);
        if (e)
            decode_burst(s, r, e);
        break;
    }
    return s->failed ? -1 : 0;
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
