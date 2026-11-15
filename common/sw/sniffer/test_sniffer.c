// SPDX-License-Identifier: CERN-OHL-S-2.0
// Host test: a register model fed with records as RecordWriter + RecordWords write them.
#include "sniffer.h"

#include <stdlib.h>
#include <string.h>

typedef uint32_t u32;
#include "sniffer_regs.h"

#define QUEUE 65536

struct queue { uint32_t words[QUEUE]; unsigned head, tail; };

struct model {
    uint32_t regs[64];
    uint8_t alloc_keep[4096], port_keep[4096], cipher[4096];
    uint8_t key[16];
    uint32_t key_switch;
    unsigned key_writes, key_forgets;
    struct queue ds, us;
};

static int failures;

#define CHECK(cond)                                                       \
    do {                                                                  \
        if (!(cond)) {                                                    \
            fprintf(stderr, "%s:%d: CHECK(%s) failed\n", __FILE__, __LINE__, #cond); \
            failures++;                                                   \
        }                                                                 \
    } while (0)

static uint32_t pop(struct queue *q) { return q->head == q->tail ? 0xDEADBEEF : q->words[q->head++ % QUEUE]; }
static unsigned level(const struct queue *q, unsigned max) { return q->tail - q->head < max ? q->tail - q->head : max; }

static uint32_t model_read(void *ctx, unsigned offset)
{
    struct model *m = ctx;
    switch (offset) {
    case SNIFFER_LEVEL: return level(&m->ds, 128) | level(&m->us, 0xFFFF) << SNIFFER_LEVEL_USWORDS_SHIFT | SNIFFER_LEVEL_SYNCED_MASK;
    case SNIFFER_DS_DATA: return pop(&m->ds);
    case SNIFFER_US_DATA: return pop(&m->us);
    default: return m->regs[offset / 4];
    }
}

static void model_write(void *ctx, unsigned offset, uint32_t value)
{
    struct model *m = ctx;
    if (offset == SNIFFER_ALLOC_MAP)
        m->alloc_keep[value & 0xFFF] = value >> 12 & 1;
    else if (offset == SNIFFER_PORT_MAP)
        m->port_keep[value & 0xFFF] = value >> 12 & 1;
    else
        m->regs[offset / 4] = value;
}

static void model_cipher(void *ctx, unsigned port, int ciphered)
{
    ((struct model *)ctx)->cipher[port] = (uint8_t)ciphered;
}

static void model_key(void *ctx, const uint8_t *key, uint32_t superframe)
{
    struct model *m = ctx;
    if (!key) {
        m->key_forgets++;
        return;
    }
    memcpy(m->key, key, 16);
    m->key_switch = superframe;
    m->key_writes++;
}

static void push(struct queue *q, unsigned kind, unsigned flags, uint64_t time, const uint8_t *payload, unsigned length)
{
    uint8_t bytes[12 + 65536 + 3] = {(uint8_t)(0xA0 | kind), (uint8_t)flags, (uint8_t)length, (uint8_t)(length >> 8)};
    for (int i = 0; i < 8; i++)
        bytes[4 + i] = (uint8_t)(time >> (8 * i));
    memcpy(bytes + 12, payload, length);
    unsigned total = (12 + length + 3) & ~3u;
    for (unsigned i = 0; i < total; i += 4)
        q->words[q->tail++ % QUEUE] = (uint32_t)bytes[i] << 24 | bytes[i + 1] << 16 | bytes[i + 2] << 8 | bytes[i + 3];
}

static void ploam(struct model *m, uint64_t time, unsigned onu, unsigned msg, const uint8_t data[10])
{
    uint8_t p[12] = {(uint8_t)onu, (uint8_t)msg};
    memcpy(p + 2, data, 10);
    push(&m->ds, SNIFFER_PLOAM, 0, time, p, 12);
}

static void alloc(struct model *m, uint64_t time, unsigned id, unsigned flags, unsigned start, unsigned stop)
{
    uint8_t p[7] = {(uint8_t)(id >> 4), (uint8_t)(id << 4 | flags >> 8), (uint8_t)flags, (uint8_t)(start >> 8), (uint8_t)start,
                    (uint8_t)(stop >> 8), (uint8_t)stop};
    push(&m->ds, SNIFFER_ALLOC, 0, time, p, 7);
}

static void burst(struct model *m, uint64_t time, unsigned onu, unsigned bytes)
{
    uint8_t p[64] = {0x5A, (uint8_t)onu};
    push(&m->us, SNIFFER_BURST, 0, time, p, bytes);
}

static const uint8_t gem_mask[5] = {0xB6, 0xAB, 0x31, 0xE0, 0x55};

static unsigned gem(uint8_t *at, unsigned pli, unsigned port, unsigned pti)
{
    uint64_t header = sniffer_gem_header(pli, port, pti);
    for (int i = 0; i < 5; i++)
        at[i] = (uint8_t)(header >> (32 - 8 * i)) ^ gem_mask[i];
    return 5;
}

static void idle(uint8_t *burst, unsigned at, unsigned end)
{
    for (; at + 5 <= end; at += 5)
        memcpy(burst + at, gem_mask, 5);
}

// grant 0 (PLOAMu, DBRu mode 0): an Acknowledge, a report, an OMCI answer, a frame of 8 bytes; grant 1: a frame of 20
// bytes; idle after both. `filtered`: as the in-line sniffer's rules leave it (3 bytes of the first frame, none of the
// second, the line length at its end). `key_half` 0 or 1: the PLOAMu is that half of key 00 01 .. 0f (index 1), else
// an Acknowledge. Returns the record's bytes.
static unsigned answer(uint8_t *b, unsigned start, unsigned middle, unsigned stop, int filtered, int key_half)
{
    static const uint8_t omci[48] = {0x80, 0x01, 0x2D, 0x0A, 0x00, 0x02, 0x00, 0x00, 0x00, 0x05}; // 5 uploads
    unsigned at = 3;
    b[0] = 0x5A;
    b[1] = 5;
    b[2] = 0;
    uint8_t ploam[13] = {5, 0x09, 0x0A, 0x05};
    if (key_half >= 0) {
        memcpy(ploam, (const uint8_t[12]){5, 0x05, 1, (uint8_t)key_half}, 12);
        for (int i = 0; i < 8; i++)
            ploam[4 + i] = (uint8_t)(8 * key_half + i);
    }
    ploam[12] = sniffer_crc8(ploam, 12);
    memcpy(b + at, ploam, 13);
    at += 13;
    b[at] = 0x10;
    b[at + 1] = sniffer_crc8(b + at, 1);
    at += 2;
    at += gem(b + at, 48, 0x123, 1);
    memcpy(b + at, omci, 48);
    at += 48;
    unsigned line = 3 + stop - start + 1, out = filtered ? 5 : 0;
    at += gem(b + at, 8, 0x200, 1);
    memset(b + at, 0xA5, 8 - out);
    at += 8 - out;
    if (filtered) { // HEC field ^ (0x1000 | bytes left)
        b[at - 5] ^= 0x10;
        b[at - 4] ^= 3;
    }
    idle(b, at, 3 + middle - start + 1 - out);
    at = 3 + middle + 1 - start - out;
    at += gem(b + at, 20, 0x201, 1);
    if (filtered) {
        b[at - 2] ^= 0x10;
        out += 20;
    } else {
        memset(b + at, 0x5A, 20);
        at += 20;
    }
    idle(b, at, line - out);
    if (!filtered)
        return line;
    b[line - out] = (uint8_t)line;
    b[line - out + 1] = (uint8_t)(line >> 8);
    return line - out + 2;
}

static void drain(struct sniffer *s, struct model *m)
{
    while (m->ds.head != m->ds.tail || m->us.head != m->us.tail)
        CHECK(sniffer_poll(s) >= 0);
}

int main(void)
{
    static struct model m;
    static struct sniffer s;
    char *text = NULL, *dump = NULL, *profile = NULL;
    size_t text_size = 0, dump_size = 0, profile_size = 0;
    struct sniffer_options opt = {.onu_id = -1, .delimiter = 0xAB5983, .lead = 128, .width = 16, .allocs = {6}, .alloc_count = 1};
    opt.text = open_memstream(&text, &text_size);
    opt.dump = open_memstream(&dump, &dump_size);
    opt.profile = open_memstream(&profile, &profile_size);
    CHECK(opt.text && opt.dump && opt.profile);
    m.alloc_keep[77] = m.port_keep[77] = 1; // a previous run's
    struct sniffer_regs regs = {&m, model_read, model_write, model_cipher, model_key, NULL};
    CHECK(sniffer_start(&s, &regs, &opt) == 0);
    uint32_t ctrl = m.regs[SNIFFER_CTRL / 4];
    CHECK(ctrl & SNIFFER_CTRL_ENABLE_MASK);
    CHECK((ctrl & SNIFFER_CTRL_ONUID_MASK) >> SNIFFER_CTRL_ONUID_SHIFT == 0xFF);
    CHECK(!m.alloc_keep[77] && !m.port_keep[77] && m.alloc_keep[6]);
    CHECK(m.regs[SNIFFER_DELIMITER / 4] == 0xAB5983 && m.regs[SNIFFER_RESET_TIMING / 4] == 0);

    // auto-configuration from the exchange
    ploam(&m, 10, 0xFF, 0x01, (const uint8_t[10]){4, 0, 0, 0, 0x12, 0x34, 0x56});
    burst(&m, 20, 0xFF, 16); // serial number answer: no ONU-ID yet
    drain(&s, &m);
    CHECK(m.regs[SNIFFER_DELIMITER / 4] == 0x123456 && s.onu_id == -1);
    burst(&m, 30, 5, 16);
    drain(&s, &m);
    CHECK((m.regs[SNIFFER_CTRL / 4] & SNIFFER_CTRL_ONUID_MASK) >> SNIFFER_CTRL_ONUID_SHIFT == 5 && m.alloc_keep[5]);
    ploam(&m, 40, 5, 0x0A, (const uint8_t[10]){1000 >> 4, (1000 & 0xF) << 4, 0x01});
    ploam(&m, 41, 4, 0x0A, (const uint8_t[10]){2000 >> 4, (2000 & 0xF) << 4, 0x01}); // another ONU's
    ploam(&m, 42, 5, 0x0E, (const uint8_t[10]){0x01, 0x123 >> 4, (0x123 & 0xF) << 4});
    drain(&s, &m);
    CHECK(m.alloc_keep[1000] && !m.alloc_keep[2000] && m.port_keep[0x123]);
    ploam(&m, 50, 5, 0x0A, (const uint8_t[10]){1000 >> 4, (1000 & 0xF) << 4, 0xFF});
    uint8_t omci[10 + 48] = {0x01, 0x23, 0, 48, 0, 0, 0, 0, 0, 1, 0x80, 0x01, 0x4D, 0x0A, 0x00, 0x02, 0x00, 0x00};
    push(&m.ds, SNIFFER_GEM, 1, 60, omci, sizeof(omci)); // PTI 1: its end
    drain(&s, &m);
    CHECK(!m.alloc_keep[1000]);

    // offset learning: grants of varied sizes every frame, two contiguous entries, answered two frames later
    const int64_t offset = 50000;
    static uint8_t bursts[64][512];
    struct { uint64_t time; unsigned bytes; } planned[64]; // every other one as the in-line sniffer's rules leave it
    unsigned planned_count = 0, sent = 0;
    for (unsigned k = 0; k < 40; k++) {
        uint64_t frame = 1000000 + (uint64_t)k * SNIFFER_FRAME_CLOCKS;
        unsigned start = 100 + k * 37 % 500, middle = start + 80 + k * 13 % 50, stop = middle + 30;
        alloc(&m, frame + 200, 5, 0x400 | 0x080, start, middle);
        alloc(&m, frame + 208, 6, 0, middle + 1, stop);
        planned[planned_count].time = (uint64_t)((int64_t)(frame + 200) + offset + 2 * start - 6);
        planned[planned_count].bytes = answer(bursts[planned_count], start, middle, stop, planned_count & 1, k == 30 ? 0 : k == 31 ? 1 : -1);
        planned_count++;
        while (sent < planned_count && planned[sent].time < frame + SNIFFER_FRAME_CLOCKS) {
            push(&m.us, SNIFFER_BURST, sent & 1 ? 2 : 0, planned[sent].time, bursts[sent], planned[sent].bytes);
            sent++;
        }
        drain(&s, &m);
    }
    CHECK(s.offset == offset);
    CHECK(m.regs[SNIFFER_RESET_OFFSET / 4] == (uint32_t)offset);
    CHECK(m.regs[SNIFFER_RESET_TIMING / 4] == (128u | 16u << 16));

    // ciphering: Port-IDs 0x123 (OMCI) and 0x124; the key's halves went up in bursts 30 and 31; the keystream of key
    // 00 01 .. 0f from crypto-counter 0x123456789AB on is OpenSSL's (AES-128-ECB of the counter three times, + 1 per block)
    static const uint8_t stream[48] = {
        0xAD, 0xD3, 0xCC, 0x95, 0xFA, 0x5A, 0xFB, 0xB1, 0x2A, 0xC5, 0xCD, 0xCE, 0xB8, 0x97, 0xC8, 0x93,
        0x7F, 0x90, 0x43, 0xBD, 0xD1, 0x15, 0xCC, 0x09, 0xAD, 0x2B, 0x8A, 0xA6, 0x1F, 0x78, 0x60, 0x5B,
        0xE7, 0x0B, 0x4D, 0x91, 0x9E, 0x01, 0xCA, 0x8A, 0xC5, 0x2B, 0x21, 0x67, 0x09, 0x17, 0x1B, 0xA8};
    uint8_t get[10 + 48] = {0x01, 0x23, 0, 48, 0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0x80, 0x02, 0x49, 0x0A, 0x01, 0x00};
    uint8_t zeros[10 + 16] = {0x01, 0x24, 0, 16, 0x01, 0x23, 0x45, 0x67, 0x89, 0xAB};
    for (int i = 0; i < 48; i++)
        get[10 + i] ^= stream[i];
    memcpy(zeros + 10, stream, 16);
    CHECK(m.key_forgets == 1 && m.key_writes == 0);
    ploam(&m, 9000000, 5, 0x08, (const uint8_t[10]){0x01, 0x123 >> 4, (0x123 & 0xF) << 4});
    ploam(&m, 9000001, 5, 0x08, (const uint8_t[10]){0x01, 0x124 >> 4, (0x124 & 0xF) << 4});
    ploam(&m, 9000002, 4, 0x08, (const uint8_t[10]){0x01, 0x125 >> 4, (0x125 & 0xF) << 4}); // another ONU's
    push(&m.ds, SNIFFER_GEM, 1, 9000010, get, sizeof(get)); // no key in use yet
    drain(&s, &m);
    CHECK(m.cipher[0x123] && m.cipher[0x124] && !m.cipher[0x125] && s.key_parts == 3 && !s.key_valid);
    ploam(&m, 9000020, 5, 0x13, (const uint8_t[10]){0x01, 0x23, 0x45, 0x00});
    ploam(&m, 9000021, 5, 0x13, (const uint8_t[10]){0x01, 0x23, 0x45, 0x00}); // sent again: one write
    drain(&s, &m);
    CHECK(m.key_writes == 1 && m.key_switch == 0x01234500 && m.key[0] == 0 && m.key[9] == 9 && m.key[15] == 15);
    push(&m.ds, SNIFFER_GEM, 1, 9000030, get, sizeof(get)); // superframe 0x1234567: the key is in use
    push(&m.ds, SNIFFER_GEM, 1, 9000031, zeros, sizeof(zeros));
    drain(&s, &m);
    CHECK(s.key_valid && !s.key_due);

    sniffer_stop(&s);
    CHECK(m.regs[SNIFFER_CTRL / 4] == 0);
    fclose(opt.text);
    fclose(opt.dump);
    fclose(opt.profile);
    char expected[512];
    snprintf(expected, sizeof(expected), "4d0a00020000%064d 2d0a000200000005%060d # MIB_upload 2/0\n", 0, 0);
    const char *pair = strchr(profile, '\n');
    CHECK(pair && strcmp(pair + 1, expected) == 0); // the first answer only: the others have no request
    CHECK(strstr(text, "MIB_upload") && strstr(text, "RESET_OFFSET 50000"));
    CHECK(!strstr(text, "bad-") && !strstr(text, "cut"));
    unsigned answers = 0, acks = 0, reports = 0, whole = 0, shortened = 0, dropped = 0, idles = 0;
    for (const char *at = text; (at = strstr(at, "  gem port=512 pli=8 pti=1 a5a5a5a5a5a5a5a5\n")); at++)
        whole++;
    for (const char *at = text; (at = strstr(at, "  gem port=512 pli=8 pti=1 kept=3 a5a5a5\n")); at++)
        shortened++;
    for (const char *at = text; (at = strstr(at, "  gem port=513 pli=20 pti=1 kept=0 \n")); at++)
        dropped++;
    for (const char *at = text; (at = strstr(at, "  gem port=513 pli=20 pti=1 5a5a")); at++)
        idles++;
    CHECK(whole == (sent + 1) / 2 && shortened == sent / 2 && dropped == sent / 2 && idles == (sent + 1) / 2);
    for (const char *at = text; (at = strstr(at, "omci tci=8001 MIB_upload ak me=2/0")); at++)
        answers++;
    for (const char *at = text; (at = strstr(at, "ploamu onu=5 msg=0x09 Acknowledge")); at++)
        acks++;
    for (const char *at = text; (at = strstr(at, "  dbru 10\n")); at++)
        reports++;
    CHECK(answers == sent && acks == sent - 2 && reports == sent);
    unsigned unread = 0, read = 0;
    for (const char *at = text; (at = strstr(at, "pti=1 counter=123456789ab ciphered ")); at++)
        unread++;
    for (const char *at = text; (at = strstr(at, "pti=1 counter=123456789ab omci tci=8002 Get ar me=256/0")); at++)
        read++;
    CHECK(unread == 1 && read == 1);
    CHECK(strstr(text, "port=292 pli=16 pti=1 counter=123456789ab 00000000000000000000000000000000\n") != NULL);
    CHECK(strstr(text, "Encryption_Key") && strstr(text, "Key_Switching_Time") && strstr(text, "port-id=291 ciphered"));

    // the dump replays record by record
    unsigned records = 0;
    for (size_t at = 0; at < dump_size;) {
        struct sniffer_record r;
        int used = sniffer_parse((const uint8_t *)dump + at, dump_size - at, &r);
        CHECK(used > 0);
        if (used <= 0)
            break;
        at += (size_t)used;
        records++;
    }
    CHECK(records == 8 + 80 + sent + 8);
    free(text);
    free(dump);
    free(profile);
    if (failures)
        fprintf(stderr, "%d check(s) failed\n", failures);
    else
        printf("test_sniffer: ok\n");
    return failures != 0;
}
