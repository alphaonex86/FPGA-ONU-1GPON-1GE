// SPDX-License-Identifier: CERN-OHL-S-2.0
#include "omci.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#define DEVICE_BASELINE 0x0A
#define DEVICE_EXTENDED 0x0B
#define AK 0x20

static unsigned be16(const uint8_t *p) { return (unsigned)p[0] << 8 | p[1]; }

unsigned omci_body(const uint8_t *m, unsigned length)
{
    if (length < 10)
        return 0;
    if (m[3] == DEVICE_BASELINE) // 32 bytes of contents
        return length >= 40 ? 38 : 0;
    unsigned contents = be16(m + 8);
    if (m[3] == DEVICE_EXTENDED)
        return length >= 10 + contents ? 8 + contents : 0;
    return 0;
}

uint32_t omci_crc32(const uint8_t *bytes, unsigned length)
{
    uint32_t crc = 0xFFFFFFFF;
    for (unsigned i = 0; i < length; i++) {
        crc ^= (uint32_t)bytes[i] << 24;
        for (int bit = 0; bit < 8; bit++)
            crc = crc & 0x80000000 ? crc << 1 ^ 0x04C11DB7 : crc << 1;
    }
    return ~crc;
}

static int hex_value(int c) { return isdigit(c) ? c - '0' : isxdigit(c) ? tolower(c) - 'a' + 10 : -1; }

// The hex word at *text (advanced past it) into a new buffer; its byte count, 0 when not hex.
static unsigned parse_hex(const char **text, uint8_t **bytes)
{
    const char *start = *text, *end = start;
    while (hex_value((unsigned char)*end) >= 0)
        end++;
    unsigned digits = (unsigned)(end - start);
    if (!digits || digits % 2 || (*end && !isspace((unsigned char)*end)))
        return 0;
    *bytes = malloc(digits / 2);
    if (!*bytes)
        return 0;
    for (unsigned i = 0; i < digits / 2; i++)
        (*bytes)[i] = (uint8_t)(hex_value((unsigned char)start[2 * i]) << 4 | hex_value((unsigned char)start[2 * i + 1]));
    *text = end;
    return digits / 2;
}

// A body with its TCI and trailer around it, as omci_body() reads it back.
static int body_ok(const uint8_t *body, unsigned length)
{
    uint8_t message[2 + 2048 + 4] = {0};
    if (length < 6 || length > 2048)
        return 0;
    memcpy(message + 2, body, length);
    return omci_body(message, body[1] == DEVICE_BASELINE ? OMCI_BASELINE_BYTES : 2 + length + 4) == length;
}

static struct omci_profile_entry *find(const struct omci_profile *p, const uint8_t *request, unsigned length)
{
    for (unsigned i = 0; i < p->count; i++)
        if (p->entries[i].request_length == length && !memcmp(p->entries[i].request, request, length))
            return &p->entries[i];
    return NULL;
}

// NULL, or what is wrong with the line.
static const char *load_line(struct omci_profile *p, const char *text)
{
    while (isspace((unsigned char)*text))
        text++;
    if (!*text || *text == '#')
        return NULL;
    struct omci_profile_entry e = {0};
    e.request_length = parse_hex(&text, &e.request);
    while (*text == ' ' || *text == '\t')
        text++;
    if (e.request_length)
        e.answer_length = parse_hex(&text, &e.answer);
    while (isspace((unsigned char)*text))
        text++;
    const char *error = !e.request_length || !e.answer_length ? "not two hex bodies (or out of memory)"
                        : *text && *text != '#'               ? "text after the answer"
                        : !body_ok(e.request, e.request_length) || !body_ok(e.answer, e.answer_length) ? "malformed body"
                        : (e.answer[0] & 0x1F) != (e.request[0] & 0x1F) || !(e.answer[0] & AK) || e.answer[1] != e.request[1]
                            ? "the answer is not the request's"
                            : NULL;
    if (error) {
        free(e.request);
        free(e.answer);
        return error;
    }
    struct omci_profile_entry *old = find(p, e.request, e.request_length);
    if (old) { // the latest answer wins
        free(old->request);
        free(old->answer);
        *old = e;
        return NULL;
    }
    if (p->count == p->room) {
        unsigned room = p->room ? 2 * p->room : 256;
        struct omci_profile_entry *entries = realloc(p->entries, room * sizeof(*entries));
        if (!entries) {
            free(e.request);
            free(e.answer);
            return "out of memory";
        }
        p->entries = entries;
        p->room = room;
    }
    p->entries[p->count++] = e;
    return NULL;
}

int omci_profile_load(struct omci_profile *p, FILE *file, const char *name)
{
    static char line[4 * 2048 + 256];
    unsigned number = 0;
    while (fgets(line, sizeof(line), file)) {
        number++;
        if (!strchr(line, '\n') && !feof(file)) {
            fprintf(stderr, "%s:%u: line too long\n", name, number);
            return -1;
        }
        const char *error = load_line(p, line);
        if (error) {
            fprintf(stderr, "%s:%u: %s\n", name, number, error);
            return -1;
        }
    }
    if (ferror(file)) {
        fprintf(stderr, "%s: read error\n", name);
        return -1;
    }
    return 0;
}

void omci_profile_free(struct omci_profile *p)
{
    for (unsigned i = 0; i < p->count; i++) {
        free(p->entries[i].request);
        free(p->entries[i].answer);
    }
    free(p->entries);
    *p = (struct omci_profile){0};
}

int omci_profile_answer(const struct omci_profile *p, const uint8_t *request, unsigned length, uint8_t *answer, unsigned room)
{
    unsigned body = omci_body(request, length);
    if (!body) {
        fprintf(stderr, "omci: malformed request (%u bytes)\n", length);
        return -1;
    }
    const struct omci_profile_entry *e = find(p, request + 2, body);
    if (!e)
        return 0;
    int baseline = e->answer[1] == DEVICE_BASELINE;
    unsigned total = baseline ? OMCI_BASELINE_BYTES : 2 + e->answer_length + 4;
    if (room < total) {
        fprintf(stderr, "omci: answer of %u bytes, room for %u\n", total, room);
        return -1;
    }
    memset(answer, 0, total);
    memcpy(answer, request, 2); // TCI
    memcpy(answer + 2, e->answer, e->answer_length);
    if (baseline)
        answer[43] = 40; // CPCS-UU 0, CPI 0, SDU length 0x0028
    // extended: the same CRC-32 as the MIC (unverified against a stock ONU)
    uint32_t crc = omci_crc32(answer, total - 4);
    for (int i = 0; i < 4; i++)
        answer[total - 4 + i] = (uint8_t)(crc >> (24 - 8 * i));
    return (int)total;
}
