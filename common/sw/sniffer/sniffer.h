// SPDX-License-Identifier: CERN-OHL-S-2.0
// GPON tap sniffer (SnifferCore): reads its records, configures it from the tapped exchange, learns RESET_OFFSET.
#ifndef SNIFFER_H
#define SNIFFER_H

#include <stdint.h>
#include <stdio.h>

#define SNIFFER_CLOCK_MHZ 311.04 // record time unit
#define SNIFFER_FRAME_CLOCKS 38880u
#define SNIFFER_MAX_IDS 16
#define SNIFFER_ENTRIES 64
#define SNIFFER_VOTES 16
#define SNIFFER_GRANTS 16 // decoded per burst
#define SNIFFER_OMCI_BYTES 2048 // an extended OMCI message, whole
#define SNIFFER_PENDING 32
#define SNIFFER_PROFILE_LINES 16384 // remembered to skip repeats

enum sniffer_kind { SNIFFER_PLOAM, SNIFFER_ALLOC, SNIFFER_GEM, SNIFFER_BURST };

struct sniffer_record {
    unsigned kind, flags, length; // length: payload bytes
    uint64_t time;
    const uint8_t *payload; // follows the record's 12 header bytes
};

// Register window of the sniffer (byte offsets of sniffer_regs.h); write NULL: replay, nothing configured.
struct sniffer_regs {
    void *ctx;
    uint32_t (*read)(void *ctx, unsigned offset);
    void (*write)(void *ctx, unsigned offset, uint32_t value);
};

struct sniffer_options {
    int onu_id;             // -1: learned from the bursts (or the serial number)
    const uint8_t *serial;  // 8 bytes (vendor + serial) or NULL
    uint32_t delimiter;     // until an Upstream_Overhead
    unsigned lead, width;   // burst reset, clocks
    int all_ploams;
    uint16_t allocs[SNIFFER_MAX_IDS], ports[SNIFFER_MAX_IDS];
    unsigned alloc_count, port_count;
    FILE *text;             // decoded records, or NULL
    FILE *dump;             // raw records, or NULL
    FILE *profile;          // OMCI request/answer pairs, or NULL
};

struct sniffer_stream {
    uint8_t bytes[12 + 65535 + 3];
    unsigned have;
};

struct sniffer_omci { uint16_t length; uint8_t bytes[SNIFFER_OMCI_BYTES]; };

struct sniffer_grant { uint16_t alloc_id, flags, start, stop; };
// A burst's BWmap entries (contiguous), stop: its last StopTime
struct sniffer_entry { uint64_t time; unsigned stop, count; struct sniffer_grant grants[SNIFFER_GRANTS]; };

struct sniffer {
    struct sniffer_regs regs;
    struct sniffer_options opt;
    int onu_id;
    uint32_t ctrl;
    int synced;
    uint32_t lost[3]; // DS_LOST, US_LOST, RESET_LOST as last read
    uint16_t omci_ports[SNIFFER_MAX_IDS];
    unsigned omci_port_count;
    struct sniffer_stream ds, us;
    unsigned desyncs;
    struct sniffer_omci omci[2][SNIFFER_MAX_IDS]; // reassembly, downstream and upstream, per OMCI Port-ID
    struct sniffer_omci pending[SNIFFER_PENDING]; // the OLT's requests waiting for their answer
    unsigned pending_next;
    uint64_t profile_lines[SNIFFER_PROFILE_LINES]; // hashes of the lines written, open addressing
    unsigned profile_line_count;
    int failed; // an output write failed (logged)
    // offset learning: the first entries of the bursts, and the offsets they gave
    struct sniffer_entry entries[SNIFFER_ENTRIES];
    unsigned entry_next, entry_count;
    uint64_t previous_time;
    unsigned previous_stop;
    int64_t votes[SNIFFER_VOTES];
    unsigned vote_next, vote_count;
    int64_t offset; // -1 unknown
};

// Bytes of a record at `bytes` (padded to words), 0 when `size` holds less, -1 when it is not a record.
int sniffer_parse(const uint8_t *bytes, size_t size, struct sniffer_record *record);
int sniffer_start(struct sniffer *s, const struct sniffer_regs *regs, const struct sniffer_options *opt);
void sniffer_stop(struct sniffer *s);
// Reads the waiting words; returns how many, -1 on an error (logged).
int sniffer_poll(struct sniffer *s);
// Dumps, prints, learns from one record; -1 on an error (logged).
int sniffer_handle(struct sniffer *s, const struct sniffer_record *record);
// Prints the sync and lost counters when they change.
void sniffer_report(struct sniffer *s);
// G.984.3 helpers: PLOAM/DBRu CRC-8, GEM header with its HEC (before the mask).
uint8_t sniffer_crc8(const uint8_t *bytes, size_t size);
uint64_t sniffer_gem_header(unsigned pli, unsigned port, unsigned pti);

#endif
