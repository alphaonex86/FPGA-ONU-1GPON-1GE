// SPDX-License-Identifier: CERN-OHL-S-2.0
// OMCI (G.988) message helpers and the clone profiles recorded by the sniffer (common/sw/sniffer -P).
#ifndef OMCI_H
#define OMCI_H

#include <stdint.h>
#include <stdio.h>

#define OMCI_BASELINE_BYTES 48

// Bytes from the message type to the contents' end (TCI and trailer left out), 0 when malformed.
unsigned omci_body(const uint8_t *message, unsigned length);
// AAL5 CRC-32 (big-endian, as the baseline trailer's).
uint32_t omci_crc32(const uint8_t *bytes, unsigned length);

struct omci_profile_entry {
    uint8_t *request, *answer; // bodies
    unsigned request_length, answer_length;
};

struct omci_profile {
    struct omci_profile_entry *entries;
    unsigned count, room;
};

// Lines "request answer [# comment]", bodies in hex; a later line for the same request replaces its answer.
// Returns 0, -1 on an error (logged with its line).
int omci_profile_load(struct omci_profile *profile, FILE *file, const char *name);
void omci_profile_free(struct omci_profile *profile);
// The recorded answer to `request` (a whole message) with its TCI and a new trailer: its bytes, 0 when the profile has
// none, -1 when the request is malformed or `room` too small (logged).
int omci_profile_answer(const struct omci_profile *profile, const uint8_t *request, unsigned length, uint8_t *answer,
                        unsigned room);

// The binary profile the GPL core loads (omci_onu_set_profile): OMCI_PROFILE_MAGIC, then records of
// OMCI_PROFILE_RECORD bytes (request bytes 2..39, answer bytes 2..39) sorted by request. Baseline entries only:
// returns how many were written, -1 on a write error (logged).
#define OMCI_PROFILE_MAGIC "OMCIPRF1"
#define OMCI_PROFILE_RECORD 76
int omci_profile_write_binary(const struct omci_profile *profile, FILE *file);

#endif
