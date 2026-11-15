// SPDX-License-Identifier: CERN-OHL-S-2.0
#ifndef AES128_H
#define AES128_H

#include <stdint.h>

// AES-128 encryption of one block (FIPS-197).
void aes128_encrypt(const uint8_t key[16], const uint8_t in[16], uint8_t out[16]);

#endif
