// SPDX-License-Identifier: CERN-OHL-S-2.0
#include "aes128.h"

#include <string.h>

static uint8_t xtime(uint8_t a) { return (uint8_t)(a << 1 ^ (a & 0x80 ? 0x1B : 0)); }

// The S-box, computed once: the affine map of the inverse in GF(2^8).
static const uint8_t *sbox(void)
{
    static uint8_t box[256];
    if (box[0])
        return box;
    uint8_t p = 1, q = 1;
    do { // p runs through the field by 3, q by its inverse
        p = (uint8_t)(p ^ xtime(p));
        q ^= (uint8_t)(q << 1);
        q ^= (uint8_t)(q << 2);
        q ^= (uint8_t)(q << 4);
        q ^= q & 0x80 ? 0x09 : 0;
        uint8_t x = (uint8_t)(q ^ (q << 1 | q >> 7) ^ (q << 2 | q >> 6) ^ (q << 3 | q >> 5) ^ (q << 4 | q >> 4));
        box[p] = x ^ 0x63;
    } while (p != 1);
    box[0] = 0x63;
    return box;
}

void aes128_encrypt(const uint8_t key[16], const uint8_t in[16], uint8_t out[16])
{
    const uint8_t *s = sbox();
    uint8_t k[16], t[16], rcon = 1;
    memcpy(k, key, 16);
    for (int i = 0; i < 16; i++)
        out[i] = in[i] ^ k[i];
    for (int round = 1; round <= 10; round++) {
        for (int i = 0; i < 16; i++) // SubBytes and ShiftRows
            t[i] = s[out[(i + 4 * (i % 4)) % 16]];
        for (int c = 0; c < 16 && round < 10; c += 4) { // MixColumns
            uint8_t a0 = t[c], a1 = t[c + 1], a2 = t[c + 2], a3 = t[c + 3], all = a0 ^ a1 ^ a2 ^ a3;
            t[c] = (uint8_t)(a0 ^ all ^ xtime(a0 ^ a1));
            t[c + 1] = (uint8_t)(a1 ^ all ^ xtime(a1 ^ a2));
            t[c + 2] = (uint8_t)(a2 ^ all ^ xtime(a2 ^ a3));
            t[c + 3] = (uint8_t)(a3 ^ all ^ xtime(a3 ^ a0));
        }
        k[0] ^= s[k[13]] ^ rcon; // the next round key
        k[1] ^= s[k[14]];
        k[2] ^= s[k[15]];
        k[3] ^= s[k[12]];
        for (int i = 4; i < 16; i++)
            k[i] ^= k[i - 4];
        rcon = xtime(rcon);
        for (int i = 0; i < 16; i++)
            out[i] = t[i] ^ k[i];
    }
}
