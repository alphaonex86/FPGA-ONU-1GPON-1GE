// SPDX-License-Identifier: CERN-OHL-S-2.0
#include "omci.h"

#include <string.h>

static int failures;

#define CHECK(cond)                                                                    \
    do {                                                                               \
        if (!(cond)) {                                                                 \
            fprintf(stderr, "%s:%d: CHECK(%s) failed\n", __FILE__, __LINE__, #cond); \
            failures++;                                                                \
        }                                                                              \
    } while (0)

static int load(struct omci_profile *p, const char *text)
{
    FILE *file = fmemopen((void *)text, strlen(text), "r");
    CHECK(file);
    int result = omci_profile_load(p, file, "test");
    fclose(file);
    return result;
}

int main(void)
{
    CHECK(omci_crc32((const uint8_t *)"123456789", 9) == 0xFC891918); // CRC-32/BZIP2 check value

    // as the sniffer writes it (MIB upload: 5 uploads), then the same request answered again (6)
    static const char text[] =
        "# OMCI profile\n"
        "4d0a00020000" "0000000000000000000000000000000000000000000000000000000000000000"
        " 2d0a00020000" "0005000000000000000000000000000000000000000000000000000000000000 # MIB_upload 2/0\n"
        "\n"
        "4d0a00020000" "0000000000000000000000000000000000000000000000000000000000000000"
        " 2d0a00020000" "0006000000000000000000000000000000000000000000000000000000000000\n";
    struct omci_profile p = {0};
    CHECK(load(&p, text) == 0 && p.count == 1);

    uint8_t request[48] = {0x12, 0x34, 0x4D, 0x0A, 0x00, 0x02, 0x00, 0x00}, answer[48];
    CHECK(omci_profile_answer(&p, request, sizeof(request), answer, sizeof(answer)) == 48);
    CHECK(answer[0] == 0x12 && answer[1] == 0x34 && answer[2] == 0x2D && answer[9] == 6 && answer[43] == 0x28);
    uint32_t mic = (uint32_t)answer[44] << 24 | answer[45] << 16 | answer[46] << 8 | answer[47];
    CHECK(mic == omci_crc32(answer, 44));
    request[7] = 1; // another instance
    CHECK(omci_profile_answer(&p, request, sizeof(request), answer, sizeof(answer)) == 0);
    request[7] = 0;
    CHECK(omci_profile_answer(&p, request, sizeof(request), answer, 40) == -1);
    omci_profile_free(&p);

    static const char *const bad[] = {
        "4d0a 2d0a\n",                                     // short bodies
        "4d0a00020000" "00" " 2d0a00020000" "00\n",        // baseline contents not 32 bytes
        "490a01000000 290a01000000 extra\n",               // text after the answer
        "4d0a00020000" "0000000000000000000000000000000000000000000000000000000000000000"
        " 080a00020000" "0000000000000000000000000000000000000000000000000000000000000000\n", // another type
    };
    for (unsigned i = 0; i < sizeof(bad) / sizeof(bad[0]); i++) {
        struct omci_profile q = {0};
        CHECK(load(&q, bad[i]) == -1);
        omci_profile_free(&q);
    }
    if (failures)
        fprintf(stderr, "%d check(s) failed\n", failures);
    else
        printf("test_omci: ok\n");
    return failures != 0;
}
