// SPDX-License-Identifier: CERN-OHL-S-2.0
// Text profile (sniffer -P) to the binary one the ONU driver loads: omci_profile TEXT BINARY
#include "omci.h"

#include <errno.h>
#include <string.h>

int main(int argc, char **argv)
{
    if (argc != 3) {
        fprintf(stderr, "usage: omci_profile TEXT BINARY\n");
        return 2;
    }
    FILE *in = fopen(argv[1], "r");
    if (!in) {
        fprintf(stderr, "omci_profile: %s: %s\n", argv[1], strerror(errno));
        return 1;
    }
    struct omci_profile profile = {0};
    int loaded = omci_profile_load(&profile, in, argv[1]);
    fclose(in);
    if (loaded < 0)
        return 1;
    FILE *out = fopen(argv[2], "wb");
    if (!out) {
        fprintf(stderr, "omci_profile: %s: %s\n", argv[2], strerror(errno));
        omci_profile_free(&profile);
        return 1;
    }
    int written = omci_profile_write_binary(&profile, out);
    if (fclose(out) != 0) {
        perror("omci_profile: close");
        written = -1;
    }
    if (written >= 0)
        printf("%d baseline records of %u entries\n", written, profile.count);
    omci_profile_free(&profile);
    return written < 0;
}
