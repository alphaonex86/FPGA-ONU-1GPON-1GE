// SPDX-License-Identifier: CERN-OHL-S-2.0
// HPS (Linux) front end: the sniffer window through /dev/mem, or a replayed dump.
#include "sniffer.h"

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <time.h>
#include <unistd.h>

#define LW_BRIDGE 0x20000000u // Agilex 5 HPS-to-FPGA lightweight bridge (to check on the board)
#define LW_ONU 0x101000u      // lwOnu (de25-standard/ghrd/qsys_top.tcl)
#define ONU_SNIFFER 0x900u    // PonBoard ONU side window
#define POLL_IDLE_NS 20000

static volatile sig_atomic_t stopping;

static void on_stop(int signal) { (void)signal; stopping = 1; }

static void on_bus_error(int signal)
{
    (void)signal;
    static const char message[] = "sniffer: bus error: the window answered an error (ONU lane down?)\n";
    if (write(STDERR_FILENO, message, sizeof(message) - 1) < 0)
        _exit(2);
    _exit(1);
}

static uint32_t mem_read(void *ctx, unsigned offset) { return ((volatile uint32_t *)ctx)[offset / 4]; }
static void mem_write(void *ctx, unsigned offset, uint32_t value) { ((volatile uint32_t *)ctx)[offset / 4] = value; }

static int parse_number(const char *text, unsigned long max, unsigned long *value)
{
    char *end;
    errno = 0;
    *value = strtoul(text, &end, 0);
    if (errno || end == text || *end || *value > max) {
        fprintf(stderr, "sniffer: bad number '%s' (max %lu)\n", text, max);
        return -1;
    }
    return 0;
}

// "HWTC12345678": 4 vendor characters, 8 hex digits
static int parse_serial(const char *text, uint8_t serial[8])
{
    if (strlen(text) != 12) {
        fprintf(stderr, "sniffer: serial number '%s' is not 4 characters + 8 hex digits\n", text);
        return -1;
    }
    memcpy(serial, text, 4);
    for (int i = 0; i < 4; i++) {
        char pair[3] = {text[4 + 2 * i], text[5 + 2 * i], 0};
        char *end;
        serial[4 + i] = (uint8_t)strtoul(pair, &end, 16);
        if (*end || end == pair + 1 || end == pair) {
            fprintf(stderr, "sniffer: serial number '%s' is not 4 characters + 8 hex digits\n", text);
            return -1;
        }
    }
    return 0;
}

static FILE *open_file(const char *path, const char *mode)
{
    FILE *file = fopen(path, mode);
    if (!file)
        fprintf(stderr, "sniffer: %s: %s\n", path, strerror(errno));
    return file;
}

static int replay(struct sniffer *s, FILE *in)
{
    static uint8_t bytes[1 << 20];
    size_t have = 0, got;
    while ((got = fread(bytes + have, 1, sizeof(bytes) - have, in)) > 0) {
        have += got;
        size_t at = 0;
        for (;;) {
            struct sniffer_record record;
            int used = sniffer_parse(bytes + at, have - at, &record);
            if (used < 0) {
                fprintf(stderr, "sniffer: no record at dump byte offset %zu\n", at);
                return -1;
            }
            if (used == 0)
                break;
            if (sniffer_handle(s, &record) < 0)
                return -1;
            at += (size_t)used;
        }
        memmove(bytes, bytes + at, have - at);
        have -= at;
    }
    if (ferror(in)) {
        perror("sniffer: replay");
        return -1;
    }
    if (have)
        fprintf(stderr, "sniffer: %zu bytes of a cut record at the dump's end\n", have);
    return 0;
}

static int live(struct sniffer *s, uint32_t bridge, const struct sniffer_options *opt)
{
    int fd = open("/dev/mem", O_RDWR | O_SYNC);
    if (fd < 0) {
        perror("sniffer: /dev/mem");
        return -1;
    }
    long page = sysconf(_SC_PAGESIZE);
    off_t base = (off_t)(bridge + LW_ONU) & ~(off_t)(page - 1);
    size_t skip = (size_t)(bridge + LW_ONU - base) + ONU_SNIFFER;
    uint8_t *map = mmap(NULL, skip + 0x100, PROT_READ | PROT_WRITE, MAP_SHARED, fd, base);
    if (close(fd) < 0)
        perror("sniffer: close /dev/mem");
    if (map == MAP_FAILED) {
        perror("sniffer: mmap");
        return -1;
    }
    struct sniffer_regs regs = {map + skip, mem_read, mem_write};
    struct sigaction bus = {.sa_handler = on_bus_error};
    if (sigaction(SIGBUS, &bus, NULL) < 0) {
        perror("sniffer: SIGBUS");
        return -1;
    }
    if (sniffer_start(s, &regs, opt) < 0)
        return -1;
    int result = 0;
    struct timespec idle = {0, POLL_IDLE_NS}, now, reported = {0, 0};
    while (!stopping) {
        int words = sniffer_poll(s);
        if (words < 0) {
            result = -1;
            break;
        }
        if (clock_gettime(CLOCK_MONOTONIC, &now) < 0) {
            perror("sniffer: clock");
            result = -1;
            break;
        }
        if (now.tv_sec != reported.tv_sec) {
            reported = now;
            sniffer_report(s);
        }
        if (words == 0 && nanosleep(&idle, NULL) < 0 && errno != EINTR) {
            perror("sniffer: nanosleep");
            result = -1;
            break;
        }
    }
    sniffer_stop(s);
    sniffer_report(s);
    if (munmap(map, skip + 0x100) < 0)
        perror("sniffer: munmap");
    return result;
}

static void usage(void)
{
    fprintf(stderr,
            "usage: sniffer [options]\n"
            "  -w FILE  raw records (keep the dumps out of the public repo)\n"
            "  -t FILE  decoded records (default stdout, '-' none)\n"
            "  -r FILE  decode a raw dump instead of the board\n"
            "  -b ADDR  HPS-to-FPGA lightweight bridge base (default 0x%08x)\n"
            "  -o ID    tapped ONU-ID (default: from its bursts)\n"
            "  -s SN    tapped ONU's serial number, e.g. HWTC1234abcd (its ONU-ID from Assign_ONU-ID)\n"
            "  -a ID    also keep this Alloc-ID's grants (repeatable)\n"
            "  -p ID    keep and decode this OMCI Port-ID (repeatable; else from Configure_Port-ID)\n"
            "  -A       every PLOAM, not only the ONU's and broadcast\n"
            "  -d HEX   upstream delimiter until an Upstream_Overhead (default ab5983)\n"
            "  -l N     burst reset lead before the StartTime, clocks (default 128)\n"
            "  -W N     burst reset width, clocks (default 16, 0 none)\n",
            LW_BRIDGE);
}

int main(int argc, char **argv)
{
    static struct sniffer s;
    static uint8_t serial[8];
    struct sniffer_options opt = {.onu_id = -1, .delimiter = 0xAB5983, .lead = 128, .width = 16, .text = stdout};
    const char *dump = NULL, *text = NULL, *in = NULL;
    unsigned long value, bridge = LW_BRIDGE;
    int c;
    while ((c = getopt(argc, argv, "w:t:r:b:o:s:a:p:Ad:l:W:h")) != -1) {
        switch (c) {
        case 'w': dump = optarg; break;
        case 't': text = optarg; break;
        case 'r': in = optarg; break;
        case 'b': if (parse_number(optarg, 0xFFFFFFFFul, &bridge) < 0) return 2; break;
        case 'o': if (parse_number(optarg, 254, &value) < 0) return 2; opt.onu_id = (int)value; break;
        case 's': if (parse_serial(optarg, serial) < 0) return 2; opt.serial = serial; break;
        case 'a':
        case 'p': {
            if (parse_number(optarg, 4095, &value) < 0)
                return 2;
            unsigned *count = c == 'a' ? &opt.alloc_count : &opt.port_count;
            if (*count == SNIFFER_MAX_IDS) {
                fprintf(stderr, "sniffer: at most %u -%c\n", SNIFFER_MAX_IDS, c);
                return 2;
            }
            (c == 'a' ? opt.allocs : opt.ports)[(*count)++] = (uint16_t)value;
            break;
        }
        case 'A': opt.all_ploams = 1; break;
        case 'd': {
            char *end;
            value = strtoul(optarg, &end, 16);
            if (*end || end == optarg || value > 0xFFFFFF) {
                fprintf(stderr, "sniffer: bad delimiter '%s'\n", optarg);
                return 2;
            }
            opt.delimiter = (uint32_t)value;
            break;
        }
        case 'l': if (parse_number(optarg, 0xFFFF, &value) < 0) return 2; opt.lead = (unsigned)value; break;
        case 'W': if (parse_number(optarg, 0xFFFF, &value) < 0) return 2; opt.width = (unsigned)value; break;
        default: usage(); return 2;
        }
    }
    if (optind != argc) {
        usage();
        return 2;
    }
    if (text && strcmp(text, "-") == 0)
        opt.text = NULL;
    else if (text && !(opt.text = open_file(text, "w")))
        return 1;
    if (dump && !(opt.dump = open_file(dump, "wb")))
        return 1;
    struct sigaction stop = {.sa_handler = on_stop};
    if (sigaction(SIGINT, &stop, NULL) < 0 || sigaction(SIGTERM, &stop, NULL) < 0) {
        perror("sniffer: signals");
        return 1;
    }
    int result;
    if (in) {
        FILE *file = open_file(in, "rb");
        if (!file)
            return 1;
        struct sniffer_regs none = {0};
        result = sniffer_start(&s, &none, &opt) < 0 ? -1 : replay(&s, file);
        sniffer_stop(&s);
        fclose(file);
    } else {
        result = live(&s, (uint32_t)bridge, &opt);
    }
    if (opt.dump && fclose(opt.dump) != 0) {
        perror("sniffer: dump");
        result = -1;
    }
    if (opt.text && opt.text != stdout && fclose(opt.text) != 0) {
        perror("sniffer: text");
        result = -1;
    }
    return result < 0 ? 1 : 0;
}
