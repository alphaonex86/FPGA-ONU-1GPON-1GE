// SPDX-License-Identifier: CERN-OHL-S-2.0
// Host test of the level setting: the box's I2C register with models of its two DACs (one address, a clock each), its
// ADC, both receivers and both lasers.
#define _GNU_SOURCE
#include "levels.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

typedef uint32_t u32;
#include "inline_regs.h"

static int failures;

#define CHECK(cond)                                                       \
    do {                                                                  \
        if (!(cond)) {                                                    \
            fprintf(stderr, "%s:%d: CHECK(%s) failed\n", __FILE__, __LINE__, #cond); \
            failures++;                                                   \
        }                                                                 \
    } while (0)

struct bus {
    int scl, sda;                  // as the master drives them
    int active, bits, count, sending, turn, pull, index;
    unsigned byte, device;
    uint8_t in[3], out[2];
};

struct box {
    struct bus bus[2];
    int dac[2][4], written[2][4];
    unsigned command;              // the ADC's
    uint32_t lasers, rx, answers[64];
    unsigned asked, answered, cleared;
    int disorder;                  // bias before its set point, laser before the bias
    // what the test sets
    double in_dbm[2];              // light into the OLT port and the ONU port, NAN none
    double threshold_ma[2], mw_per_ma[2], md_mv_per_mw[2], rx_a_per_w[2];
    int missing_bus, stuck, leak_mv;
};

static double laser_mw(const struct box *b, unsigned p)
{
    static const double bias_ma_per_mv[2] = {88 / 5400.0, 88 / 3600.0};
    static const unsigned channel[2] = {1, 2};
    static const uint32_t enable[2] = {INLINE_LASERS_US_MASK, INLINE_LASERS_DS_MASK};
    double ma = b->dac[p][channel[p]] * bias_ma_per_mv[p] - b->threshold_ma[p] + b->dac[p][channel[p] + 1] * (88 / 3600.0) / 2;
    return (b->lasers & enable[p]) && !isnan(b->in_dbm[!p]) && ma > 0 ? ma * b->mw_per_ma[p] : 0;
}

static double apd_mv(const struct box *b, unsigned p)
{
    if (!(b->lasers & INLINE_LASERS_APD_MASK))
        return b->leak_mv;
    return isnan(b->in_dbm[p]) ? 0 : pow(10, b->in_dbm[p] / 10) * b->rx_a_per_w[p] * 1200;
}

static unsigned convert(const struct box *b)
{
    unsigned select = b->command >> 4 & 7, channel = (select & 3) << 1 | select >> 2;
    double mv = channel == 0 ? apd_mv(b, LEVELS_OLT) : channel == 1 ? laser_mw(b, LEVELS_OLT) * b->md_mv_per_mw[LEVELS_OLT] :
                channel == 3 ? laser_mw(b, LEVELS_ONU) * b->md_mv_per_mw[LEVELS_ONU] : 0;
    if ((b->command & 0x8C) != 0x8C) // single ended, reference on
        return 0xFFF;
    return mv >= 2500 ? 0xFFF : (unsigned)(mv * 4096 / 2500);
}

// A byte for the device addressed on clock n; returns its acknowledge.
static int received(struct box *b, unsigned n)
{
    struct bus *s = &b->bus[n];
    if (s->count++ == 0) {
        s->device = s->byte >> 1;
        if ((int)n == b->missing_bus || (s->device != 0x60 && !(s->device == 0x48 && n == 0)))
            return s->active = 0;
        if (s->byte & 1) {
            unsigned code = convert(b);
            s->out[0] = (uint8_t)(code >> 8);
            s->out[1] = (uint8_t)code;
            s->turn = 1;
        }
        return 1;
    }
    if (s->device == 0x48) {
        b->command = s->byte;
        return 1;
    }
    if (s->count - 2 < 3)
        s->in[s->count - 2] = (uint8_t)s->byte;
    if (s->count == 4 && (s->in[0] & 0xF9) == 0x40 && (s->in[1] & 0xF0) == 0x90) {
        unsigned channel = s->in[0] >> 1 & 3;
        b->dac[n][channel] = (s->in[1] & 0x0F) << 8 | s->in[2];
        b->written[n][channel] = 1;
    }
    return 1;
}

static void edge(struct box *b, unsigned n, int scl, int sda)
{
    struct bus *s = &b->bus[n];
    if (s->scl && scl && s->sda != sda) { // start (the data line falls) or stop
        s->active = !sda;
        s->bits = s->count = s->sending = s->turn = s->pull = 0;
    } else if (s->active && scl && !s->scl) {
        if (++s->bits <= 8 && !s->sending)
            s->byte = (s->byte << 1 | (unsigned)sda) & 0xFF;
        else if (s->bits == 9 && s->sending && sda)
            s->active = s->sending = 0; // not acknowledged: the last byte read
    } else if (s->active && !scl && s->scl) {
        if (s->bits == 9) {
            s->bits = s->pull = 0;
            s->index = s->sending ? s->index + 1 : 0;
            s->sending |= s->turn;
            s->turn = 0;
        } else if (s->bits == 8 && !s->sending) {
            s->pull = received(b, n);
        }
        if (s->sending)
            s->pull = s->bits < 8 && !(s->out[s->index & 1] >> (7 - s->bits) & 1);
    }
    s->scl = scl;
    s->sda = sda;
}

static uint32_t reg_value(struct box *b, unsigned offset)
{
    int lit = !isnan(b->in_dbm[LEVELS_OLT]), bursts = !isnan(b->in_dbm[LEVELS_ONU]) && !b->cleared ? 0 : 100;
    switch (offset) {
    case INLINE_I2C:
        return (uint32_t)(b->bus[0].sda && !b->bus[0].pull && !b->bus[1].pull && !b->stuck) << INLINE_I2C_SDAIN_SHIFT;
    case INLINE_STATUS:
        return lit ? INLINE_STATUS_DSSIGNAL_MASK | (b->lasers & INLINE_LASERS_DS_MASK ? INLINE_STATUS_DSON_MASK : 0) : 0;
    case INLINE_US_LEVEL:
        if (isnan(b->in_dbm[LEVELS_ONU]))
            return 0;
        return (uint32_t)bursts | (apd_mv(b, LEVELS_ONU) > b->dac[LEVELS_ONU][1] ? (uint32_t)bursts << INLINE_US_LEVEL_ABOVE_SHIFT : 0);
    }
    failures++;
    return 0;
}

static int box_write(void *ctx, unsigned offset, uint32_t value)
{
    struct box *b = ctx;
    if (offset == INLINE_I2C) {
        int sda = (value & INLINE_I2C_SDA_MASK) != 0;
        edge(b, 0, (value & INLINE_I2C_SCL_MASK) != 0, sda);
        edge(b, 1, (value & INLINE_I2C_SCL2_MASK) != 0, sda);
    } else if (offset == INLINE_LASERS) {
        if ((value & INLINE_LASERS_APD_MASK) && !(b->written[0][0] && b->written[1][0]))
            b->disorder = 1;
        if ((value & INLINE_LASERS_US_MASK) && !((value & INLINE_LASERS_APD_MASK) && b->written[0][1] && b->written[0][2]))
            b->disorder = 1;
        if ((value & INLINE_LASERS_DS_MASK) && !((value & INLINE_LASERS_APD_MASK) && b->written[1][2] && b->written[1][3]))
            b->disorder = 1;
        b->lasers = value;
    } else if (offset == INLINE_US_LEVEL) {
        b->cleared = 1;
    } else if (offset == INLINE_RX) {
        b->rx = value;
    } else {
        failures++;
    }
    return 0;
}

static int box_ask(void *ctx, unsigned offset)
{
    struct box *b = ctx;
    b->answers[b->asked++ % 64] = reg_value(b, offset);
    CHECK(b->asked - b->answered <= 64);
    return 0;
}

static int box_answer(void *ctx, unsigned offset, uint32_t *value)
{
    struct box *b = ctx;
    (void)offset;
    CHECK(b->answered < b->asked);
    *value = b->answers[b->answered++ % 64];
    return 0;
}

static const char UNIT[] =
    "# a unit's constants\n"
    "olt_apd_v = 40\nolt_rx_a_per_w = 8\n"
    "olt_tx_threshold_ma = 8\nolt_tx_mw_per_ma = 0.1\nolt_tx_max_dbm = 4\nolt_tx_start_dbm = 2\nolt_tx_md_mv_per_mw = 300\n"
    "onu_apd_v = 45   # behind the pad\nonu_rx_a_per_w = 0.08\nonu_rx_inverted = 1\n"
    "onu_tx_threshold_ma = 10\nonu_tx_mw_per_ma = 0.001\nonu_tx_max_dbm = -16\nonu_tx_start_dbm = -26\nonu_tx_md_mv_per_mw = 33000\n";

static int load(struct levels *lv, const char *text, const char *more)
{
    char path[] = "/tmp/test_levels_XXXXXX";
    int fd = mkstemp(path);
    FILE *file = fd >= 0 ? fdopen(fd, "w") : NULL;
    if (!file || fprintf(file, "%s%s", text, more) < 0 || fclose(file) != 0) {
        perror("test_levels: level file");
        exit(1);
    }
    int result = levels_load(lv, path);
    if (unlink(path) < 0)
        perror("test_levels: unlink");
    return result;
}

// A box whose lasers need 2 mA more than the unit's file says.
static void box_init(struct box *b, double olt_dbm, double onu_dbm)
{
    *b = (struct box){.in_dbm = {olt_dbm, onu_dbm}, .threshold_ma = {10, 12}, .mw_per_ma = {0.1, 0.001},
                      .md_mv_per_mw = {300, 33000}, .rx_a_per_w = {8, 0.08}, .missing_bus = -1};
    b->bus[0].scl = b->bus[0].sda = b->bus[1].scl = b->bus[1].sda = 1;
}

static int run(struct levels *lv, struct box *b, unsigned steps)
{
    const struct levels_link link = {b, box_write, box_ask, box_answer};
    int result = levels_start(lv, &link);
    for (unsigned i = 0; !result && i < steps; i++)
        result = levels_step(lv);
    CHECK(b->asked == b->answered && !b->disorder);
    return result;
}

static double sent_dbm(const struct box *b, unsigned p) { return 10 * log10(laser_mw(b, p)); }

int main(void)
{
    static struct levels lv;
    static struct box b;

    // each port sends what the other receives + 1 dB, the monitor diodes find the 2 mA
    CHECK(load(&lv, UNIT, "") == 0);
    box_init(&b, -27, 2);
    CHECK(run(&lv, &b, 40) == 0);
    CHECK(b.dac[0][0] == 1013 && b.dac[1][0] == 945); // 40 V and 45 V
    CHECK(b.rx == INLINE_RX_US_MASK);
    CHECK(b.lasers == (INLINE_LASERS_APD_MASK | INLINE_LASERS_US_MASK | INLINE_LASERS_DS_MASK));
    CHECK(fabs(sent_dbm(&b, LEVELS_ONU) - -26) < 0.3 && fabs(sent_dbm(&b, LEVELS_OLT) - 3) < 0.3);
    CHECK(abs(lv.burst_level_mv - 152) <= 4 && !lv.laser[0].limited && !lv.laser[1].limited);

    // the floor and the laser's maximum
    box_init(&b, -35, 5);
    CHECK(run(&lv, &b, 40) == 0);
    CHECK(fabs(sent_dbm(&b, LEVELS_ONU) - -26) < 0.3 && fabs(sent_dbm(&b, LEVELS_OLT) - 4) < 0.3);

    // no downstream: its laser stays dark, the upstream one waits at its start level
    box_init(&b, NAN, NAN);
    CHECK(run(&lv, &b, 3) == 0);
    CHECK(laser_mw(&b, LEVELS_ONU) == 0 && b.dac[0][1] > 0 && b.dac[0][2] > 0);

    // a DAC missing, the data line held, a current with the bias off: no bias, no laser
    for (int fault = 0; fault < 3; fault++) {
        box_init(&b, -27, 2);
        b.missing_bus = fault == 0 ? 1 : -1;
        b.stuck = fault == 1;
        b.leak_mv = fault == 2 ? 2000 : 0;
        CHECK(run(&lv, &b, 1) < 0 && b.lasers == 0);
    }

    // a shorted APD lead: all off
    box_init(&b, 3, 2);
    CHECK(run(&lv, &b, 1) < 0 && b.lasers == 0);

    // fixed currents (calibration): 20 mA and 30 mA, no light measured needed
    CHECK(load(&lv, "olt_apd_v = 40\nonu_apd_v = 45\nolt_tx_bias_ma = 20\nolt_tx_mod_ma = 30\n", "") == 0);
    box_init(&b, NAN, 2);
    CHECK(run(&lv, &b, 2) == 0);
    CHECK(b.dac[0][1] == 1227 && b.dac[0][2] == 1227 && b.lasers == (INLINE_LASERS_APD_MASK | INLINE_LASERS_US_MASK));
    CHECK(b.dac[1][2] == 0 && b.dac[1][3] == 0);

    CHECK(load(&lv, UNIT, "olt_tx_power = 3\n") < 0);         // not a setting
    CHECK(load(&lv, UNIT, "gain_db = 1 dB\n") < 0);
    CHECK(load(&lv, UNIT, "onu_apd_v = 70\n") < 0);           // over the supply's range
    CHECK(load(&lv, UNIT, "olt_rx_inverted = 2\n") < 0);
    CHECK(load(&lv, UNIT, "onu_tx_bias_ma = 20\n") < 0);      // regulated and fixed
    CHECK(load(&lv, "olt_apd_v = 40\nonu_apd_v = 45\nolt_tx_mw_per_ma = 0.1\n", "") < 0); // no threshold
    CHECK(load(&lv, "onu_apd_v = 45\n", "") < 0);
    if (failures)
        fprintf(stderr, "%d check(s) failed\n", failures);
    else
        printf("test_levels: ok\n");
    return failures != 0;
}
