// SPDX-License-Identifier: CERN-OHL-S-2.0
// Optical levels of the in-line sniffer (its board: common/pcb/gen/boards.py inline()): two MCP4728 DACs and an
// ADS7828 ADC on the I2C lines of the FPGA's I2C register, one data line, a clock per DAC (both answer at 0x60).
#include "levels.h"

#include <errno.h>
#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

typedef uint32_t u32;
#include "inline_regs.h"

#define DAC 0x60
#define ADC 0x48
#define DAC_MAX_MV 2500         // internal reference x 2: 1 mV per code; MAX3643 set points and TPS61391 ADJ end there
#define REFERENCE_MS 100        // ADC reference on its 2.2 uF
#define MON_MV_PER_MA 1200.0    // APD current: 4:5 mirror into 1.5k
#define MON_OFF_MV 120          // bias off: more is a current that is not the APD's
#define MON_SHORT_MV 2400       // 2 mA
#define MOD_MA_PER_MV (88 / 3600.0) // MAX3643: 88 x (DAC / 2) / 1.8k
#define TRIM_GAIN 0.5
#define STEP_MIN_MV 4
#define STEP_MAX_MV 256
#define SHOWN_DB 0.5

enum { DS_APD_MON, US_LD_MD, US_LD_BCMON, DS_LD_MD, DS_LD_BCMON }; // ADC channels
#define RSSI_REF 1 // the ONU port's DAC: the burst comparator's level

static const struct {
    const char *name;
    unsigned adj, bias, mod, monitor; // DAC channels on the port's clock, ADC channel
    double apd_top_v, apd_v_per_v;    // TPS61391: bias at ADJ = 0 V, lost per volt
    double bias_ma_per_mv;
    uint32_t laser;
} BOARD[2] = {
    {"OLT", 0, 1, 2, US_LD_MD, 55.2, 15.0, 88 / 5400.0, INLINE_LASERS_US_MASK},
    {"ONU", 0, 2, 3, DS_LD_MD, 60.6, 16.5, 88 / 3600.0, INLINE_LASERS_DS_MASK},
};

static const struct { const char *name; size_t offset; } PORT_KEYS[] = {
    {"apd_v", offsetof(struct levels_port, apd_v)},
    {"rx_a_per_w", offsetof(struct levels_port, rx_a_per_w)},
    {"rx_inverted", offsetof(struct levels_port, rx_inverted)},
    {"tx_threshold_ma", offsetof(struct levels_port, tx_threshold_ma)},
    {"tx_mw_per_ma", offsetof(struct levels_port, tx_mw_per_ma)},
    {"tx_max_dbm", offsetof(struct levels_port, tx_max_dbm)},
    {"tx_start_dbm", offsetof(struct levels_port, tx_start_dbm)},
    {"tx_md_mv_per_mw", offsetof(struct levels_port, tx_md_mv_per_mw)},
    {"tx_bias_ma", offsetof(struct levels_port, tx_bias_ma)},
    {"tx_mod_ma", offsetof(struct levels_port, tx_mod_ma)},
};
#define PORT_KEY_COUNT (sizeof(PORT_KEYS) / sizeof(PORT_KEYS[0]))

static double *port_value(struct levels_port *port, unsigned key) { return (double *)((char *)port + PORT_KEYS[key].offset); }

static double *find(struct levels *lv, const char *key)
{
    if (strcmp(key, "gain_db") == 0)
        return &lv->gain_db;
    if (strcmp(key, "floor_dbm") == 0)
        return &lv->floor_dbm;
    if (strcmp(key, "extinction_db") == 0)
        return &lv->extinction_db;
    int port = strncmp(key, "olt_", 4) == 0 ? LEVELS_OLT : strncmp(key, "onu_", 4) == 0 ? LEVELS_ONU : -1;
    for (unsigned i = 0; port >= 0 && i < PORT_KEY_COUNT; i++)
        if (strcmp(key + 4, PORT_KEYS[i].name) == 0)
            return port_value(&lv->port[port], i);
    return NULL;
}

static int regulated(const struct levels_port *port) { return !isnan(port->tx_mw_per_ma); }
static int fixed(const struct levels_port *port) { return !isnan(port->tx_bias_ma) || !isnan(port->tx_mod_ma); }

static int check(const struct levels *lv, const char *path)
{
    const char *wrong = lv->extinction_db > 0 ? NULL : "extinction_db must be over 0";
    for (unsigned p = 0; p < 2 && !wrong; p++) {
        const struct levels_port *port = &lv->port[p];
        double lowest = BOARD[p].apd_top_v - BOARD[p].apd_v_per_v * DAC_MAX_MV / 1000;
        if (!(port->apd_v >= lowest && port->apd_v <= BOARD[p].apd_top_v)) {
            fprintf(stderr, "sniffer: %s: %s_apd_v must be %.1f to %.1f V\n", path, p ? "onu" : "olt", lowest, BOARD[p].apd_top_v);
            return -1;
        }
        if (port->rx_a_per_w <= 0)
            wrong = "rx_a_per_w must be over 0";
        else if (!isnan(port->rx_inverted) && port->rx_inverted != 0 && port->rx_inverted != 1)
            wrong = "rx_inverted is 0 or 1";
        else if (regulated(port) && fixed(port))
            wrong = "tx_mw_per_ma (regulated) with tx_bias_ma / tx_mod_ma (fixed currents)";
        else if (regulated(port) && !(port->tx_mw_per_ma > 0 && port->tx_threshold_ma >= 0 && !isnan(port->tx_max_dbm) &&
                                      !isnan(port->tx_start_dbm) && !(port->tx_md_mv_per_mw <= 0)))
            wrong = "a regulated laser needs tx_mw_per_ma > 0, tx_threshold_ma, tx_max_dbm, tx_start_dbm (tx_md_mv_per_mw > 0)";
        else if (fixed(port) && !(port->tx_bias_ma >= 0 && port->tx_mod_ma >= 0))
            wrong = "fixed currents need tx_bias_ma and tx_mod_ma";
        else if (!regulated(port) && !fixed(port))
            fprintf(stderr, "sniffer: %s: no %s_tx_ setting: the %s port's laser stays dark\n", path, p ? "onu" : "olt", BOARD[p].name);
        if (wrong) {
            fprintf(stderr, "sniffer: %s: %s port: %s\n", path, BOARD[p].name, wrong);
            return -1;
        }
    }
    if (wrong)
        fprintf(stderr, "sniffer: %s: %s\n", path, wrong);
    return wrong ? -1 : 0;
}

int levels_load(struct levels *lv, const char *path)
{
    memset(lv, 0, sizeof(*lv));
    lv->gain_db = 1;
    lv->floor_dbm = -26;
    lv->extinction_db = 10;
    for (unsigned p = 0; p < 2; p++)
        for (unsigned i = 0; i < PORT_KEY_COUNT; i++)
            *port_value(&lv->port[p], i) = NAN;
    FILE *file = fopen(path, "r");
    if (!file) {
        fprintf(stderr, "sniffer: %s: %s\n", path, strerror(errno));
        return -1;
    }
    char line[256], key[64], extra;
    int bad = 0;
    for (unsigned number = 1; fgets(line, sizeof(line), file); number++) {
        double value;
        line[strcspn(line, "#\n")] = 0;
        if (!line[strspn(line, " \t\r")])
            continue;
        double *field = sscanf(line, " %63[a-z_] = %lf %c", key, &value, &extra) == 2 && isfinite(value) ? find(lv, key) : NULL;
        if (field) {
            *field = value;
        } else {
            fprintf(stderr, "sniffer: %s:%u: not a level setting: %s\n", path, number, line);
            bad = 1;
        }
    }
    if (ferror(file)) {
        fprintf(stderr, "sniffer: %s: %s\n", path, strerror(errno));
        bad = 1;
    }
    if (fclose(file) != 0) {
        fprintf(stderr, "sniffer: %s: %s\n", path, strerror(errno));
        bad = 1;
    }
    return bad ? -1 : check(lv, path);
}

static int reg_write(struct levels *lv, unsigned offset, uint32_t value) { return lv->link.write(lv->link.ctx, offset, value); }

static int reg_read(struct levels *lv, unsigned offset, uint32_t *value)
{
    return lv->link.ask(lv->link.ctx, offset) < 0 ? -1 : lv->link.answer(lv->link.ctx, offset, value);
}

// The lines of the transfer's bus. The other clock stays low: its DAC sees no start.
static int lines(struct levels *lv, unsigned scl, unsigned sda)
{
    uint32_t clock = lv->bus ? INLINE_I2C_SCL2_MASK : INLINE_I2C_SCL_MASK;
    return reg_write(lv, INLINE_I2C, (scl ? clock : 0) | (sda ? INLINE_I2C_SDA_MASK : 0));
}

static int bit_out(struct levels *lv, unsigned bit)
{
    return lines(lv, 0, bit) < 0 || lines(lv, 1, bit) < 0 || lines(lv, 0, bit) < 0 ? -1 : 0;
}

// The data line read while the clock is high; the answer waits in the link.
static int bit_in(struct levels *lv)
{
    if (lines(lv, 0, 1) < 0 || lines(lv, 1, 1) < 0 || lv->link.ask(lv->link.ctx, INLINE_I2C) < 0 || lines(lv, 0, 1) < 0)
        return -1;
    lv->asked++;
    return 0;
}

static int byte_out(struct levels *lv, unsigned byte)
{
    for (int i = 7; i >= 0; i--)
        if (bit_out(lv, byte >> i & 1) < 0)
            return -1;
    return bit_in(lv); // its acknowledge
}

static int start_out(struct levels *lv, unsigned byte)
{
    return lines(lv, 0, 1) < 0 || lines(lv, 1, 1) < 0 || lines(lv, 1, 0) < 0 || lines(lv, 0, 0) < 0 ? -1 : byte_out(lv, byte);
}

// One transfer on the clock `bus`: `outs` bytes written, then `ins` read. The whole transfer is sent, then its
// answers are taken: the acknowledges, the bits read, the released data line.
static int transfer(struct levels *lv, unsigned bus, unsigned address, const uint8_t *out, unsigned outs, uint8_t *in, unsigned ins)
{
    const uint32_t released = INLINE_I2C_SCL_MASK | INLINE_I2C_SCL2_MASK | INLINE_I2C_SDA_MASK;
    lv->bus = bus;
    lv->asked = 0;
    int sent = start_out(lv, address << 1);
    for (unsigned i = 0; !sent && i < outs; i++)
        sent = byte_out(lv, out[i]);
    if (!sent && ins)
        sent = start_out(lv, address << 1 | 1);
    for (unsigned i = 0; !sent && i < ins; i++) {
        for (unsigned bit = 0; !sent && bit < 8; bit++)
            sent = bit_in(lv);
        if (!sent)
            sent = bit_out(lv, i + 1 == ins); // acknowledged, but the last
    }
    if (sent || lines(lv, 0, 0) < 0 || lines(lv, 1, 0) < 0 || lines(lv, 1, 1) < 0 || reg_write(lv, INLINE_I2C, released) < 0 ||
        lv->link.ask(lv->link.ctx, INLINE_I2C) < 0)
        return -1;
    unsigned acks = 1 + outs + (ins ? 1 : 0), refused = 0, idle = 0;
    if (ins)
        memset(in, 0, ins);
    for (unsigned i = 0; i <= lv->asked; i++) {
        uint32_t value;
        if (lv->link.answer(lv->link.ctx, INLINE_I2C, &value) < 0)
            return -1;
        unsigned bit = value >> INLINE_I2C_SDAIN_SHIFT & 1;
        if (i < acks)
            refused |= bit;
        else if (i < lv->asked)
            in[(i - acks) / 8] = (uint8_t)(in[(i - acks) / 8] << 1 | bit);
        else
            idle = bit;
    }
    if (!idle)
        fprintf(stderr, "sniffer: I2C data line held low\n");
    else if (refused)
        fprintf(stderr, "sniffer: no I2C device 0x%02x on the %s port's clock\n", address, BOARD[bus].name);
    return idle && !refused ? 0 : -1;
}

// Multi-write: the output now, not its EEPROM; internal reference, gain 2.
static int dac(struct levels *lv, unsigned bus, unsigned channel, int mv)
{
    const uint8_t bytes[3] = {(uint8_t)(0x40 | channel << 1), (uint8_t)(0x90 | mv >> 8), (uint8_t)mv};
    return transfer(lv, bus, DAC, bytes, sizeof(bytes), NULL, 0);
}

// Single ended; the internal 2.5 V reference and the converter stay on.
static int adc(struct levels *lv, unsigned channel, int *mv)
{
    const uint8_t command = (uint8_t)(0x80 | (channel >> 1 | (channel & 1) << 2) << 4 | 0x0C);
    uint8_t code[2];
    if (transfer(lv, LEVELS_OLT, ADC, &command, 1, code, sizeof(code)) < 0)
        return -1;
    *mv = ((code[0] & 0x0F) << 8 | code[1]) * 2500 / 4096;
    return 0;
}

static int adc_median(struct levels *lv, unsigned channel, int *mv)
{
    int m[3];
    for (unsigned i = 0; i < 3; i++)
        if (adc(lv, channel, &m[i]) < 0)
            return -1;
    int low = m[0] < m[1] ? m[0] : m[1], high = m[0] < m[1] ? m[1] : m[0];
    *mv = m[2] < low ? low : m[2] > high ? high : m[2];
    return 0;
}

int levels_start(struct levels *lv, const struct levels_link *link)
{
    const struct timespec settle = {0, REFERENCE_MS * 1000000L};
    int mv;
    lv->link = *link;
    lv->burst_mv = lv->burst_step = STEP_MAX_MV;
    lv->burst_up = lv->burst_run = lv->burst_level_mv = 0;
    lv->shown_dbm[0] = lv->shown_dbm[1] = NAN;
    const uint32_t inverted = (lv->port[LEVELS_OLT].rx_inverted == 1 ? INLINE_RX_DS_MASK : 0) |
                              (lv->port[LEVELS_ONU].rx_inverted == 1 ? INLINE_RX_US_MASK : 0);
    if (reg_write(lv, INLINE_LASERS, 0) < 0 || reg_write(lv, INLINE_RX, inverted) < 0 ||
        adc(lv, DS_APD_MON, &mv) < 0) // a conversion: reference on
        return -1;
    for (unsigned p = 0; p < 2; p++) {
        lv->laser[p] = (struct levels_laser){.sent_mw = NAN, .shown_dbm = NAN};
        int adj = (int)lround((BOARD[p].apd_top_v - lv->port[p].apd_v) / BOARD[p].apd_v_per_v * 1000);
        if (dac(lv, p, BOARD[p].adj, adj) < 0 || dac(lv, p, BOARD[p].bias, 0) < 0 || dac(lv, p, BOARD[p].mod, 0) < 0)
            return -1;
    }
    if (dac(lv, LEVELS_ONU, RSSI_REF, lv->burst_mv) < 0)
        return -1;
    lv->burst_written = lv->burst_mv;
    if (nanosleep(&settle, NULL) < 0 && errno != EINTR) {
        perror("sniffer: nanosleep");
        return -1;
    }
    if (adc(lv, DS_APD_MON, &mv) < 0)
        return -1;
    if (mv > MON_OFF_MV) {
        fprintf(stderr, "sniffer: OLT port: %.2f mA on the APD lead with its bias off: bias left off (VCC and APD leads exchanged?)\n",
                mv / MON_MV_PER_MA);
        return -1;
    }
    lv->lasers = INLINE_LASERS_APD_MASK;
    return reg_write(lv, INLINE_LASERS, lv->lasers);
}

// The comparator's level follows the bursts: halved step at each turn, doubled at the third move the same way.
// A turn at the smallest step is the bursts' level; the laser keeps the last one while the search runs again.
static void search(struct levels *lv, int up)
{
    if (up != lv->burst_up) {
        if (lv->burst_step == STEP_MIN_MV)
            lv->burst_level_mv = lv->burst_mv;
        lv->burst_step = lv->burst_step > STEP_MIN_MV ? lv->burst_step / 2 : STEP_MIN_MV;
        lv->burst_run = 0;
    } else if (++lv->burst_run >= 2 && lv->burst_step < STEP_MAX_MV) {
        lv->burst_step *= 2;
    }
    lv->burst_up = up;
    lv->burst_mv += up ? lv->burst_step : -lv->burst_step;
    lv->burst_mv = lv->burst_mv < 0 ? 0 : lv->burst_mv > DAC_MAX_MV ? DAC_MAX_MV : lv->burst_mv;
}

static int set_point(struct levels *lv, unsigned p, unsigned channel, double ma, double ma_per_mv, int *written, int *limited)
{
    long mv = lround(ma / ma_per_mv);
    if (mv < 0 || mv > DAC_MAX_MV) {
        *limited = 1;
        mv = mv < 0 ? 0 : DAC_MAX_MV;
    }
    if (mv == *written)
        return 0;
    *written = (int)mv;
    return dac(lv, p, channel, (int)mv);
}

// The laser of port `p`: trimmed by its monitor diode while `lit`, then aimed at what the other port receives + gain.
static int regulate(struct levels *lv, unsigned p, double received_mw, int lit)
{
    const struct levels_port *port = &lv->port[p];
    struct levels_laser *l = &lv->laser[p];
    double bias_ma = port->tx_bias_ma, mod_ma = port->tx_mod_ma;
    int limited = 0;
    if (!regulated(port) && !fixed(port))
        return 0;
    if (lit && (lv->lasers & BOARD[p].laser)) {
        if (adc_median(lv, BOARD[p].monitor, &l->monitor_mv) < 0)
            return -1;
        if (regulated(port) && !isnan(port->tx_md_mv_per_mw)) {
            double missing_mw = l->sent_mw - l->monitor_mv / port->tx_md_mv_per_mw;
            l->trim_ma += TRIM_GAIN * missing_mw / port->tx_mw_per_ma;
            if (fabs(l->trim_ma) > port->tx_threshold_ma) {
                l->trim_ma = copysign(port->tx_threshold_ma, l->trim_ma);
                limited = 1;
            }
        }
    }
    if (regulated(port)) {
        double dbm = isnan(received_mw) ? port->tx_start_dbm : 10 * log10(received_mw) + lv->gain_db;
        dbm = fmin(fmax(dbm, lv->floor_dbm), port->tx_max_dbm);
        // zeros at the threshold + what the extinction ratio leaves them, ones on the modulation
        double ratio = pow(10, lv->extinction_db / 10);
        l->sent_mw = pow(10, dbm / 10);
        double zero_ma = 2 * l->sent_mw / (1 + ratio) / port->tx_mw_per_ma;
        mod_ma = (ratio - 1) * zero_ma;
        bias_ma = port->tx_threshold_ma + zero_ma + l->trim_ma;
    }
    if (set_point(lv, p, BOARD[p].bias, bias_ma, BOARD[p].bias_ma_per_mv, &l->bias_mv, &limited) < 0 ||
        set_point(lv, p, BOARD[p].mod, mod_ma, MOD_MA_PER_MV, &l->mod_mv, &limited) < 0)
        return -1;
    if (limited != l->limited)
        fprintf(stderr, "sniffer: %s port laser %s\n", BOARD[p].name,
                limited ? "at a limit (set point range, or the monitor diode asks over twice the threshold)" : "back in its range");
    l->limited = limited;
    lv->lasers |= BOARD[p].laser;
    return 0;
}

static int moved(double *shown, double now)
{
    if (isnan(now) == isnan(*shown) && !(fabs(now - *shown) >= SHOWN_DB))
        return 0;
    *shown = now;
    return 1;
}

// A port's line when a level moved; every time while the port is calibrated (a constant missing: the raw readings).
static void show(struct levels *lv, unsigned p, double received_mw, int apd_mv)
{
    const struct levels_port *port = &lv->port[p];
    struct levels_laser *l = &lv->laser[p];
    double received_dbm = 10 * log10(received_mw), sent_dbm = 10 * log10(l->sent_mw);
    int calibrating = isnan(port->rx_a_per_w) || fixed(port);
    if (!(moved(&lv->shown_dbm[p], received_dbm) | moved(&l->shown_dbm, sent_dbm)) && !calibrating)
        return;
    char received[32] = "?", sent[32] = "?";
    if (!isnan(received_dbm))
        snprintf(received, sizeof(received), "%+.1f dBm", received_dbm);
    if (!isnan(sent_dbm))
        snprintf(sent, sizeof(sent), "%+.1f dBm", sent_dbm);
    fprintf(stderr, "sniffer: %s port: receives %s (APD %d mV)", BOARD[p].name, received, apd_mv);
    if (lv->lasers & BOARD[p].laser)
        fprintf(stderr, ", sends %s (bias %.1f mA, modulation %.1f mA, monitor %d mV)", sent, l->bias_mv * BOARD[p].bias_ma_per_mv,
                l->mod_mv * MOD_MA_PER_MV, l->monitor_mv);
    fputc('\n', stderr);
}

int levels_step(struct levels *lv)
{
    uint32_t status, level, lasers = lv->lasers;
    int apd_mv;
    if (reg_read(lv, INLINE_STATUS, &status) < 0 || reg_read(lv, INLINE_US_LEVEL, &level) < 0 || adc(lv, DS_APD_MON, &apd_mv) < 0)
        return -1;
    if (apd_mv >= MON_SHORT_MV) {
        fprintf(stderr, "sniffer: OLT port: APD current over %.0f mA: bias and lasers off (olt_apd_v over the breakdown, or VCC and "
                "APD leads exchanged)\n", MON_SHORT_MV / MON_MV_PER_MA);
        if (reg_write(lv, INLINE_LASERS, 0) == 0)
            lv->lasers = 0;
        return -1;
    }
    unsigned bursts = (level & INLINE_US_LEVEL_BURSTS_MASK) >> INLINE_US_LEVEL_BURSTS_SHIFT;
    unsigned above = (level & INLINE_US_LEVEL_ABOVE_MASK) >> INLINE_US_LEVEL_ABOVE_SHIFT;
    if (bursts)
        search(lv, 2 * above >= bursts);
    if (lv->burst_mv != lv->burst_written && dac(lv, LEVELS_ONU, RSSI_REF, lv->burst_mv) < 0)
        return -1;
    lv->burst_written = lv->burst_mv;
    const double received_mw[2] = {
        status & INLINE_STATUS_DSSIGNAL_MASK ? apd_mv / MON_MV_PER_MA / lv->port[LEVELS_OLT].rx_a_per_w : NAN,
        lv->burst_level_mv ? lv->burst_level_mv / MON_MV_PER_MA / lv->port[LEVELS_ONU].rx_a_per_w : NAN,
    };
    if (regulate(lv, LEVELS_OLT, received_mw[LEVELS_ONU], bursts != 0) < 0 ||
        regulate(lv, LEVELS_ONU, received_mw[LEVELS_OLT], (status & INLINE_STATUS_DSON_MASK) != 0) < 0)
        return -1;
    if (lv->lasers != lasers && reg_write(lv, INLINE_LASERS, lv->lasers) < 0)
        return -1;
    // the bursts from here on are compared with the level just written
    if (reg_write(lv, INLINE_US_LEVEL, 0) < 0)
        return -1;
    show(lv, LEVELS_OLT, received_mw[LEVELS_OLT], apd_mv);
    show(lv, LEVELS_ONU, received_mw[LEVELS_ONU], lv->burst_level_mv);
    return 0;
}
