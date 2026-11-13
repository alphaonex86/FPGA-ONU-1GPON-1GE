// SPDX-License-Identifier: CERN-OHL-S-2.0
#ifndef LEVELS_H
#define LEVELS_H

#include <stdint.h>

// Optical levels of the in-line sniffer: APD bias, the power received at each port, each laser sent at what the
// other port receives + gain_db. The unit's constants come from its level file (levels.example of the board's
// repository), measured once with a power meter at the two connectors.

enum { LEVELS_OLT, LEVELS_ONU }; // the port facing the OLT, the port facing the ONU

struct levels_link { // the device's registers; -1 on an error (logged)
    void *ctx;
    int (*write)(void *ctx, unsigned offset, uint32_t value);
    int (*ask)(void *ctx, unsigned offset);                     // a read; several may wait
    int (*answer)(void *ctx, unsigned offset, uint32_t *value); // of the oldest read not answered yet
};

struct levels_port { // a connector's receiver and laser; NAN: not in the file
    double apd_v, rx_a_per_w;
    double tx_threshold_ma, tx_mw_per_ma, tx_max_dbm, tx_start_dbm, tx_md_mv_per_mw; // the regulated laser
    double tx_bias_ma, tx_mod_ma;                                                    // or fixed currents, to calibrate
};

struct levels_laser {
    int bias_mv, mod_mv; // DAC outputs
    double sent_mw;      // aimed at, NAN with fixed currents
    double trim_ma;      // what the monitor diode added to the file's threshold
    int monitor_mv, limited;
    double shown_dbm;
};

struct levels {
    struct levels_link link;
    double gain_db, floor_dbm, extinction_db;
    struct levels_port port[2];
    struct levels_laser laser[2];
    double shown_dbm[2];                             // received, as last printed
    unsigned bus, asked;                             // I2C transfer: its clock, the reads waiting
    int burst_mv, burst_step, burst_up, burst_run, burst_written; // comparator level search
    int burst_level_mv;                              // where it last settled, 0 not yet
    uint32_t lasers;                                 // LASERS as written
};

// Reads the unit's level file; -1 on an error (logged).
int levels_load(struct levels *lv, const char *path);
// Lasers dark, set points written, APD bias on; -1 on an error (logged), the lasers left dark.
int levels_start(struct levels *lv, const struct levels_link *link);
// Once a second: measures both ports, moves both lasers, prints what changed; -1 on an error (logged).
int levels_step(struct levels *lv);

#endif
