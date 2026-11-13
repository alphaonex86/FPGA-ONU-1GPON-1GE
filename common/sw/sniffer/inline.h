// SPDX-License-Identifier: CERN-OHL-S-2.0
#ifndef INLINE_H
#define INLINE_H

#include <signal.h>

#include "sniffer.h"

// The in-line sniffer (InlineSniffer) on its USB serial port (CDC-ACM): configures it, decodes its records
// and reports its state every second until *stopping; -1 on an error (logged). level_file: the unit's optical
// constants (levels.h), the lasers start dark and follow the received levels; NULL: levels and lasers left as they are.
int inline_run(struct sniffer *s, const char *port, const char *level_file, const struct sniffer_options *opt,
               const volatile sig_atomic_t *stopping);

#endif
