// SPDX-License-Identifier: CERN-OHL-S-2.0
#ifndef INLINE_H
#define INLINE_H

#include <signal.h>

#include "sniffer.h"

// The in-line sniffer (InlineSniffer) on its serial port (FT2232H channel A): configures it, decodes its records
// and reports its state every second until *stopping; -1 on an error (logged).
int inline_run(struct sniffer *s, const char *port, const struct sniffer_options *opt, const volatile sig_atomic_t *stopping);

#endif
