// SPDX-License-Identifier: CERN-OHL-S-2.0
#ifndef WIDE_H
#define WIDE_H

#include <stddef.h>
#include <stdint.h>

// The in-line sniffer's SuperSpeed side (USB 1209:0002, bulk IN endpoint 1): its records at the line's rate.
struct wide;
typedef void (*wide_sink)(void *ctx, const uint8_t *bytes, size_t size);

// NULL when it cannot be used (said on stderr).
struct wide *wide_open(wide_sink sink, void *ctx);
// Waits until fd is readable, ms at most; the bytes received meanwhile go to the sink. w NULL: fd alone; fd -1: w alone.
// 1: fd is readable, 0: not yet, -1: error (errno; EINTR at a signal, EIO when the link failed: logged).
int wide_poll(struct wide *w, int fd, int ms);
void wide_close(struct wide *w);

#endif
