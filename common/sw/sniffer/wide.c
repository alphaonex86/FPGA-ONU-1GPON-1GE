// SPDX-License-Identifier: CERN-OHL-S-2.0
// Several transfers stay queued on the bulk endpoint, so the host controller never stops asking between two of them.
#include "wide.h"

#include <errno.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef LIBUSB
#include <libusb.h>

#define VENDOR 0x1209
#define PRODUCT 0x0002
#define ENDPOINT 0x81
#define TRANSFERS 4
#define TRANSFER_BYTES (1 << 18)
#define MAX_FDS 8
#define CLOSE_MS 1000

struct wide {
    libusb_context *usb;
    libusb_device_handle *device;
    struct libusb_transfer *transfers[TRANSFERS];
    int queued, closing, failed;
    wide_sink sink;
    void *ctx;
};

static void submit(struct wide *w, struct libusb_transfer *t)
{
    int error = libusb_submit_transfer(t);
    if (error) {
        fprintf(stderr, "sniffer: SuperSpeed link: %s\n", libusb_error_name(error));
        w->failed = 1;
    } else {
        w->queued++;
    }
}

static void LIBUSB_CALL received(struct libusb_transfer *t)
{
    struct wide *w = t->user_data;
    w->queued--;
    if (t->actual_length > 0)
        w->sink(w->ctx, t->buffer, (size_t)t->actual_length);
    if (w->closing || w->failed)
        return;
    if (t->status != LIBUSB_TRANSFER_COMPLETED) {
        fprintf(stderr, "sniffer: SuperSpeed link: transfer ended with status %d\n", t->status);
        w->failed = 1;
        return;
    }
    submit(w, t);
}

static void handle(struct wide *w)
{
    struct timeval none = {0, 0};
    int error = libusb_handle_events_timeout_completed(w->usb, &none, NULL);
    if (error) {
        fprintf(stderr, "sniffer: SuperSpeed link: %s\n", libusb_error_name(error));
        w->failed = 1;
    }
}

struct wide *wide_open(wide_sink sink, void *ctx)
{
    struct wide *w = calloc(1, sizeof(*w));
    if (!w) {
        perror("sniffer: SuperSpeed link");
        return NULL;
    }
    w->sink = sink;
    w->ctx = ctx;
    int error = libusb_init(&w->usb);
    if (error) {
        fprintf(stderr, "sniffer: libusb: %s\n", libusb_error_name(error));
        free(w);
        return NULL;
    }
    w->device = libusb_open_device_with_vid_pid(w->usb, VENDOR, PRODUCT);
    if (!w->device)
        fprintf(stderr, "sniffer: USB %04x:%04x not found, or no right to open it\n", VENDOR, PRODUCT);
    else if ((error = libusb_claim_interface(w->device, 0)) != 0)
        fprintf(stderr, "sniffer: SuperSpeed link: %s\n", libusb_error_name(error));
    for (int i = 0; w->device && !error && !w->failed && i < TRANSFERS; i++) {
        struct libusb_transfer *t = w->transfers[i] = libusb_alloc_transfer(0);
        uint8_t *buffer = t ? malloc(TRANSFER_BYTES) : NULL;
        if (!buffer) {
            fprintf(stderr, "sniffer: SuperSpeed link: no memory\n");
            w->failed = 1;
            break;
        }
        libusb_fill_bulk_transfer(t, w->device, ENDPOINT, buffer, TRANSFER_BYTES, received, w, 0);
        t->flags = LIBUSB_TRANSFER_FREE_BUFFER;
        submit(w, t);
    }
    if (w->device && !error && !w->failed)
        return w;
    wide_close(w);
    return NULL;
}

int wide_poll(struct wide *w, int fd, int ms)
{
    struct pollfd fds[1 + MAX_FDS] = {{fd, POLLIN, 0}};
    nfds_t count = 1;
    if (w) {
        const struct libusb_pollfd **list = libusb_get_pollfds(w->usb);
        if (!list) {
            errno = ENOMEM;
            return -1;
        }
        for (int i = 0; list[i] && count < 1 + MAX_FDS; i++)
            fds[count++] = (struct pollfd){list[i]->fd, list[i]->events, 0};
        libusb_free_pollfds(list);
    }
    int ready = poll(fds, count, ms);
    if (ready <= 0)
        return ready;
    if (w) {
        handle(w);
        if (w->failed) {
            errno = EIO;
            return -1;
        }
    }
    return fds[0].revents != 0;
}

void wide_close(struct wide *w)
{
    if (!w)
        return;
    w->closing = 1;
    for (int i = 0; i < TRANSFERS; i++)
        if (w->transfers[i])
            libusb_cancel_transfer(w->transfers[i]); // an error: it is not queued
    for (int waited = 0; w->queued && !w->failed && waited < CLOSE_MS; waited += 10) {
        struct timeval step = {0, 10000};
        if (libusb_handle_events_timeout_completed(w->usb, &step, NULL))
            break;
    }
    if (w->queued) {
        fprintf(stderr, "sniffer: SuperSpeed link: %d transfer(s) not given back\n", w->queued);
    } else {
        for (int i = 0; i < TRANSFERS; i++)
            if (w->transfers[i])
                libusb_free_transfer(w->transfers[i]);
    }
    if (w->device) {
        libusb_release_interface(w->device, 0); // closing releases it anyway
        libusb_close(w->device);
    }
    libusb_exit(w->usb);
    free(w);
}

#else

struct wide *wide_open(wide_sink sink, void *ctx)
{
    (void)sink;
    (void)ctx;
    fprintf(stderr, "sniffer: built without libusb\n");
    return NULL;
}

int wide_poll(struct wide *w, int fd, int ms)
{
    (void)w;
    struct pollfd wait = {fd, POLLIN, 0};
    return poll(&wait, 1, ms);
}

void wide_close(struct wide *w) { (void)w; }

#endif
