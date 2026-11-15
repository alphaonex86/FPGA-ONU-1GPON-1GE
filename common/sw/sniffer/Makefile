# SPDX-License-Identifier: CERN-OHL-S-2.0
# sniffer_regs.h and inline_regs.h come from the RTL build (out/rtl). Cross build for the HPS, without libusb (the
# in-line sniffer's SuperSpeed link): make CC=aarch64-linux-gnu-gcc LIBUSB=0
RTL ?= ../../../out/rtl
LIBUSB ?= 1
CFLAGS ?= -O2
CFLAGS += -std=gnu11 -Wall -Wextra -Werror -I$(RTL) -I../omci
LDLIBS = -lm
ifeq ($(LIBUSB),1)
CFLAGS += -DLIBUSB $(shell pkg-config --cflags libusb-1.0)
LDLIBS += $(shell pkg-config --libs libusb-1.0)
endif
REGS = $(RTL)/sniffer_regs.h $(RTL)/inline_regs.h
CODE = sniffer.c inline.c levels.c ../omci/omci.c
WIDE = wide.c
DEPS = $(CODE) wide.c sniffer.h inline.h levels.h wide.h $(REGS)

all: sniffer

$(REGS):
	@echo "$@ missing: generate the RTL first (sbt \"runMain pon.Generate\")" >&2; exit 1

sniffer: main.c $(DEPS)
	$(CC) $(CFLAGS) -o $@ main.c $(CODE) $(WIDE) $(LDLIBS)

TESTS = test_sniffer test_inline test_levels

test_inline: WIDE =   # it plays the SuperSpeed side itself

$(TESTS): %: %.c $(DEPS)
	$(CC) $(CFLAGS) -fsanitize=address,undefined -pthread -o $@ $< $(CODE) $(WIDE) $(LDLIBS)

test: $(TESTS)
	./test_sniffer
	./test_inline
	./test_levels

clean:
	rm -f sniffer $(TESTS)

.PHONY: all test clean
