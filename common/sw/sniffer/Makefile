# SPDX-License-Identifier: CERN-OHL-S-2.0
# sniffer_regs.h and inline_regs.h come from the RTL build (out/rtl). Cross build for the HPS: make CC=aarch64-linux-gnu-gcc
RTL ?= ../../../out/rtl
CFLAGS ?= -O2
CFLAGS += -std=gnu11 -Wall -Wextra -Werror -I$(RTL) -I../omci
REGS = $(RTL)/sniffer_regs.h $(RTL)/inline_regs.h
CODE = sniffer.c inline.c ../omci/omci.c
DEPS = $(CODE) sniffer.h inline.h $(REGS)

all: sniffer

$(REGS):
	@echo "$@ missing: generate the RTL first (sbt \"runMain pon.Generate\")" >&2; exit 1

sniffer: main.c $(DEPS)
	$(CC) $(CFLAGS) -o $@ main.c $(CODE)

test_sniffer test_inline: %: %.c $(DEPS)
	$(CC) $(CFLAGS) -fsanitize=address,undefined -pthread -o $@ $< $(CODE)

test: test_sniffer test_inline
	./test_sniffer
	./test_inline

clean:
	rm -f sniffer test_sniffer test_inline

.PHONY: all test clean
