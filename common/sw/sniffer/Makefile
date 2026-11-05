# SPDX-License-Identifier: CERN-OHL-S-2.0
# sniffer_regs.h comes from the RTL build (out/rtl). Cross build for the HPS: make CC=aarch64-linux-gnu-gcc
RTL ?= ../../../out/rtl
CFLAGS ?= -O2
CFLAGS += -std=gnu11 -Wall -Wextra -Werror -I$(RTL)

all: sniffer

$(RTL)/sniffer_regs.h:
	@echo "$@ missing: generate the RTL first (sbt, PonBoard or SnifferCore)" >&2; exit 1

sniffer: main.c sniffer.c sniffer.h $(RTL)/sniffer_regs.h
	$(CC) $(CFLAGS) -o $@ main.c sniffer.c

test_sniffer: test_sniffer.c sniffer.c sniffer.h $(RTL)/sniffer_regs.h
	$(CC) $(CFLAGS) -fsanitize=address,undefined -o $@ test_sniffer.c sniffer.c

test: test_sniffer
	./test_sniffer

clean:
	rm -f sniffer test_sniffer

.PHONY: all test clean
