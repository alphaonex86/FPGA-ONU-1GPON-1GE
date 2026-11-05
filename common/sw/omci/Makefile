# SPDX-License-Identifier: CERN-OHL-S-2.0
CFLAGS ?= -O2
CFLAGS += -std=gnu11 -Wall -Wextra -Werror

all: omci_profile

omci_profile: omci_profile.c omci.c omci.h
	$(CC) $(CFLAGS) -o $@ omci_profile.c omci.c

test_omci: test_omci.c omci.c omci.h
	$(CC) $(CFLAGS) -fsanitize=address,undefined -o $@ test_omci.c omci.c

test: test_omci
	./test_omci

clean:
	rm -f omci_profile test_omci

.PHONY: all test clean
