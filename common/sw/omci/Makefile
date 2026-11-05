# SPDX-License-Identifier: CERN-OHL-S-2.0
CFLAGS ?= -O2
CFLAGS += -std=gnu11 -Wall -Wextra -Werror

test_omci: test_omci.c omci.c omci.h
	$(CC) $(CFLAGS) -fsanitize=address,undefined -o $@ test_omci.c omci.c

test: test_omci
	./test_omci

clean:
	rm -f test_omci

.PHONY: test clean
