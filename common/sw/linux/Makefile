# SPDX-License-Identifier: GPL-2.0-or-later
# Out-of-tree module: make KDIR=<kernel build dir> [GPON=<fork gpon dir>] [RTL=<out/rtl>] [ARCH= CROSS_COMPILE=]
# The GPL core's sources are linked into gpon/ (kbuild objects stay under the module dir).
ifneq ($(KERNELRELEASE),)
obj-m := fpga-onu.o
fpga-onu-y := fpga_onu.o gpon/gpon_omci_core.o gpon/gpon_omci_me.o gpon/gpon_sn.o
ccflags-y := -I$(src)/gpon -I$(RTL)
else
KDIR ?= /lib/modules/$(shell uname -r)/build
GPON ?= /mnt/data/perso/tmp/Realtek-RTL9607C/openwrt-fork/target/linux/gpon-common/files-6.18/drivers/net/gpon
RTL ?= $(abspath ../../../out/rtl)
CORE := gpon_omci_core.c gpon_omci_me.c gpon_sn.c

all: gpon
	$(MAKE) -C $(KDIR) M=$(CURDIR) RTL=$(RTL) modules

gpon: $(addprefix $(GPON)/,$(CORE)) $(RTL)/onu_regs.h $(RTL)/gem_queues.h
	mkdir -p gpon
	for f in $(GPON)/*.h $(addprefix $(GPON)/,$(CORE)); do ln -sf $$f gpon/; done

$(RTL)/%.h:
	@echo "$@ missing: generate the RTL first (sbt, PonBoard)" >&2; exit 1

clean:
	$(MAKE) -C $(KDIR) M=$(CURDIR) clean
	rm -rf gpon

.PHONY: all clean gpon
endif
