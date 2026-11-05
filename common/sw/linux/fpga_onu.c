// SPDX-License-Identifier: GPL-2.0-or-later
/*
 * fpga_onu.c -- the FPGA ONU's shell for the GPL GPON core (OpenWrt fork, drivers/net/gpon): activation runs in
 * the FPGA (OnuPloam), the core's OMCI responder answers on the OMCI Port-ID through the CPU GEM queues, and a
 * profile recorded from a stock ONU (common/sw/omci) answers what its ME model lacks. HPS Linux, PonBoard's lwOnu
 * window: MAC registers (onu_regs.h) at 0x000, CPU GEM queues (gem_queues.h) at 0x200.
 * Not yet here: the Encryption_Key exchange (Request_Key is logged), the data path's OMCI to bridge mapping.
 */
#include <linux/firmware.h>
#include <linux/interrupt.h>
#include <linux/io.h>
#include <linux/module.h>
#include <linux/mutex.h>
#include <linux/of.h>
#include <linux/platform_device.h>
#include <linux/proc_fs.h>
#include <linux/random.h>
#include <linux/seq_file.h>
#include <linux/slab.h>

#include "gpon_omci_core.h"
#include "gpon_omci_me.h"
#include "gpon_sn.h"
#include "onu_regs.h"
#include "gem_queues.h"

#define FO_MAC 0x000
#define FO_GEMQ 0x200
#define FO_O5 4			/* ONU_STATE.state: 0 = O1 .. 6 = O7 */
#define FO_PROFILE_MAGIC "OMCIPRF1"

static char *sn = "XPON00000001";
module_param(sn, charp, 0444);
MODULE_PARM_DESC(sn, "G.984.3 serial number, AAAAhhhhhhhh");
static char *password = "";
module_param(password, charp, 0444);
MODULE_PARM_DESC(password, "G.984.3 password, up to 10 characters (zero padded)");
static char *profile = "fpga-onu-omci.bin";
module_param(profile, charp, 0444);
MODULE_PARM_DESC(profile, "OMCI clone profile (firmware file, common/sw/omci/omci_profile), absent: the ME model alone");

struct fpga_onu {
	struct device *dev;
	void __iomem *base;
	struct mutex lock;		/* the core is single-threaded: every call holds this */
	struct omci_onu *onu;
	u8 sn[GPON_SN_BYTES];
	u8 *profile;
	u32 profile_n;
	u8 rx[OMCI_LEN];		/* the OMCI message being reassembled */
	unsigned int rx_len;
	bool rx_lost;			/* a fragment of it was cut, truncated or too long */
	u8 state, onu_id;
	u32 omci_rx, omci_tx, omci_lost, tx_refused, gem_other, ploam_rx;
	u32 ploam_types[256];
};

static u32 fo_rd(struct fpga_onu *fo, unsigned int offset) { return readl(fo->base + offset); }
static void fo_wr(struct fpga_onu *fo, unsigned int offset, u32 v) { writel(v, fo->base + offset); }

static u32 fo_be32(const u8 *p) { return (u32)p[0] << 24 | (u32)p[1] << 16 | (u32)p[2] << 8 | p[3]; }

static void fo_send(struct fpga_onu *fo, u16 port, const u8 *msg)
{
	u32 status = fo_rd(fo, FO_GEMQ + GEMQ_STATUS);
	int i;

	if ((status & GEMQ_STATUS_TXBUSY_MASK) ||
	    (status & GEMQ_STATUS_TXFREE_MASK) >> GEMQ_STATUS_TXFREE_SHIFT < 1 + OMCI_LEN / 4) {
		fo->tx_refused++;
		dev_warn_ratelimited(fo->dev, "OMCI answer dropped: TX queue full (status %08x)\n", status);
		return;
	}
	fo_wr(fo, FO_GEMQ + GEMQ_TX_HEAD, (port << GEMQ_TX_HEAD_PORT_SHIFT) | (OMCI_LEN << GEMQ_TX_HEAD_LENGTH_SHIFT));
	for (i = 0; i < OMCI_LEN; i += 4)
		fo_wr(fo, FO_GEMQ + GEMQ_TX_DATA, fo_be32(msg + i));
	fo->omci_tx++;
}

/* A whole message on the OMCI Port-ID: the core's answer, then the AVCs it owes. */
static void fo_omci(struct fpga_onu *fo, u16 port)
{
	u8 resp[OMCI_LEN];

	fo->omci_rx++;
	if (omci_onu_input(fo->onu, fo->rx, OMCI_LEN, resp) == OMCI_LEN)
		fo_send(fo, port, resp);
	if (omci_onu_owed_avc(fo->onu, resp) == OMCI_LEN) {
		fo_send(fo, port, resp);
		omci_onu_owed_avc_sent(fo->onu);
	}
	/* the OLT gates downstream user data on the VEIP being up once it provisioned */
	if (fo->onu->provisioned && !fo->onu->avc_veip_up_sent && fo->state == FO_O5 &&
	    omci_onu_emit_veip_up_avc(fo->onu, resp) == OMCI_LEN)
		fo_send(fo, port, resp);
}

static void fo_gem_rx(struct fpga_onu *fo)
{
	u32 st = fo_rd(fo, FO_MAC + ONU_STATE);
	bool omci_valid = st & ONU_STATE_OMCIVALID_MASK;
	u16 omci_port = (st & ONU_STATE_OMCIPORT_MASK) >> ONU_STATE_OMCIPORT_SHIFT;

	while (fo_rd(fo, FO_GEMQ + GEMQ_RX_DESC_0) & GEMQ_RX_DESC_0_READY_MASK) {
		u32 d0 = fo_rd(fo, FO_GEMQ + GEMQ_RX_DESC_0), d1 = fo_rd(fo, FO_GEMQ + GEMQ_RX_DESC_1);
		u16 port = (d0 & GEMQ_RX_DESC_0_PORT_MASK) >> GEMQ_RX_DESC_0_PORT_SHIFT;
		unsigned int bytes = (d1 & GEMQ_RX_DESC_1_BYTES_MASK) >> GEMQ_RX_DESC_1_BYTES_SHIFT;
		unsigned int words = (d1 & GEMQ_RX_DESC_1_WORDS_MASK) >> GEMQ_RX_DESC_1_WORDS_SHIFT;
		bool omci = omci_valid && port == omci_port, keep;
		unsigned int i;

		if (!omci)
			fo->gem_other++;
		keep = omci && !fo->rx_lost && fo->rx_len + bytes <= OMCI_LEN;
		if (omci && !keep)
			fo->rx_lost = true; /* an extended (or broken) message: the core serves baseline only */
		for (i = 0; i < words; i++) { /* the last word pops the descriptor */
			u32 w = fo_rd(fo, FO_GEMQ + GEMQ_RX_DATA);
			unsigned int k;

			for (k = 0; k < 4 && keep && 4 * i + k < bytes; k++)
				fo->rx[fo->rx_len + 4 * i + k] = (u8)(w >> (24 - 8 * k));
		}
		if (!omci)
			continue;
		if (keep)
			fo->rx_len += bytes;
		if (d1 & (GEMQ_RX_DESC_1_CUT_MASK | GEMQ_RX_DESC_1_TRUNCATED_MASK))
			fo->rx_lost = true;
		if (!(d1 & GEMQ_RX_DESC_1_END_MASK))
			continue;
		if (fo->rx_lost || fo->rx_len != OMCI_LEN) {
			fo->omci_lost++;
			dev_warn_ratelimited(fo->dev, "OMCI message lost (%u bytes%s)\n", fo->rx_len,
					     fo->rx_lost ? ", cut, truncated or over 48" : "");
		} else {
			fo_omci(fo, port);
		}
		fo->rx_len = 0;
		fo->rx_lost = false;
	}
}

/* Every PLOAM addressed to us reaches the CPU too; the FPGA already acted on its fixed set. */
static void fo_ploam_rx(struct fpga_onu *fo)
{
	while (fo_rd(fo, FO_MAC + ONU_STATUS) & ONU_STATUS_RX_MASK) {
		u32 b0 = fo_rd(fo, FO_MAC + ONU_PLOAM_RX_0);
		u32 b1 = fo_rd(fo, FO_MAC + ONU_PLOAM_RX_1);
		u32 b2 = fo_rd(fo, FO_MAC + ONU_PLOAM_RX_2); /* pops it */
		u8 type = (u8)(b0 >> 16);

		fo->ploam_rx++;
		fo->ploam_types[type]++;
		if (type == 0x0d)
			dev_warn_ratelimited(fo->dev, "Request_Key: the key exchange is not in this shell yet\n");
		dev_dbg(fo->dev, "PLOAM %08x %08x %08x\n", b0, b1, b2);
	}
}

static void fo_errors(struct fpga_onu *fo)
{
	u32 mac = fo_rd(fo, FO_MAC + ONU_ERRORS), gemq = fo_rd(fo, FO_GEMQ + GEMQ_ERRORS);

	if (mac) {
		fo_wr(fo, FO_MAC + ONU_ERRORS, mac);
		dev_warn_ratelimited(fo->dev, "MAC errors %08x\n", mac);
	}
	if (gemq) {
		fo_wr(fo, FO_GEMQ + GEMQ_ERRORS, gemq);
		dev_warn_ratelimited(fo->dev, "GEM queue errors %08x\n", gemq);
	}
}

static irqreturn_t fo_thread(int irq, void *data)
{
	struct fpga_onu *fo = data;
	u32 events;

	mutex_lock(&fo->lock);
	events = fo_rd(fo, FO_MAC + ONU_EVENTS);
	if (events) {
		u32 st = fo_rd(fo, FO_MAC + ONU_STATE);

		fo_wr(fo, FO_MAC + ONU_EVENTS, events);
		fo->state = (st & ONU_STATE_STATE_MASK) >> ONU_STATE_STATE_SHIFT;
		fo->onu_id = (st & ONU_STATE_ONUID_MASK) >> ONU_STATE_ONUID_SHIFT;
		dev_info(fo->dev, "O%u, ONU-ID %u\n", fo->state + 1, fo->onu_id);
	}
	fo_ploam_rx(fo);
	fo_gem_rx(fo);
	fo_errors(fo);
	mutex_unlock(&fo->lock);
	return IRQ_HANDLED;
}

static int fo_show(struct seq_file *m, void *v)
{
	struct fpga_onu *fo = m->private;
	char core[512];
	int i;

	mutex_lock(&fo->lock);
	seq_printf(m, "state O%u onu_id %u\nomci rx %u tx %u lost %u tx_refused %u gem_other %u\n", fo->state + 1,
		   fo->onu_id, fo->omci_rx, fo->omci_tx, fo->omci_lost, fo->tx_refused, fo->gem_other);
	seq_printf(m, "ploam_rx %u:", fo->ploam_rx);
	for (i = 0; i < 256; i++)
		if (fo->ploam_types[i])
			seq_printf(m, " %02x=%u", i, fo->ploam_types[i]);
	omci_resp_fmt(fo->onu, true, fo->omci_tx, fo->tx_refused, core, sizeof(core));
	seq_printf(m, "\n%s", core);
	mutex_unlock(&fo->lock);
	return 0;
}

/* OMCIPRF1, then records of OMCI_PROFILE_REC bytes sorted by request (the core binary-searches them). */
static int fo_load_profile(struct fpga_onu *fo)
{
	const struct firmware *fw;
	size_t n, i;
	int rc = firmware_request_nowarn(&fw, profile, fo->dev);

	if (rc) {
		dev_info(fo->dev, "no OMCI profile %s (%d): the ME model alone\n", profile, rc);
		return 0;
	}
	n = fw->size >= 8 ? (fw->size - 8) / OMCI_PROFILE_REC : 0;
	rc = -EINVAL;
	if (fw->size < 8 || memcmp(fw->data, FO_PROFILE_MAGIC, 8) || 8 + n * OMCI_PROFILE_REC != fw->size) {
		dev_err(fo->dev, "OMCI profile %s: not OMCIPRF1 records (%zu bytes)\n", profile, fw->size);
		goto out;
	}
	for (i = 1; i < n; i++)
		if (memcmp(fw->data + 8 + (i - 1) * OMCI_PROFILE_REC, fw->data + 8 + i * OMCI_PROFILE_REC, 38) >= 0) {
			dev_err(fo->dev, "OMCI profile %s: record %zu out of order\n", profile, i);
			goto out;
		}
	rc = -ENOMEM;
	fo->profile = kmemdup(fw->data + 8, n * OMCI_PROFILE_REC, GFP_KERNEL);
	if (!fo->profile && n)
		goto out;
	fo->profile_n = n;
	omci_onu_set_profile(fo->onu, fo->profile, fo->profile_n);
	dev_info(fo->dev, "OMCI profile %s: %zu answers\n", profile, n);
	rc = 0;
out:
	release_firmware(fw);
	return rc;
}

static int fo_probe(struct platform_device *pdev)
{
	struct fpga_onu *fo;
	u8 pw[12] = { 0 };
	int rc, i, irq;

	fo = devm_kzalloc(&pdev->dev, sizeof(*fo), GFP_KERNEL);
	if (!fo)
		return -ENOMEM;
	fo->onu = devm_kzalloc(&pdev->dev, sizeof(*fo->onu), GFP_KERNEL);
	if (!fo->onu)
		return -ENOMEM;
	fo->dev = &pdev->dev;
	mutex_init(&fo->lock);
	platform_set_drvdata(pdev, fo);
	if (gpon_sn_parse(sn, fo->sn)) {
		dev_err(fo->dev, "sn '%s' is not AAAAhhhhhhhh\n", sn);
		return -EINVAL;
	}
	if (strlen(password) > 10) {
		dev_err(fo->dev, "password over 10 characters\n");
		return -EINVAL;
	}
	memcpy(pw, password, strlen(password));
	fo->base = devm_platform_ioremap_resource(pdev, 0);
	if (IS_ERR(fo->base))
		return PTR_ERR(fo->base);
	omci_onu_init(fo->onu, fo->sn, get_random_u8());
	rc = fo_load_profile(fo);
	if (rc)
		return rc;

	fo_wr(fo, FO_MAC + ONU_CTRL, 0);
	fo_wr(fo, FO_MAC + ONU_SN_0, fo_be32(fo->sn));
	fo_wr(fo, FO_MAC + ONU_SN_1, fo_be32(fo->sn + 4));
	fo_wr(fo, FO_MAC + ONU_PASSWORD_0, fo_be32(pw));
	fo_wr(fo, FO_MAC + ONU_PASSWORD_1, fo_be32(pw + 4));
	fo_wr(fo, FO_MAC + ONU_PASSWORD_2, (u32)pw[8] << 8 | pw[9]);
	for (i = 0; i < 2; i++) { /* "pon" (OnuRegs), "gem" (the CPU GEM queues) */
		irq = platform_get_irq(pdev, i);
		if (irq < 0)
			return irq;
		rc = devm_request_threaded_irq(fo->dev, irq, NULL, fo_thread, IRQF_ONESHOT, dev_name(fo->dev), fo);
		if (rc) {
			dev_err(fo->dev, "IRQ %d refused (%d)\n", irq, rc);
			return rc;
		}
	}
	fo_wr(fo, FO_GEMQ + GEMQ_IRQ_ENABLE, GEMQ_IRQ_ENABLE_RX_MASK | GEMQ_IRQ_ENABLE_ERROR_MASK);
	fo_wr(fo, FO_MAC + ONU_IRQ_ENABLE, ONU_IRQ_ENABLE_RX_MASK | ONU_IRQ_ENABLE_STATE_MASK | ONU_IRQ_ENABLE_ERROR_MASK);
	if (!proc_create_single_data("fpga_onu", 0444, NULL, fo_show, fo)) {
		dev_err(fo->dev, "no /proc/fpga_onu\n");
		return -ENOMEM;
	}
	fo_wr(fo, FO_MAC + ONU_CTRL, ONU_CTRL_ENABLE_MASK);
	dev_info(fo->dev, "ONU %s, activation on\n", sn);
	return 0;
}

static void fo_remove(struct platform_device *pdev)
{
	struct fpga_onu *fo = platform_get_drvdata(pdev);

	fo_wr(fo, FO_MAC + ONU_CTRL, 0); /* O1, laser dark */
	fo_wr(fo, FO_MAC + ONU_IRQ_ENABLE, 0);
	fo_wr(fo, FO_GEMQ + GEMQ_IRQ_ENABLE, 0);
	remove_proc_entry("fpga_onu", NULL);
	mutex_lock(&fo->lock);
	omci_onu_set_profile(fo->onu, NULL, 0);
	mutex_unlock(&fo->lock);
	kfree(fo->profile);
}

static const struct of_device_id fo_of_match[] = {
	{ .compatible = "fpga-onu,onu-mac" },
	{ }
};
MODULE_DEVICE_TABLE(of, fo_of_match);

static struct platform_driver fo_driver = {
	.probe = fo_probe,
	.remove = fo_remove,
	.driver = { .name = "fpga-onu", .of_match_table = fo_of_match },
};
module_platform_driver(fo_driver);
MODULE_DESCRIPTION("FPGA ONU: the GPL GPON core's OMCI responder on the FPGA's MAC");
MODULE_LICENSE("GPL");
