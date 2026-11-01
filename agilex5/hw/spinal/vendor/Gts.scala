// SPDX-License-Identifier: CERN-OHL-S-2.0
package vendor

import spinal.core._

/** GTS Direct PHY variant from agilex5/gts/gen_ip.sh (`variant`: olt, onu, eth): duplex, single width, parallel data
  * first bit in bit 0. */
case class GtsDuplex(variant: String, lockToRef: Boolean = false) extends BlackBox {
  setDefinitionName(variant)
  val i_rx_cdr_refclk_p, i_tx_pll_refclk_p, i_pma_cu_clk = in Bool ()
  val i_tx_reset, i_rx_reset = in Bool ()
  val o_tx_reset_ack, o_rx_reset_ack, o_tx_ready, o_rx_ready = out Bool ()
  val i_tx_coreclkin, i_rx_coreclkin = in Bool ()
  val o_tx_clkout, o_rx_clkout = out Bool ()
  val i_src_rs_grant = in Bool ()
  val o_src_rs_req = out Bool ()
  val o_tx_serial_data, o_tx_serial_data_n = out Bool ()
  val i_rx_serial_data, i_rx_serial_data_n = in Bool ()
  val o_tx_pll_locked, o_rx_is_lockedtodata, o_rx_is_lockedtoref = out Bool ()
  val i_rx_set_locktoref = lockToRef generate in(Bool())
  val i_tx_parallel_data = in Bits (80 bits)
  val o_rx_parallel_data = out Bits (80 bits)
  noIoPrefix()
}

/** TX-only GTS variant from agilex5/gts/gen_ip.sh (ben_tx). */
case class GtsTx(variant: String) extends BlackBox {
  setDefinitionName(variant)
  val i_tx_pll_refclk_p, i_pma_cu_clk, i_tx_reset, i_tx_coreclkin, i_src_rs_grant = in Bool ()
  val o_tx_reset_ack, o_tx_ready, o_tx_clkout, o_src_rs_req, o_tx_serial_data, o_tx_serial_data_n, o_tx_pll_locked = out Bool ()
  val i_tx_parallel_data = in Bits (80 bits)
  noIoPrefix()
}

/** Shared reference clock and reset sequencer of the GTS lanes (one req/grant pair per lane). */
case class GtsSrcss(lanes: Int) extends BlackBox {
  setDefinitionName("srcss")
  val o_src_rs_grant = out Bits (lanes bits)
  val i_src_rs_priority, i_src_rs_req = in Bits (lanes bits)
  val o_pma_cu_clk = out Bool ()
  val i_src_rs_refclk_status_bus = in Bits (10 bits)
  val o_refclk_fail_status = out Bits (8 bits)
  val o_refclk_on_ack = out Bool ()
  val i_refclk_on = in Bits (10 bits)
  val o_src_rs_refclk_cmd_bus = out Bits (10 bits)
  noIoPrefix()
}

/** Clock network divider from gen_ip.sh (pon_clocks): inclk and inclk / 2, aligned and timed as related clocks. */
case class PonClocks() extends BlackBox {
  setDefinitionName("pon_clocks")
  val inclk = in Bool ()
  val clock_div1x, clock_div2x = out Bool ()
  noIoPrefix()
}

/** I/O PLL from gen_ip.sh (helper_pll): 155.357 MHz from a 50 MHz board oscillator, the VCXO loop's helper clock. */
case class HelperPll() extends BlackBox {
  setDefinitionName("helper_pll")
  val refclk, rst = in Bool ()
  val outclk_0, locked = out Bool ()
  noIoPrefix()
}
