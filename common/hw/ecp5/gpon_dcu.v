// SPDX-License-Identifier: CERN-OHL-S-2.0
// ECP5 DCU on a GPON line at 2.48832 Gb/s from a 155.52 MHz VCXO, with its reset sequence and clocks.
// CH0 downstream (CDR, recovered clock ds_clk), CH1 upstream 2x oversampled, read on ref_clk (the VCXO's, from
// CH0's TX clock). Words of 20 bits, first line bit in bit 0. US_TX: CH1's transmitter powered (a repeater).
// out_clk = ref_clk x5/8 (77.76 MHz), fwd_clk the same 90 degrees later. Needs dcu_params.vh (gen.py).
module gpon_dcu #(parameter US_TX = 0) (
  input         REFCLK_P, REFCLK_N,
  input         DS_RX_P, DS_RX_N, US_RX_P, US_RX_N,
  output        DS_TX_P, DS_TX_N, US_TX_P, US_TX_N,
  input  [19:0] ds_tx, us_tx,      // ref_clk
  output [19:0] ds_rx,             // ds_clk
  output [19:0] us_rx,             // ref_clk
  output        ref_clk, ds_clk, out_clk, fwd_clk,
  output        ref_rst, ds_rst, out_rst,
  output        ds_ready           // downstream present and its CDR locked for 1.7 ms
);
  // power-on sequence on the internal oscillator (2.4 MHz): DCU macro, then TX PLL, then the RX lanes
  wire slow_clk;
  OSCG #(.DIV(128)) osc (.OSC(slow_clk));
  reg [15:0] por = 0;
  always @(posedge slow_clk) if (!(&por)) por <= por + 1'b1;
  wire plol, ds_los, ds_lol;
  reg [1:0] plol_s = 2'b11, ds_los_s = 2'b11, ds_lol_s = 2'b11;
  always @(posedge slow_clk) begin
    plol_s <= {plol_s[0], plol};
    ds_los_s <= {ds_los_s[0], ds_los};
    ds_lol_s <= {ds_lol_s[0], ds_lol};
  end
  wire macro_rst = !por[14];
  wire tx_pll_rst = !por[15];
  wire tx_ready = (&por) && !plol_s[1];
  // CH0: CDR held in reset without signal, lane reset until locked for 2^12 slow clocks (1.7 ms)
  reg [11:0] ds_locked_for = 0;
  always @(posedge slow_clk)
    if (!tx_ready || ds_los_s[1] || ds_lol_s[1]) ds_locked_for <= 0;
    else if (!(&ds_locked_for)) ds_locked_for <= ds_locked_for + 1'b1;
  assign ds_ready = &ds_locked_for;

  wire refclk, pll_lock;
  wire [23:0] ds_rx_bus, us_rx_bus;
  wire [23:0] ds_tx_bus = {2'b00, ds_tx[19:10], 2'b00, ds_tx[9:0]};
  wire [23:0] us_tx_bus = {2'b00, us_tx[19:10], 2'b00, us_tx[9:0]};
  assign ds_rx = {ds_rx_bus[21:12], ds_rx_bus[9:0]};
  assign us_rx = {us_rx_bus[21:12], us_rx_bus[9:0]};
  (* LOC = "EXTREF0" *)
  EXTREFB #(.REFCK_PWDNB("0b1"), .REFCK_RTERM("0b1"), .REFCK_DCBIAS_EN("0b0"))
    extref (.REFCLKP(REFCLK_P), .REFCLKN(REFCLK_N), .REFCLKO(refclk));
  (* LOC = "DCU0" *)
  DCUA #(
`include "dcu_params.vh"
  ) dcu (
    .D_REFCLKI(refclk), .D_FFC_MACROPDB(1'b1), .D_FFC_MACRO_RST(macro_rst), .D_FFC_DUAL_RST(macro_rst),
    .D_FFC_TRST(tx_pll_rst), .D_FFS_PLOL(plol),
    .CH0_HDINP(DS_RX_P), .CH0_HDINN(DS_RX_N), .CH0_RX_REFCLK(refclk),
    .CH0_FFC_RXPWDNB(1'b1), .CH0_FFC_RRST(!tx_ready || ds_los_s[1]), .CH0_FFC_LANE_RX_RST(!ds_ready),
    .CH0_FFS_RLOS(ds_los), .CH0_FFS_RLOL(ds_lol), .CH0_FF_RX_PCLK(ds_clk), .CH0_FF_RXI_CLK(ds_clk),
    .CH0_HDOUTP(DS_TX_P), .CH0_HDOUTN(DS_TX_N),
    .CH0_FFC_TXPWDNB(1'b1), .CH0_FFC_LANE_TX_RST(tx_pll_rst), .CH0_FF_TX_PCLK(ref_clk), .CH0_FF_TXI_CLK(ref_clk),
    .CH1_HDINP(US_RX_P), .CH1_HDINN(US_RX_N), .CH1_RX_REFCLK(refclk),
    .CH1_FFC_RXPWDNB(1'b1), .CH1_FFC_RRST(!tx_ready), .CH1_FFC_LANE_RX_RST(!tx_ready), .CH1_FF_RXI_CLK(ref_clk),
    .CH1_HDOUTP(US_TX_P), .CH1_HDOUTN(US_TX_N),
    .CH1_FFC_TXPWDNB(US_TX ? 1'b1 : 1'b0), .CH1_FFC_LANE_TX_RST(tx_pll_rst), .CH1_FF_TXI_CLK(ref_clk),
    .CH0_FF_RX_D_0(ds_rx_bus[0]), .CH0_FF_RX_D_1(ds_rx_bus[1]), .CH0_FF_RX_D_2(ds_rx_bus[2]), .CH0_FF_RX_D_3(ds_rx_bus[3]),
    .CH0_FF_RX_D_4(ds_rx_bus[4]), .CH0_FF_RX_D_5(ds_rx_bus[5]), .CH0_FF_RX_D_6(ds_rx_bus[6]), .CH0_FF_RX_D_7(ds_rx_bus[7]),
    .CH0_FF_RX_D_8(ds_rx_bus[8]), .CH0_FF_RX_D_9(ds_rx_bus[9]), .CH0_FF_RX_D_12(ds_rx_bus[12]), .CH0_FF_RX_D_13(ds_rx_bus[13]),
    .CH0_FF_RX_D_14(ds_rx_bus[14]), .CH0_FF_RX_D_15(ds_rx_bus[15]), .CH0_FF_RX_D_16(ds_rx_bus[16]), .CH0_FF_RX_D_17(ds_rx_bus[17]),
    .CH0_FF_RX_D_18(ds_rx_bus[18]), .CH0_FF_RX_D_19(ds_rx_bus[19]), .CH0_FF_RX_D_20(ds_rx_bus[20]), .CH0_FF_RX_D_21(ds_rx_bus[21]),
    .CH1_FF_RX_D_0(us_rx_bus[0]), .CH1_FF_RX_D_1(us_rx_bus[1]), .CH1_FF_RX_D_2(us_rx_bus[2]), .CH1_FF_RX_D_3(us_rx_bus[3]),
    .CH1_FF_RX_D_4(us_rx_bus[4]), .CH1_FF_RX_D_5(us_rx_bus[5]), .CH1_FF_RX_D_6(us_rx_bus[6]), .CH1_FF_RX_D_7(us_rx_bus[7]),
    .CH1_FF_RX_D_8(us_rx_bus[8]), .CH1_FF_RX_D_9(us_rx_bus[9]), .CH1_FF_RX_D_12(us_rx_bus[12]), .CH1_FF_RX_D_13(us_rx_bus[13]),
    .CH1_FF_RX_D_14(us_rx_bus[14]), .CH1_FF_RX_D_15(us_rx_bus[15]), .CH1_FF_RX_D_16(us_rx_bus[16]), .CH1_FF_RX_D_17(us_rx_bus[17]),
    .CH1_FF_RX_D_18(us_rx_bus[18]), .CH1_FF_RX_D_19(us_rx_bus[19]), .CH1_FF_RX_D_20(us_rx_bus[20]), .CH1_FF_RX_D_21(us_rx_bus[21]),
    .CH0_FF_TX_D_0(ds_tx_bus[0]), .CH0_FF_TX_D_1(ds_tx_bus[1]), .CH0_FF_TX_D_2(ds_tx_bus[2]), .CH0_FF_TX_D_3(ds_tx_bus[3]),
    .CH0_FF_TX_D_4(ds_tx_bus[4]), .CH0_FF_TX_D_5(ds_tx_bus[5]), .CH0_FF_TX_D_6(ds_tx_bus[6]), .CH0_FF_TX_D_7(ds_tx_bus[7]),
    .CH0_FF_TX_D_8(ds_tx_bus[8]), .CH0_FF_TX_D_9(ds_tx_bus[9]), .CH0_FF_TX_D_12(ds_tx_bus[12]), .CH0_FF_TX_D_13(ds_tx_bus[13]),
    .CH0_FF_TX_D_14(ds_tx_bus[14]), .CH0_FF_TX_D_15(ds_tx_bus[15]), .CH0_FF_TX_D_16(ds_tx_bus[16]), .CH0_FF_TX_D_17(ds_tx_bus[17]),
    .CH0_FF_TX_D_18(ds_tx_bus[18]), .CH0_FF_TX_D_19(ds_tx_bus[19]), .CH0_FF_TX_D_20(ds_tx_bus[20]), .CH0_FF_TX_D_21(ds_tx_bus[21]),
    .CH1_FF_TX_D_0(us_tx_bus[0]), .CH1_FF_TX_D_1(us_tx_bus[1]), .CH1_FF_TX_D_2(us_tx_bus[2]), .CH1_FF_TX_D_3(us_tx_bus[3]),
    .CH1_FF_TX_D_4(us_tx_bus[4]), .CH1_FF_TX_D_5(us_tx_bus[5]), .CH1_FF_TX_D_6(us_tx_bus[6]), .CH1_FF_TX_D_7(us_tx_bus[7]),
    .CH1_FF_TX_D_8(us_tx_bus[8]), .CH1_FF_TX_D_9(us_tx_bus[9]), .CH1_FF_TX_D_12(us_tx_bus[12]), .CH1_FF_TX_D_13(us_tx_bus[13]),
    .CH1_FF_TX_D_14(us_tx_bus[14]), .CH1_FF_TX_D_15(us_tx_bus[15]), .CH1_FF_TX_D_16(us_tx_bus[16]), .CH1_FF_TX_D_17(us_tx_bus[17]),
    .CH1_FF_TX_D_18(us_tx_bus[18]), .CH1_FF_TX_D_19(us_tx_bus[19]), .CH1_FF_TX_D_20(us_tx_bus[20]), .CH1_FF_TX_D_21(us_tx_bus[21])
  );

  // VCO 622.08 MHz; fwd_clk 2 VCO cycles later
  EHXPLLL #(.CLKI_DIV(8), .CLKFB_DIV(5), .CLKOP_DIV(8), .CLKOS_DIV(8), .CLKOP_CPHASE(7), .CLKOS_CPHASE(9),
            .CLKOS_ENABLE("ENABLED"), .FEEDBK_PATH("CLKOP"))
    pll (.CLKI(ref_clk), .CLKFB(out_clk), .RST(!tx_ready), .STDBY(1'b0), .PHASESEL0(1'b0), .PHASESEL1(1'b0),
         .PHASEDIR(1'b0), .PHASESTEP(1'b0), .PHASELOADREG(1'b0), .PLLWAKESYNC(1'b0),
         .ENCLKOP(1'b1), .ENCLKOS(1'b1), .ENCLKOS2(1'b0), .ENCLKOS3(1'b0),
         .CLKOP(out_clk), .CLKOS(fwd_clk), .LOCK(pll_lock));

  // resets: asserted at once, released two clocks after their condition clears, in each domain
  reg [1:0] ref_r = 2'b11, ds_r = 2'b11, out_r = 2'b11;
  always @(posedge ref_clk or negedge tx_ready) if (!tx_ready) ref_r <= 2'b11; else ref_r <= {ref_r[0], 1'b0};
  always @(posedge ds_clk or negedge ds_ready) if (!ds_ready) ds_r <= 2'b11; else ds_r <= {ds_r[0], 1'b0};
  wire out_ok = tx_ready && pll_lock;
  always @(posedge out_clk or negedge out_ok) if (!out_ok) out_r <= 2'b11; else out_r <= {out_r[0], 1'b0};
  assign ref_rst = ref_r[1];
  assign ds_rst = ds_r[1];
  assign out_rst = out_r[1];
endmodule
