// SPDX-License-Identifier: CERN-OHL-S-2.0
// Sniffer card ECP5 (LFE5UM-25F) top: DCU lanes, reset sequence, clocks and the DDR pins around SnifferBridge.
// CH0 downstream (CDR, recovered clock ds_clk), CH1 upstream 2x oversampled, read on ref_clk (the VCXO's,
// from CH0's TX clock). To the DE25-Nano: data DDR on out_clk (77.76 MHz), forwarded clock 90 degrees later.
module sniffer_ecp5 (
  input         REFCLK_P, REFCLK_N,
  input         DS_RX_P, DS_RX_N, US_RX_P, US_RX_N,
  input         DS_SD,
  output        VCXO_PWM,
  output        FWD_CLK_S,
  output [15:0] DS_S,
  output [7:0]  US_S
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
  wire ds_ready = &ds_locked_for;

  wire refclk, ref_clk, ds_clk, out_clk, fwd_clk, pll_lock;
  wire [23:0] ds_rx, us_rx;
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
    .CH0_FFC_TXPWDNB(1'b1), .CH0_FFC_LANE_TX_RST(tx_pll_rst), .CH0_FF_TX_PCLK(ref_clk), .CH0_FF_TXI_CLK(ref_clk),
    .CH1_HDINP(US_RX_P), .CH1_HDINN(US_RX_N), .CH1_RX_REFCLK(refclk),
    .CH1_FFC_RXPWDNB(1'b1), .CH1_FFC_RRST(!tx_ready), .CH1_FFC_LANE_RX_RST(!tx_ready), .CH1_FF_RXI_CLK(ref_clk),
    .CH1_FFC_TXPWDNB(1'b0),
    .CH0_FF_RX_D_0(ds_rx[0]), .CH0_FF_RX_D_1(ds_rx[1]), .CH0_FF_RX_D_2(ds_rx[2]), .CH0_FF_RX_D_3(ds_rx[3]),
    .CH0_FF_RX_D_4(ds_rx[4]), .CH0_FF_RX_D_5(ds_rx[5]), .CH0_FF_RX_D_6(ds_rx[6]), .CH0_FF_RX_D_7(ds_rx[7]),
    .CH0_FF_RX_D_8(ds_rx[8]), .CH0_FF_RX_D_9(ds_rx[9]), .CH0_FF_RX_D_12(ds_rx[12]), .CH0_FF_RX_D_13(ds_rx[13]),
    .CH0_FF_RX_D_14(ds_rx[14]), .CH0_FF_RX_D_15(ds_rx[15]), .CH0_FF_RX_D_16(ds_rx[16]), .CH0_FF_RX_D_17(ds_rx[17]),
    .CH0_FF_RX_D_18(ds_rx[18]), .CH0_FF_RX_D_19(ds_rx[19]), .CH0_FF_RX_D_20(ds_rx[20]), .CH0_FF_RX_D_21(ds_rx[21]),
    .CH1_FF_RX_D_0(us_rx[0]), .CH1_FF_RX_D_1(us_rx[1]), .CH1_FF_RX_D_2(us_rx[2]), .CH1_FF_RX_D_3(us_rx[3]),
    .CH1_FF_RX_D_4(us_rx[4]), .CH1_FF_RX_D_5(us_rx[5]), .CH1_FF_RX_D_6(us_rx[6]), .CH1_FF_RX_D_7(us_rx[7]),
    .CH1_FF_RX_D_8(us_rx[8]), .CH1_FF_RX_D_9(us_rx[9]), .CH1_FF_RX_D_12(us_rx[12]), .CH1_FF_RX_D_13(us_rx[13]),
    .CH1_FF_RX_D_14(us_rx[14]), .CH1_FF_RX_D_15(us_rx[15]), .CH1_FF_RX_D_16(us_rx[16]), .CH1_FF_RX_D_17(us_rx[17]),
    .CH1_FF_RX_D_18(us_rx[18]), .CH1_FF_RX_D_19(us_rx[19]), .CH1_FF_RX_D_20(us_rx[20]), .CH1_FF_RX_D_21(us_rx[21])
  );

  // out_clk = ref_clk x5/8 (VCO 622.08 MHz), fwd_clk the same 90 degrees later (2 VCO cycles)
  EHXPLLL #(.CLKI_DIV(8), .CLKFB_DIV(5), .CLKOP_DIV(8), .CLKOS_DIV(8), .CLKOP_CPHASE(7), .CLKOS_CPHASE(9),
            .CLKOS_ENABLE("ENABLED"), .FEEDBK_PATH("CLKOP"))
    pll (.CLKI(ref_clk), .CLKFB(out_clk), .RST(!tx_ready), .STDBY(1'b0), .PHASESEL0(1'b0), .PHASESEL1(1'b0),
         .PHASEDIR(1'b0), .PHASESTEP(1'b0), .PHASELOADREG(1'b0), .PLLWAKESYNC(1'b0),
         .ENCLKOP(1'b1), .ENCLKOS(1'b1), .ENCLKOS2(1'b0), .ENCLKOS3(1'b0),
         .CLKOP(out_clk), .CLKOS(fwd_clk), .LOCK(pll_lock));

  // resets: asserted at once, released two clocks after their condition clears, in each domain
  reg [1:0] ref_rst = 2'b11, ds_rst = 2'b11, out_rst = 2'b11;
  always @(posedge ref_clk or negedge tx_ready) if (!tx_ready) ref_rst <= 2'b11; else ref_rst <= {ref_rst[0], 1'b0};
  always @(posedge ds_clk or negedge ds_ready) if (!ds_ready) ds_rst <= 2'b11; else ds_rst <= {ds_rst[0], 1'b0};
  wire out_ok = tx_ready && pll_lock;
  always @(posedge out_clk or negedge out_ok) if (!out_ok) out_rst <= 2'b11; else out_rst <= {out_rst[0], 1'b0};

  wire [31:0] ds_pins;
  wire [15:0] us_pins;
  SnifferBridge bridge (
    .clk(ref_clk), .reset(ref_rst[1]), .ds_clk(ds_clk), .ds_reset(ds_rst[1]), .out_clk(out_clk), .out_reset(out_rst[1]),
    .io_dsWord({ds_rx[21:12], ds_rx[9:0]}), .io_usWord({us_rx[21:12], us_rx[9:0]}),
    .io_dsSignal(DS_SD && ds_ready), .io_vcxo(VCXO_PWM), .io_duty(), .io_usOdd(),
    .io_dsPins(ds_pins), .io_usPins(us_pins));

  // the first half of each word on the rising edge
  ODDRX1F fwd (.D0(1'b1), .D1(1'b0), .SCLK(fwd_clk), .RST(1'b0), .Q(FWD_CLK_S));
  genvar i;
  generate
    for (i = 0; i < 16; i = i + 1) begin : ds_out
      ODDRX1F q (.D0(ds_pins[31 - i]), .D1(ds_pins[15 - i]), .SCLK(out_clk), .RST(1'b0), .Q(DS_S[i]));
    end
    for (i = 0; i < 8; i = i + 1) begin : us_out
      ODDRX1F q (.D0(us_pins[15 - i]), .D1(us_pins[7 - i]), .SCLK(out_clk), .RST(1'b0), .Q(US_S[i]));
    end
  endgenerate
endmodule
