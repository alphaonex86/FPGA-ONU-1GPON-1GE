// SPDX-License-Identifier: CERN-OHL-S-2.0
// Sniffer card ECP5 (LFE5UM-25F) top: the DCU lanes (common/hw/ecp5/gpon_dcu.v) and the DDR pins around
// SnifferBridge. To the DE25-Nano: data DDR on out_clk (77.76 MHz), forwarded clock 90 degrees later.
module sniffer_ecp5 (
  input         REFCLK_P, REFCLK_N,
  input         DS_RX_P, DS_RX_N, US_RX_P, US_RX_N,
  input         DS_SD,
  output        VCXO_PWM,
  output        FWD_CLK_S,
  output [15:0] DS_S,
  output [7:0]  US_S
);
  wire ref_clk, ds_clk, out_clk, fwd_clk, ref_rst, ds_rst, out_rst, ds_ready;
  wire [19:0] ds_rx, us_rx;
  gpon_dcu dcu (
    .REFCLK_P(REFCLK_P), .REFCLK_N(REFCLK_N), .DS_RX_P(DS_RX_P), .DS_RX_N(DS_RX_N), .US_RX_P(US_RX_P), .US_RX_N(US_RX_N),
    .DS_TX_P(), .DS_TX_N(), .US_TX_P(), .US_TX_N(), .ds_tx(20'b0), .us_tx(20'b0), .ds_rx(ds_rx), .us_rx(us_rx),
    .ref_clk(ref_clk), .ds_clk(ds_clk), .out_clk(out_clk), .fwd_clk(fwd_clk),
    .ref_rst(ref_rst), .ds_rst(ds_rst), .out_rst(out_rst), .ds_ready(ds_ready));

  wire [31:0] ds_pins;
  wire [15:0] us_pins;
  SnifferBridge bridge (
    .clk(ref_clk), .reset(ref_rst), .ds_clk(ds_clk), .ds_reset(ds_rst), .out_clk(out_clk), .out_reset(out_rst),
    .io_dsWord(ds_rx), .io_usWord(us_rx),
    .io_dsSignal(DS_SD && ds_ready), .io_vcxo(VCXO_PWM), .io_duty(), .io_usOdd(),
    .io_dsLine(), .io_dsValid(), .io_usBits(),
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
