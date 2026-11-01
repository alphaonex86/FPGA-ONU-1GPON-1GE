// SPDX-License-Identifier: CERN-OHL-S-2.0
// Fit check of the HSMC card lanes on the DE25-Standard: every GTS lane at its rate, from its refclk pin.
// Lane 3 OLT, 2 Ethernet, 1 ONU, 0 burst enable (TX only); PRBS-ish counters keep the datapaths alive.
module hsmc_lanes (
    input  wire       pon_refclk,   // VCXO 155.52 MHz (HSMC_GTS_RX_REF)
    input  wire       eth_refclk,   // XO 156.25 MHz (HSMC_GTS_REFCLK)
    input  wire       reset,
    output wire [3:0] tx_p, tx_n,
    input  wire [3:1] rx_p, rx_n,
    output wire [7:0] status
);
    wire [3:0] req, grant;
    wire       cu_clk;
    srcss srcss (.o_src_rs_grant(grant), .i_src_rs_priority(4'b0), .i_src_rs_req(req), .o_pma_cu_clk(cu_clk),
                 .i_src_rs_refclk_status_bus(10'b0), .o_refclk_fail_status(), .o_refclk_on_ack(), .i_refclk_on(10'b0),
                 .o_src_rs_refclk_cmd_bus());

    wire [4:0] tx_clk;
    reg  [7:0] tx_count [4:0];
    genvar i;
    generate for (i = 0; i < 5; i = i + 1) begin : g_tx
        always @(posedge tx_clk[i]) tx_count[i] <= tx_count[i] + 8'd1;
    end endgenerate

    wire [3:0] rx_clk;
    wire [79:0] rx_data [3:0];
    reg  [3:0] rx_bit;
    generate for (i = 0; i < 4; i = i + 1) begin : g_rx
        always @(posedge rx_clk[i]) rx_bit[i] <= ^rx_data[i][9:0];
    end endgenerate
    assign status = {4'b0, rx_bit};

    olt olt (.i_rx_cdr_refclk_p(pon_refclk), .i_tx_pll_refclk_p(pon_refclk), .i_pma_cu_clk(cu_clk), .i_tx_reset(reset),
        .i_rx_reset(reset), .o_tx_reset_ack(), .o_rx_reset_ack(), .o_tx_ready(), .o_rx_ready(),
        .i_tx_coreclkin(tx_clk[0]), .i_rx_coreclkin(rx_clk[3]), .o_tx_clkout(tx_clk[0]), .o_rx_clkout(rx_clk[3]),
        .i_src_rs_grant(grant[0]), .o_src_rs_req(req[0]), .o_tx_serial_data(tx_p[3]), .o_tx_serial_data_n(tx_n[3]),
        .i_rx_serial_data(rx_p[3]), .i_rx_serial_data_n(rx_n[3]), .o_tx_pll_locked(), .o_rx_is_lockedtodata(),
        .o_rx_is_lockedtoref(), .i_rx_set_locktoref(1'b0), .i_tx_parallel_data({72'b0, tx_count[0]}),
        .o_rx_parallel_data(rx_data[3]));
    eth eth (.i_rx_cdr_refclk_p(eth_refclk), .i_tx_pll_refclk_p(eth_refclk), .i_pma_cu_clk(cu_clk), .i_tx_reset(reset),
        .i_rx_reset(reset), .o_tx_reset_ack(), .o_rx_reset_ack(), .o_tx_ready(), .o_rx_ready(),
        .i_tx_coreclkin(tx_clk[1]), .i_rx_coreclkin(rx_clk[2]), .o_tx_clkout(tx_clk[1]), .o_rx_clkout(rx_clk[2]),
        .i_src_rs_grant(grant[1]), .o_src_rs_req(req[1]), .o_tx_serial_data(tx_p[2]), .o_tx_serial_data_n(tx_n[2]),
        .i_rx_serial_data(rx_p[2]), .i_rx_serial_data_n(rx_n[2]), .o_tx_pll_locked(), .o_rx_is_lockedtodata(),
        .o_rx_is_lockedtoref(), .i_tx_parallel_data({72'b0, tx_count[1]}), .o_rx_parallel_data(rx_data[2]));
    onu onu (.i_rx_cdr_refclk_p(pon_refclk), .i_tx_pll_refclk_p(pon_refclk), .i_pma_cu_clk(cu_clk), .i_tx_reset(reset),
        .i_rx_reset(reset), .o_tx_reset_ack(), .o_rx_reset_ack(), .o_tx_ready(), .o_rx_ready(),
        .i_tx_coreclkin(tx_clk[2]), .i_rx_coreclkin(rx_clk[1]), .o_tx_clkout(tx_clk[2]), .o_rx_clkout(rx_clk[1]),
        .i_src_rs_grant(grant[3]), .o_src_rs_req(req[3]), .o_tx_serial_data(tx_p[1]), .o_tx_serial_data_n(tx_n[1]),
        .i_rx_serial_data(rx_p[1]), .i_rx_serial_data_n(rx_n[1]), .o_tx_pll_locked(), .o_rx_is_lockedtodata(),
        .o_rx_is_lockedtoref(), .i_tx_parallel_data({72'b0, tx_count[2]}), .o_rx_parallel_data(rx_data[1]));
    ben_tx ben_tx (.i_tx_pll_refclk_p(pon_refclk), .i_pma_cu_clk(cu_clk), .i_tx_reset(reset), .o_tx_reset_ack(), .o_tx_ready(),
        .i_tx_coreclkin(tx_clk[3]), .o_tx_clkout(tx_clk[3]), .i_src_rs_grant(grant[2]), .o_src_rs_req(req[2]),
        .o_tx_serial_data(tx_p[0]), .o_tx_serial_data_n(tx_n[0]), .o_tx_pll_locked(), .i_tx_parallel_data({72'b0, tx_count[3]}));
    assign tx_clk[4] = tx_clk[0];
    assign rx_clk[0] = rx_clk[1];
    assign rx_data[0] = 80'b0;
endmodule

