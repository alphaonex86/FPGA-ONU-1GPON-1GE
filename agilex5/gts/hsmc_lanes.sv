// SPDX-License-Identifier: CERN-OHL-S-2.0
// Fit check of the HSMC card lanes on the DE25-Standard: every GTS lane at its rate, from its refclk pin. The OLT
// lane's tx_clkout through the clock divider (311.04 and 155.52 MHz) clocks every PON lane's core, as the MACs run;
// Ethernet keeps its own clocks. Counters keep the datapaths alive.
module hsmc_lanes (
    input  wire       pon_refclk,   // VCXO 155.52 MHz (HSMC_GTS_RX_REF)
    input  wire       eth_refclk,   // XO 156.25 MHz (HSMC_GTS_REFCLK)
    input  wire       reset,
    output wire       olt_tx_p,
    output wire       olt_tx_n,
    input  wire       olt_rx_p,
    input  wire       olt_rx_n,
    output wire       eth_tx_p,
    output wire       eth_tx_n,
    input  wire       eth_rx_p,
    input  wire       eth_rx_n,
    output wire       onu_tx_p,
    output wire       onu_tx_n,
    input  wire       onu_rx_p,
    input  wire       onu_rx_n,
    output wire       ben_tx_p,
    output wire       ben_tx_n,
    output wire [7:0] status
);
    wire [3:0] req, grant;
    wire       cu_clk;
    srcss srcss (.o_src_rs_grant(grant), .i_src_rs_priority(4'b0), .i_src_rs_req(req), .o_pma_cu_clk(cu_clk),
                 .i_src_rs_refclk_status_bus(10'b0), .o_refclk_fail_status(), .o_refclk_on_ack(), .i_refclk_on(10'b0),
                 .o_src_rs_refclk_cmd_bus());

    wire olt_clk, ds_clk, us_clk, eth_tx_clk, eth_rx_clk;
    pon_clocks pon_clocks (.inclk(olt_clk), .clock_div1x(ds_clk), .clock_div2x(us_clk));
    reg  [7:0] ds_count, us_count, eth_count;
    always @(posedge ds_clk) ds_count <= ds_count + 8'd1;
    always @(posedge us_clk) us_count <= us_count + 8'd1;
    always @(posedge eth_tx_clk) eth_count <= eth_count + 8'd1;

    wire [79:0] olt_rx, onu_rx, eth_rx;
    reg  [2:0] rx_bit;
    always @(posedge ds_clk) rx_bit[1:0] <= {^olt_rx[7:0], ^onu_rx[7:0]};
    always @(posedge eth_rx_clk) rx_bit[2] <= ^eth_rx[9:0];
    assign status = {us_count[7:3], rx_bit};

    olt olt (.i_rx_cdr_refclk_p(pon_refclk), .i_tx_pll_refclk_p(pon_refclk), .i_pma_cu_clk(cu_clk), .i_tx_reset(reset),
        .i_rx_reset(reset), .o_tx_reset_ack(), .o_rx_reset_ack(), .o_tx_ready(), .o_rx_ready(),
        .i_tx_coreclkin(ds_clk), .i_rx_coreclkin(ds_clk), .o_tx_clkout(olt_clk), .o_rx_clkout(),
        .i_src_rs_grant(grant[0]), .o_src_rs_req(req[0]), .o_tx_serial_data(olt_tx_p), .o_tx_serial_data_n(olt_tx_n),
        .i_rx_serial_data(olt_rx_p), .i_rx_serial_data_n(olt_rx_n), .o_tx_pll_locked(), .o_rx_is_lockedtodata(),
        .o_rx_is_lockedtoref(), .i_rx_set_locktoref(1'b0), .i_tx_parallel_data({72'b0, ds_count}),
        .o_rx_parallel_data(olt_rx));
    eth eth (.i_rx_cdr_refclk_p(eth_refclk), .i_tx_pll_refclk_p(eth_refclk), .i_pma_cu_clk(cu_clk), .i_tx_reset(reset),
        .i_rx_reset(reset), .o_tx_reset_ack(), .o_rx_reset_ack(), .o_tx_ready(), .o_rx_ready(),
        .i_tx_coreclkin(eth_tx_clk), .i_rx_coreclkin(eth_rx_clk), .o_tx_clkout(eth_tx_clk), .o_rx_clkout(eth_rx_clk),
        .i_src_rs_grant(grant[1]), .o_src_rs_req(req[1]), .o_tx_serial_data(eth_tx_p), .o_tx_serial_data_n(eth_tx_n),
        .i_rx_serial_data(eth_rx_p), .i_rx_serial_data_n(eth_rx_n), .o_tx_pll_locked(), .o_rx_is_lockedtodata(),
        .o_rx_is_lockedtoref(), .i_tx_parallel_data({72'b0, eth_count}), .o_rx_parallel_data(eth_rx));
    onu onu (.i_rx_cdr_refclk_p(pon_refclk), .i_tx_pll_refclk_p(pon_refclk), .i_pma_cu_clk(cu_clk), .i_tx_reset(reset),
        .i_rx_reset(reset), .o_tx_reset_ack(), .o_rx_reset_ack(), .o_tx_ready(), .o_rx_ready(),
        .i_tx_coreclkin(ds_clk), .i_rx_coreclkin(ds_clk), .o_tx_clkout(), .o_rx_clkout(),
        .i_src_rs_grant(grant[3]), .o_src_rs_req(req[3]), .o_tx_serial_data(onu_tx_p), .o_tx_serial_data_n(onu_tx_n),
        .i_rx_serial_data(onu_rx_p), .i_rx_serial_data_n(onu_rx_n), .o_tx_pll_locked(), .o_rx_is_lockedtodata(),
        .o_rx_is_lockedtoref(), .i_tx_parallel_data({72'b0, ~ds_count}), .o_rx_parallel_data(onu_rx));
    ben_tx ben_tx (.i_tx_pll_refclk_p(pon_refclk), .i_pma_cu_clk(cu_clk), .i_tx_reset(reset), .o_tx_reset_ack(), .o_tx_ready(),
        .i_tx_coreclkin(ds_clk), .o_tx_clkout(), .i_src_rs_grant(grant[2]), .o_src_rs_req(req[2]),
        .o_tx_serial_data(ben_tx_p), .o_tx_serial_data_n(ben_tx_n), .o_tx_pll_locked(), .i_tx_parallel_data({72'b0, ds_count ^ 8'h5A}));
endmodule
