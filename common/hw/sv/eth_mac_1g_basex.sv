// SPDX-License-Identifier: CERN-OHL-S-2.0
// taxi 1G MAC + 1000BASE-X/SGMII PCS with autonegotiation, flat ports (no SV interfaces) for SpinalHDL. SERDES side:
// one 8b/10b symbol per clock (data + K), the 8b/10b code itself outside.
`timescale 1ns / 1ps
`default_nettype none

module eth_mac_1g_basex (
    input  wire logic        rx_clk,
    input  wire logic        rx_rst,
    input  wire logic        tx_clk,
    input  wire logic        tx_rst,
    input  wire logic [7:0]  tx_tdata,
    input  wire logic        tx_tvalid,
    output wire logic        tx_tready,
    input  wire logic        tx_tlast,
    input  wire logic        tx_tuser,   // abort the frame
    output wire logic [7:0]  rx_tdata,
    output wire logic        rx_tvalid,
    output wire logic        rx_tlast,
    output wire logic        rx_tuser,   // bad frame (FCS, code error)
    output wire logic [7:0]  serdes_tx_data,
    output wire logic        serdes_tx_data_k,
    input  wire logic [7:0]  serdes_rx_data,
    input  wire logic        serdes_rx_data_k,
    input  wire logic        an_restart,
    input  wire logic        an_speedup, // simulation: short link timer
    input  wire logic        an_sgmii_en,
    output wire logic        an_complete,
    output wire logic        an_sgmii_mode,
    output wire logic [15:0] an_lp_adv_ability,
    output wire logic        rx_status,  // PCS synchronized
    output wire logic        stat_rx_pkt_good,
    output wire logic        stat_rx_pkt_bad,
    output wire logic        stat_tx_pkt_good
);

taxi_axis_if #(.DATA_W(8), .USER_EN(1), .USER_W(1), .ID_EN(1), .ID_W(8)) s_axis_tx();
taxi_axis_if #(.DATA_W(96), .KEEP_W(1), .ID_EN(1), .ID_W(8)) m_axis_tx_cpl();
taxi_axis_if #(.DATA_W(8), .USER_EN(1), .USER_W(1)) m_axis_rx();
taxi_axis_if #(.DATA_W(24), .KEEP_W(1), .LAST_EN(0), .USER_EN(1), .USER_W(1), .ID_EN(1), .ID_W(8)) m_axis_stat();

assign s_axis_tx.tdata = tx_tdata;
assign s_axis_tx.tkeep = 1'b1;
assign s_axis_tx.tstrb = 1'b1;
assign s_axis_tx.tvalid = tx_tvalid;
assign s_axis_tx.tlast = tx_tlast;
assign s_axis_tx.tuser = tx_tuser;
assign s_axis_tx.tid = '0;
assign s_axis_tx.tdest = '0;
assign tx_tready = s_axis_tx.tready;
assign m_axis_tx_cpl.tready = 1'b1;
assign rx_tdata = m_axis_rx.tdata;
assign rx_tvalid = m_axis_rx.tvalid;
assign rx_tlast = m_axis_rx.tlast;
assign rx_tuser = m_axis_rx.tuser[0];
assign m_axis_rx.tready = 1'b1;
assign m_axis_stat.tready = 1'b1;

taxi_eth_mac_phy_1g_basex #(
    .DATA_W(8),
    .SGMII_EN(1'b1),
    .AN_EN(1'b1),
    .PTP_TS_EN(1'b0),
    .STAT_EN(1'b0)
) mac (
    .rx_clk(rx_clk),
    .rx_rst(rx_rst),
    .tx_clk(tx_clk),
    .tx_rst(tx_rst),
    .s_axis_tx(s_axis_tx),
    .m_axis_tx_cpl(m_axis_tx_cpl),
    .m_axis_rx(m_axis_rx),
    .serdes_tx_data(serdes_tx_data),
    .serdes_tx_data_k(serdes_tx_data_k),
    .serdes_tx_data_dm(),
    .serdes_tx_data_dv(),
    .serdes_tx_data_valid(),
    .serdes_tx_gbx_sync(),
    .serdes_rx_data(serdes_rx_data),
    .serdes_rx_data_k(serdes_rx_data_k),
    .serdes_rx_reset_req(),
    .an_en(1'b1),
    .an_restart(an_restart),
    .an_speedup(an_speedup),
    .an_sgmii_en(an_sgmii_en),
    .an_intr(),
    .an_running(),
    .an_complete(an_complete),
    .an_timeout(),
    .an_sgmii_mode(an_sgmii_mode),
    .an_lp_adv_ability(an_lp_adv_ability),
    .an_lp_remote_fault(),
    .an_lp_sgmii_link(),
    .an_lp_sgmii_speed(),
    .an_res_full_duplex(),
    .an_res_tx_pause(),
    .an_res_rx_pause(),
    .tx_ptp_ts_out(),
    .tx_ptp_ts_step_out(),
    .tx_ptp_locked(),
    .rx_ptp_ts_out(),
    .rx_ptp_ts_step_out(),
    .rx_ptp_locked(),
    .rx_lfc_req(),
    .rx_pfc_req(),
    .tx_pause_ack(),
    .stat_clk(tx_clk),
    .stat_rst(tx_rst),
    .m_axis_stat(m_axis_stat),
    .tx_start_packet(),
    .stat_tx_byte(),
    .stat_tx_pkt_len(),
    .stat_tx_pkt_ucast(),
    .stat_tx_pkt_mcast(),
    .stat_tx_pkt_bcast(),
    .stat_tx_pkt_vlan(),
    .stat_tx_pkt_good(stat_tx_pkt_good),
    .stat_tx_pkt_bad(),
    .stat_tx_pad_frame(),
    .stat_tx_err_oversize(),
    .stat_tx_err_user(),
    .stat_tx_err_underflow(),
    .rx_start_packet(),
    .rx_error_count(),
    .rx_block_lock(),
    .rx_high_ber(),
    .rx_status(rx_status),
    .stat_rx_byte(),
    .stat_rx_pkt_len(),
    .stat_rx_pkt_fragment(),
    .stat_rx_pkt_jabber(),
    .stat_rx_pkt_ucast(),
    .stat_rx_pkt_mcast(),
    .stat_rx_pkt_bcast(),
    .stat_rx_pkt_vlan(),
    .stat_rx_pkt_good(stat_rx_pkt_good),
    .stat_rx_pkt_bad(stat_rx_pkt_bad),
    .stat_rx_err_oversize(),
    .stat_rx_err_bad_fcs(),
    .stat_rx_err_bad_block(),
    .stat_rx_err_framing(),
    .stat_rx_err_preamble(),
    .stat_tx_mcf(),
    .stat_rx_mcf(),
    .stat_tx_lfc_pkt(),
    .stat_tx_lfc_xon(),
    .stat_tx_lfc_xoff(),
    .stat_tx_lfc_paused(),
    .stat_tx_pfc_pkt(),
    .stat_tx_pfc_xon(),
    .stat_tx_pfc_xoff(),
    .stat_tx_pfc_paused(),
    .stat_rx_lfc_pkt(),
    .stat_rx_lfc_xon(),
    .stat_rx_lfc_xoff(),
    .stat_rx_lfc_paused(),
    .stat_rx_pfc_pkt(),
    .stat_rx_pfc_xon(),
    .stat_rx_pfc_xoff(),
    .stat_rx_pfc_paused()
);

endmodule

`resetall
