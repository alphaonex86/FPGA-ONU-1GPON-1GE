// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.io.InOutWrapper

/** DE25-Standard + HSMC card top: OLT (SFP cage lane), ONU (BOSA lanes: data + burst enable) and Ethernet (SFP cage
  * lane) side by side, each on its own HPS lightweight bridge slave. Every GPON lane runs from the VCXO; each PON side's
  * core clocks come from its lane's tx clock through a clock network divider (aligned 311.04 / 155.52 MHz); the
  * OLT lane switches by DR to 1.25 Gb/s from the XO (EPON OLT, 125 MHz core clock through the same divider). The
  * Ethernet lane runs from the XO, its tx and rx clocks fed back. The default clock domain is the HPS management clock. */
case class PonBoard() extends Component {
  val olt = OltBoardCore()
  val onu = OnuBoardCore()
  val eth = EthBoardCore()
  val io = new Bundle {
    val pon_refclk = in Bool () // VCXO 155.52 MHz
    val eth_refclk = in Bool () // XO 156.25 MHz
    val clock_50 = in Bool () // board oscillator, 50 MHz
    val olt_tx_p, olt_tx_n, onu_tx_p, onu_tx_n, ben_tx_p, ben_tx_n = out Bool ()
    val eth_tx_p, eth_tx_n = out Bool ()
    val olt_rx_p, olt_rx_n, onu_rx_p, onu_rx_n, eth_rx_p, eth_rx_n = in Bool ()
    val lwOlt, lwOnu, lwEth = slave(Axi4(HpsBridge.axiConfig))
    val sfp, ethSfp = master(SfpPins())
    val onuPins = master(OnuPins())
    val vcxoPwm = out Bool ()
    val oltPonIrq, oltBoardIrq, oltGemIrq = out Bool ()
    val onuPonIrq, onuBoardIrq, onuGemIrq, ethBoardIrq, ethFrameIrq = out Bool ()
    val oltBridgeIrq, onuBridgeIrq = out Bits (4 bits) // LAN, WAN, LAN control, WAN control frame queues
  }
  noIoPrefix()
  olt.io.axi <> io.lwOlt
  onu.io.axi <> io.lwOnu
  io.sfp <> olt.io.sfp
  io.onuPins <> onu.io.pins
  io.vcxoPwm := onu.io.vcxoPwm
  io.oltPonIrq := olt.io.ponIrq
  io.oltBoardIrq := olt.io.boardIrq
  io.oltGemIrq := olt.io.gemIrq
  io.oltBridgeIrq := olt.io.bridgeIrq
  io.onuPonIrq := onu.io.ponIrq
  io.onuBoardIrq := onu.io.boardIrq
  io.onuGemIrq := onu.io.gemIrq
  io.onuBridgeIrq := onu.io.bridgeIrq
  eth.io.axi <> io.lwEth
  io.ethSfp <> eth.io.sfp
  io.ethBoardIrq := eth.io.boardIrq
  io.ethFrameIrq := eth.io.frameIrq

  val srcss = vendor.GtsSrcss(4)
  srcss.i_src_rs_priority := 0
  srcss.i_src_rs_refclk_status_bus := 0
  srcss.i_refclk_on := 0
  olt.io.refclkFail := srcss.o_refclk_fail_status

  def duplex(gts: vendor.GtsDuplex, ctrl: GtsLaneCtrl, refclk: Bool, txCoreClk: Bool, rxCoreClk: Bool, txWord: Bits,
             rxWord: Bits): Unit = {
    gts.i_pma_cu_clk := srcss.o_pma_cu_clk
    gts.i_rx_cdr_refclk_p := refclk
    gts.i_tx_pll_refclk_p := refclk
    gts.i_tx_coreclkin := txCoreClk
    gts.i_rx_coreclkin := rxCoreClk
    gts.i_tx_parallel_data := B(0, 80 - txWord.getWidth bits) ## txWord
    rxWord := gts.o_rx_parallel_data(rxWord.getWidth - 1 downto 0)
    gts.i_tx_reset := ctrl.txReset
    gts.i_rx_reset := ctrl.rxReset
    if (gts.lockToRef) gts.i_rx_set_locktoref := ctrl.rxLockToRef
    ctrl.txResetAck := gts.o_tx_reset_ack
    ctrl.rxResetAck := gts.o_rx_reset_ack
    ctrl.txReady := gts.o_tx_ready
    ctrl.rxReady := gts.o_rx_ready
    ctrl.txPllLocked := gts.o_tx_pll_locked
    ctrl.rxLockedToData := gts.o_rx_is_lockedtodata
    ctrl.rxLockedToRef := gts.o_rx_is_lockedtoref
  }

  val oltLane = vendor.GtsDrLane()
  val drCtrl = vendor.GtsDrCtrl()
  drCtrl.i_rst_n := !ClockDomain.current.readResetWire
  drCtrl.i_csr_clk := ClockDomain.current.readClockWire
  drCtrl.i_cpu_clk := ClockDomain.current.readClockWire
  drCtrl.i_host_avmm_address := olt.io.dr.address
  drCtrl.i_host_avmm_read := olt.io.dr.read
  drCtrl.i_host_avmm_write := olt.io.dr.write
  drCtrl.i_host_avmm_writedata := olt.io.dr.writeData
  olt.io.dr.readData := drCtrl.o_host_avmm_readdata
  olt.io.dr.readDataValid := drCtrl.o_host_avmm_readdatavalid
  olt.io.dr.waitRequest := drCtrl.o_host_avmm_waitrequest
  oltLane.one_hot_sel := drCtrl.o_one_hot_sel
  oltLane.i_src_pause_request_ch0 := drCtrl.o_src_pause_request
  drCtrl.i_src_pause_grant := oltLane.o_src_pause_grant_ch0
  oltLane.i_dr_lavmm_clk_ch0 := ClockDomain.current.readClockWire
  oltLane.i_dr_lavmm_addr_ch0 := drCtrl.o_ch0_lavmm_addr
  oltLane.i_dr_lavmm_be_ch0 := drCtrl.o_ch0_lavmm_be
  oltLane.i_dr_lavmm_read_ch0 := drCtrl.o_ch0_lavmm_read
  oltLane.i_dr_lavmm_write_ch0 := drCtrl.o_ch0_lavmm_write
  oltLane.i_dr_lavmm_wdata_ch0 := drCtrl.o_ch0_lavmm_wdata
  oltLane.i_dr_lavmm_rstn_ch0 := drCtrl.o_ch0_lavmm_rstn
  drCtrl.i_ch0_lavmm_rdata := oltLane.o_dr_lavmm_rdata_ch0
  drCtrl.i_ch0_lavmm_rdata_valid := oltLane.o_dr_lavmm_rdata_valid_ch0
  drCtrl.i_ch0_lavmm_waitreq := oltLane.o_dr_lavmm_waitreq_ch0
  oltLane.i_pma_cu_clk_bank0 := srcss.o_pma_cu_clk
  val epon = drCtrl.o_one_hot_sel(0)
  olt.io.epon := epon

  val oltClock = vendor.ClockMux()
  oltClock.inclk0x := oltLane.gpon.o_tx_clkout
  oltClock.inclk1x := oltLane.oneG25.o_tx_clkout
  oltClock.clkselect := epon
  val oltClocks = vendor.PonClocks()
  oltClocks.inclk := oltClock.outclk
  olt.io.dsClk := oltClocks.clock_div1x
  olt.io.usClk := oltClocks.clock_div2x
  for ((p, refclk) <- Seq(oltLane.gpon -> io.pon_refclk, oltLane.oneG25 -> io.eth_refclk)) {
    p.i_tx_pll_refclk_p := refclk
    p.i_rx_cdr_refclk_p := refclk
    p.i_tx_coreclkin := oltClocks.clock_div1x
    p.i_rx_coreclkin := p.o_rx_clkout
    p.i_tx_reset := olt.io.lane.txReset
    p.i_rx_reset := olt.io.lane.rxReset
    p.i_rx_set_locktoref := olt.io.lane.rxLockToRef
  }
  oltLane.gpon.i_tx_parallel_data := B(0, 72 bits) ## olt.io.txWord(7 downto 0)
  oltLane.oneG25.i_tx_parallel_data := B(0, 70 bits) ## olt.io.txWord
  olt.io.gponRxClk := oltLane.gpon.o_rx_clkout
  olt.io.gponRxWord := oltLane.gpon.o_rx_parallel_data(7 downto 0)
  olt.io.eponRxClk := oltLane.oneG25.o_rx_clkout
  olt.io.eponRxWord := oltLane.oneG25.o_rx_parallel_data(9 downto 0)
  def active(f: oltLane.Profile => Bool): Bool = epon ? f(oltLane.oneG25) | f(oltLane.gpon)
  olt.io.lane.txResetAck := active(_.o_tx_reset_ack)
  olt.io.lane.rxResetAck := active(_.o_rx_reset_ack)
  olt.io.lane.txReady := active(_.o_tx_ready)
  olt.io.lane.rxReady := active(_.o_rx_ready)
  olt.io.lane.txPllLocked := active(_.o_tx_pll_locked)
  olt.io.lane.rxLockedToData := active(_.o_rx_is_lockedtodata)
  olt.io.lane.rxLockedToRef := active(_.o_rx_is_lockedtoref)
  io.olt_tx_p := oltLane.o_tx_serial_data_p_ch0
  io.olt_tx_n := oltLane.o_tx_serial_data_n_ch0
  oltLane.i_rx_serial_data_p_ch0 := io.olt_rx_p
  oltLane.i_rx_serial_data_n_ch0 := io.olt_rx_n

  val onuLane = vendor.GtsDuplex("onu")
  val onuClocks = vendor.PonClocks()
  onuClocks.inclk := onuLane.o_tx_clkout
  duplex(onuLane, onu.io.onuLane, io.pon_refclk, onuClocks.clock_div1x, onuClocks.clock_div1x, onu.io.txWord, onu.io.rxWord)
  onu.io.dsClk := onuClocks.clock_div1x
  onu.io.usClk := onuClocks.clock_div2x
  onu.io.rxClk := onuLane.o_rx_clkout
  val helperPll = vendor.HelperPll()
  helperPll.refclk := io.clock_50
  helperPll.rst := False
  onu.io.helperClk := helperPll.outclk_0
  onu.io.helperLocked := helperPll.locked
  io.onu_tx_p := onuLane.o_tx_serial_data
  io.onu_tx_n := onuLane.o_tx_serial_data_n
  onuLane.i_rx_serial_data := io.onu_rx_p
  onuLane.i_rx_serial_data_n := io.onu_rx_n

  val benLane = vendor.GtsTx("ben_tx")
  benLane.i_pma_cu_clk := srcss.o_pma_cu_clk
  benLane.i_tx_pll_refclk_p := io.pon_refclk
  benLane.i_tx_coreclkin := onuClocks.clock_div1x
  benLane.i_tx_parallel_data := B(0, 72 bits) ## onu.io.benWord
  benLane.i_tx_reset := onu.io.benLane.txReset
  val ben = onu.io.benLane
  ben.txResetAck := benLane.o_tx_reset_ack
  ben.txReady := benLane.o_tx_ready
  ben.txPllLocked := benLane.o_tx_pll_locked
  for (b <- Seq(ben.rxResetAck, ben.rxReady, ben.rxLockedToData, ben.rxLockedToRef)) b := False
  io.ben_tx_p := benLane.o_tx_serial_data
  io.ben_tx_n := benLane.o_tx_serial_data_n

  val ethLane = vendor.GtsDuplex("eth")
  duplex(ethLane, eth.io.lane, io.eth_refclk, ethLane.o_tx_clkout, ethLane.o_rx_clkout, eth.io.txWord, eth.io.rxWord)
  eth.io.txClk := ethLane.o_tx_clkout
  eth.io.rxClk := ethLane.o_rx_clkout
  val ethDown = !ethLane.o_tx_ready || !ethLane.o_rx_ready
  for ((txClk, rxClk, down) <- Seq((olt.io.ethTxClk, olt.io.ethRxClk, olt.io.ethDown), (onu.io.ethTxClk, onu.io.ethRxClk, onu.io.ethDown))) {
    txClk := ethLane.o_tx_clkout
    rxClk := ethLane.o_rx_clkout
    down := ethDown
  }
  eth.io.olt <> olt.io.eth
  eth.io.onu <> onu.io.eth
  io.eth_tx_p := ethLane.o_tx_serial_data
  io.eth_tx_n := ethLane.o_tx_serial_data_n
  ethLane.i_rx_serial_data := io.eth_rx_p
  ethLane.i_rx_serial_data_n := io.eth_rx_n

  srcss.i_src_rs_req := ethLane.o_src_rs_req ## benLane.o_src_rs_req ## onuLane.o_src_rs_req ## oltLane.o_rs_request_ch0
  oltLane.i_rs_grant_ch0 := srcss.o_src_rs_grant(0)
  onuLane.i_src_rs_grant := srcss.o_src_rs_grant(1)
  benLane.i_src_rs_grant := srcss.o_src_rs_grant(2)
  ethLane.i_src_rs_grant := srcss.o_src_rs_grant(3)
}

object PonBoardVerilog extends App {
  val report = SpinalConfig(targetDirectory = "out/rtl").generateVerilog {
    val top = InOutWrapper(PonBoard()) // TriState -> inout pins
    vendor.Quartus.ramStyles(top)
    top
  }
  val sources = new java.io.PrintWriter("out/rtl/PonBoard.sources") // black box RTL, for fit.sh
  report.blackboxesSourcesPaths.foreach(sources.println)
  sources.close()
}
