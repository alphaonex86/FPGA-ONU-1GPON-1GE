// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.io.InOutWrapper

/** DE25-Standard + HSMC card top: OLT (SFP cage lane), ONU (BOSA lanes: data + burst enable) and Ethernet (SFP cage
  * lane) side by side, each on its own HPS lightweight bridge slave. Every PON lane runs from the VCXO; each PON side's
  * core clocks come from its lane's tx clock through a clock network divider (aligned 311.04 / 155.52 MHz); the
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
    val oltPonIrq, oltBoardIrq, oltGemIrq, onuPonIrq, onuBoardIrq, onuGemIrq, ethBoardIrq, ethFrameIrq = out Bool ()
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
  io.onuPonIrq := onu.io.ponIrq
  io.onuBoardIrq := onu.io.boardIrq
  io.onuGemIrq := onu.io.gemIrq
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

  val oltLane = vendor.GtsDuplex("olt", lockToRef = true)
  val oltClocks = vendor.PonClocks()
  oltClocks.inclk := oltLane.o_tx_clkout
  duplex(oltLane, olt.io.lane, io.pon_refclk, oltClocks.clock_div1x, oltClocks.clock_div1x, olt.io.txWord, olt.io.rxWord)
  olt.io.dsClk := oltClocks.clock_div1x
  olt.io.usClk := oltClocks.clock_div2x
  io.olt_tx_p := oltLane.o_tx_serial_data
  io.olt_tx_n := oltLane.o_tx_serial_data_n
  oltLane.i_rx_serial_data := io.olt_rx_p
  oltLane.i_rx_serial_data_n := io.olt_rx_n

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

  srcss.i_src_rs_req := ethLane.o_src_rs_req ## benLane.o_src_rs_req ## onuLane.o_src_rs_req ## oltLane.o_src_rs_req
  oltLane.i_src_rs_grant := srcss.o_src_rs_grant(0)
  onuLane.i_src_rs_grant := srcss.o_src_rs_grant(1)
  benLane.i_src_rs_grant := srcss.o_src_rs_grant(2)
  ethLane.i_src_rs_grant := srcss.o_src_rs_grant(3)
}

object PonBoardVerilog extends App {
  val report = SpinalConfig(targetDirectory = "out/rtl").generateVerilog(InOutWrapper(PonBoard())) // TriState -> inout pins
  val sources = new java.io.PrintWriter("out/rtl/PonBoard.sources") // black box RTL, for fit.sh
  report.blackboxesSourcesPaths.foreach(sources.println)
  sources.close()
}
