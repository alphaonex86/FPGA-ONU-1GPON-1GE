// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.io.InOutWrapper

/** DE25-Standard + HSMC card top: OLT (SFP cage 0 lane: GPON or EPON OLT module, or an Ethernet module on the OLT
  * core's own port), ONU (BOSA lanes: data + burst enable) and Ethernet (SFP cage 1 lane) side by side, each on its own
  * HPS lightweight bridge slave. Every GPON lane runs from the 155.52 MHz VCXO;
  * each PON side's core clocks come from its lane's tx clock through a clock network divider (aligned 311.04 / 155.52
  * MHz). One DR controller switches each PON side on its own to 1.25 Gb/s from the 156.25 MHz VCXO (EPON OLT or Ethernet, or EPON
  * ONU with its burst enable lane; 125 MHz core clock through the same divider). The Ethernet lane runs from the 156.25 MHz VCXO, its tx and rx clocks
  * fed back. The default clock domain is the HPS management clock. */
case class PonBoard() extends Component {
  val olt = OltBoardCore().setName("oltCore") // fixed names for fit.sdc (the IP module onu exists)
  val onu = OnuBoardCore().setName("onuCore")
  val eth = EthBoardCore().setName("ethCore")
  val io = new Bundle {
    val pon_refclk = in Bool () // VCXO 155.52 MHz
    val eth_refclk = in Bool () // VCXO 156.25 MHz
    val clock_50 = in Bool () // board oscillator, 50 MHz
    val sfp0_tx_p, sfp0_tx_n, onu_tx_p, onu_tx_n, ben_tx_p, ben_tx_n = out Bool ()
    val sfp1_tx_p, sfp1_tx_n = out Bool ()
    val sfp0_rx_p, sfp0_rx_n, onu_rx_p, onu_rx_n, ben_rx_p, ben_rx_n, sfp1_rx_p, sfp1_rx_n = in Bool ()
    val lwOlt, lwOnu, lwEth = slave(Axi4(HpsBridge.axiConfig))
    val sfp0, sfp1 = master(SfpPins())
    val onuPins = master(OnuPins())
    val vcxoPwm, vcxo156Pwm = out Bool ()
    val oltPonIrq, oltBoardIrq, oltGemIrq = out Bool ()
    val onuPonIrq, onuBoardIrq, onuGemIrq, ethBoardIrq, ethFrameIrq = out Bool ()
    val oltBridgeIrq, onuBridgeIrq = out Bits (4 bits) // LAN, WAN, LAN control, WAN control frame queues
    val oltEthPortIrq, onuSnifferIrq = out Bool () // last: the HPS interrupt numbers of the others stay
  }
  noIoPrefix()
  olt.io.axi <> io.lwOlt
  onu.io.axi <> io.lwOnu
  io.sfp0 <> olt.io.sfp
  io.onuPins <> onu.io.pins
  io.vcxoPwm := onu.io.vcxoPwm
  io.vcxo156Pwm := onu.io.vcxo156Pwm
  io.oltPonIrq := olt.io.ponIrq
  io.oltBoardIrq := olt.io.boardIrq
  io.oltGemIrq := olt.io.gemIrq
  io.oltEthPortIrq := olt.io.ethPortIrq
  io.oltBridgeIrq := olt.io.bridgeIrq
  io.onuPonIrq := onu.io.ponIrq
  io.onuBoardIrq := onu.io.boardIrq
  io.onuGemIrq := onu.io.gemIrq
  io.onuSnifferIrq := onu.io.snifferIrq
  io.onuBridgeIrq := onu.io.bridgeIrq
  eth.io.axi <> io.lwEth
  io.sfp1 <> eth.io.sfp
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

  val ponLanes = vendor.GtsDrLanes()
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
  ponLanes.one_hot_sel := drCtrl.o_one_hot_sel
  ponLanes.i_pma_cu_clk_bank0 := srcss.o_pma_cu_clk
  val channels = Seq(ponLanes.sfp0, ponLanes.onu, ponLanes.ben)
  for (((channel, lavmm), k) <- channels.zip(Seq(drCtrl.ch0, drCtrl.ch1, drCtrl.ch2)).zipWithIndex) {
    lavmm.drive(channel, ClockDomain.current.readClockWire)
    channel.i_src_pause_request := drCtrl.o_src_pause_request(k)
  }
  drCtrl.i_src_pause_grant := Vec(channels.map(_.o_src_pause_grant)).asBits

  /** A PON side on its DR lane: core clocks from the active profile's tx clock (each profile's refclk), its reset and
    * data to both profiles, the active one's status. The received words come on the profile's rx clock (OLT: bursts)
    * or, through the GTS RX FIFO, on the core clock (ONU: kept on the received clock by its VCXO loop). */
  class DrSide(channel: vendor.GtsDrLanes#Channel, gpon: vendor.GtsDrLanes#Profile, epon: vendor.GtsDrLanes#Profile,
               val isEpon: Bool, lane: GtsLaneCtrl, txWord: Bits, rxOnCoreClock: Boolean, tx: (Bool, Bool), rx: (Bool, Bool)) extends Area {
    val clock = vendor.ClockMux()
    clock.inclk0x := gpon.o_tx_clkout
    clock.inclk1x := epon.o_tx_clkout
    clock.clkselect := isEpon
    val clocks = vendor.PonClocks()
    clocks.inclk := clock.outclk
    for ((p, refclk) <- Seq(gpon -> io.pon_refclk, epon -> io.eth_refclk)) {
      p.i_tx_pll_refclk_p := refclk
      p.i_rx_cdr_refclk_p := refclk
      p.i_tx_coreclkin := clocks.clock_div1x
      p.i_rx_coreclkin := (if (rxOnCoreClock) clocks.clock_div1x else p.o_rx_clkout)
      p.i_tx_reset := lane.txReset
      p.i_rx_reset := lane.rxReset
      if (p.lockToRef) p.i_rx_set_locktoref := lane.rxLockToRef
    }
    gpon.i_tx_parallel_data := B(0, 72 bits) ## txWord(7 downto 0)
    epon.i_tx_parallel_data := B(0, 70 bits) ## txWord
    def active[T <: Data](f: vendor.GtsDrLanes#Profile => T): T = isEpon ? f(epon) | f(gpon)
    lane.txResetAck := active(_.o_tx_reset_ack)
    lane.rxResetAck := active(_.o_rx_reset_ack)
    lane.txReady := active(_.o_tx_ready)
    lane.rxReady := active(_.o_rx_ready)
    lane.txPllLocked := active(_.o_tx_pll_locked)
    lane.rxLockedToData := active(_.o_rx_is_lockedtodata)
    lane.rxLockedToRef := active(_.o_rx_is_lockedtoref)
    tx._1 := channel.o_tx_serial_data_p
    tx._2 := channel.o_tx_serial_data_n
    channel.i_rx_serial_data_p := rx._1
    channel.i_rx_serial_data_n := rx._2
  }

  val oltSide = new DrSide(ponLanes.sfp0, ponLanes.sfp0Gpon, ponLanes.sfp0Epon, drCtrl.o_one_hot_sel(4), olt.io.lane,
    olt.io.txWord, rxOnCoreClock = false, (io.sfp0_tx_p, io.sfp0_tx_n), (io.sfp0_rx_p, io.sfp0_rx_n))
  olt.io.epon := oltSide.isEpon
  olt.io.dsClk := oltSide.clocks.clock_div1x
  olt.io.usClk := oltSide.clocks.clock_div2x
  olt.io.gponRxClk := ponLanes.sfp0Gpon.o_rx_clkout
  olt.io.gponRxWord := ponLanes.sfp0Gpon.o_rx_parallel_data(7 downto 0)
  olt.io.eponRxClk := ponLanes.sfp0Epon.o_rx_clkout
  olt.io.eponRxWord := ponLanes.sfp0Epon.o_rx_parallel_data(9 downto 0)

  val onuSide = new DrSide(ponLanes.onu, ponLanes.onuGpon, ponLanes.onuEpon, drCtrl.o_one_hot_sel(2), onu.io.onuLane,
    onu.io.txWord, rxOnCoreClock = true, (io.onu_tx_p, io.onu_tx_n), (io.onu_rx_p, io.onu_rx_n))
  val onuClocks = onuSide.clocks
  onu.io.epon := onuSide.isEpon
  onu.io.dsClk := onuClocks.clock_div1x
  onu.io.usClk := onuClocks.clock_div2x
  onu.io.rxWord := onuSide.active(_.o_rx_parallel_data(9 downto 0))
  onu.io.gponRxClk := ponLanes.onuGpon.o_rx_clkout
  onu.io.eponRxClk := ponLanes.onuEpon.o_rx_clkout
  val helperPll = vendor.HelperPll()
  helperPll.refclk := io.clock_50
  helperPll.rst := False
  onu.io.helperClk := helperPll.outclk_0
  onu.io.eponHelperClk := helperPll.outclk_1
  onu.io.helperLocked := helperPll.locked

  // the burst enable lane switches with the ONU lane (DR combinations): always on the ONU core clock, bit-aligned;
  // its receiver (the upstream sniffer) through the GTS RX FIFO on that clock too, as the ONU lane's
  val ben = onu.io.benLane
  for ((p, refclk) <- Seq(ponLanes.benGpon -> io.pon_refclk, ponLanes.benEpon -> io.eth_refclk)) {
    p.i_tx_pll_refclk_p := refclk
    p.i_rx_cdr_refclk_p := refclk
    p.i_tx_coreclkin := onuClocks.clock_div1x
    p.i_rx_coreclkin := onuClocks.clock_div1x
    p.i_tx_reset := ben.txReset
    p.i_rx_reset := ben.rxReset
    p.i_rx_set_locktoref := ben.rxLockToRef
  }
  ponLanes.benGpon.i_tx_parallel_data := B(0, 72 bits) ## onu.io.benWord(7 downto 0)
  ponLanes.benEpon.i_tx_parallel_data := B(0, 70 bits) ## onu.io.benWord
  def benActive[T <: Data](f: vendor.GtsDrLanes#Profile => T): T = onuSide.isEpon ? f(ponLanes.benEpon) | f(ponLanes.benGpon)
  ben.txResetAck := benActive(_.o_tx_reset_ack)
  ben.rxResetAck := benActive(_.o_rx_reset_ack)
  ben.txReady := benActive(_.o_tx_ready)
  ben.rxReady := benActive(_.o_rx_ready)
  ben.txPllLocked := benActive(_.o_tx_pll_locked)
  ben.rxLockedToData := benActive(_.o_rx_is_lockedtodata)
  ben.rxLockedToRef := benActive(_.o_rx_is_lockedtoref)
  onu.io.benRxWord := benActive(_.o_rx_parallel_data(9 downto 0))
  io.ben_tx_p := ponLanes.ben.o_tx_serial_data_p
  io.ben_tx_n := ponLanes.ben.o_tx_serial_data_n
  ponLanes.ben.i_rx_serial_data_p := io.ben_rx_p
  ponLanes.ben.i_rx_serial_data_n := io.ben_rx_n

  val ethLane = vendor.GtsDuplex("sfp1")
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
  io.sfp1_tx_p := ethLane.o_tx_serial_data
  io.sfp1_tx_n := ethLane.o_tx_serial_data_n
  ethLane.i_rx_serial_data := io.sfp1_rx_p
  ethLane.i_rx_serial_data_n := io.sfp1_rx_n

  srcss.i_src_rs_req := ethLane.o_src_rs_req ## Vec(channels.map(_.o_rs_request)).asBits
  for ((channel, k) <- channels.zipWithIndex) channel.i_rs_grant := srcss.o_src_rs_grant(k)
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
