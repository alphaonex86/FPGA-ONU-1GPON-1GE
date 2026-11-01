// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif._
import spinal.lib.io.TriState

/** Reset and status of one GTS lane (signals of vendor.GtsDuplex). */
case class GtsLaneCtrl() extends Bundle with IMasterSlave {
  val txReset, rxReset, rxLockToRef = Bool()
  val txResetAck, rxResetAck, txReady, rxReady, txPllLocked, rxLockedToData, rxLockedToRef = Bool()
  override def asMaster(): Unit = {
    out(txReset, rxReset, rxLockToRef)
    in(txResetAck, rxResetAck, txReady, rxReady, txPllLocked, rxLockedToData, rxLockedToRef)
  }
}

/** OLT SFP cage pins. RS0/RS1 (pins 7, 9) stay hi-Z until software knows the module. */
case class SfpPins() extends Bundle with IMasterSlave {
  val txDisable = Bool()
  val modAbs, los, txFault = Bool()
  val rs0, rs1, scl, sda = TriState(Bool())
  override def asMaster(): Unit = {
    out(txDisable)
    in(modAbs, los, txFault)
    master(rs0, rs1, scl, sda)
  }
}

/** Board registers (APB3, management domain): GTS lane reset and status, OLT SFP cage pins, bit-banged module I2C.
  * Lane reset by software: set the reset bit, wait its ack, clear it, wait ready. EVENTS bits are cleared by
  * writing 1. */
case class BoardRegs() extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(Apb3Config(addressWidth = 8, dataWidth = 32)))
    val lane = master(GtsLaneCtrl())
    val refclkFail = in Bits (8 bits)
    val sfp = master(SfpPins())
    val burstReset = in Bool () // OltMac.rxReset, for an RS pin in burst reset mode
    val alive = out Bool () // the PON clocks run: the MAC registers answer
    val irq = out Bool ()
  }
  val busif = BusInterface(io.apb, SizeMapping(0, 256))
  def synced(pin: Bool): Bool = BufferCC(pin, False)

  val GTS_CTRL = busif.newReg("OLT lane control")
  io.lane.txReset := GTS_CTRL.field(Bool(), AccessType.RW, 0, "TX reset (stops the PON clocks)")(SymbolName("txReset"))
  io.lane.rxReset := GTS_CTRL.field(Bool(), AccessType.RW, 0, "RX reset")(SymbolName("rxReset"))
  io.lane.rxLockToRef := GTS_CTRL.field(Bool(), AccessType.RW, 0, "RX CDR locked to the reference")(SymbolName("lockToRef"))
  io.alive := synced(io.lane.txReady) && !io.lane.txReset

  val GTS_STATUS = busif.newReg("OLT lane status")
  for ((name, pin) <- Seq("txResetAck" -> io.lane.txResetAck, "rxResetAck" -> io.lane.rxResetAck,
         "txReady" -> io.lane.txReady, "rxReady" -> io.lane.rxReady, "txPllLocked" -> io.lane.txPllLocked,
         "rxLockedToData" -> io.lane.rxLockedToData, "rxLockedToRef" -> io.lane.rxLockedToRef))
    GTS_STATUS.field(Bool(), AccessType.RO, 0, name)(SymbolName(name)) := synced(pin)
  GTS_STATUS.field(Bits(8 bits), AccessType.RO, 0, "refclkFail: per reference clock")(SymbolName("refclkFail")) :=
    BufferCC(io.refclkFail, B(0, 8 bits))

  val SFP_CTRL = busif.newReg("OLT SFP cage")
  io.sfp.txDisable := SFP_CTRL.field(Bool(), AccessType.RW, 1, "TX_DIS, also forced while not alive")(SymbolName("txDisable")) || !io.alive
  def rsPin(pin: TriState[Bool], name: String): Unit = {
    val mode = SFP_CTRL.field(UInt(2 bits), AccessType.RW, 0, s"$name: 0 hi-Z, 1 low, 2 high, 3 burst reset")(SymbolName(name))
    pin.writeEnable := mode =/= 0
    pin.write := (mode === 3) ? io.burstReset | mode(1)
  }
  rsPin(io.sfp.rs0, "rs0")
  rsPin(io.sfp.rs1, "rs1")
  for ((pin, name) <- Seq(io.sfp.scl -> "sclLow", io.sfp.sda -> "sdaLow")) {
    pin.writeEnable := SFP_CTRL.field(Bool(), AccessType.RW, 0, s"$name: pull the open-drain line low")(SymbolName(name))
    pin.write := False
  }

  val STATUS = busif.newReg("OLT SFP cage pins, alive: the PON clocks run and the MAC registers answer")
  val pins = Seq("modAbs" -> synced(io.sfp.modAbs), "los" -> synced(io.sfp.los), "txFault" -> synced(io.sfp.txFault),
    "alive" -> io.alive)
  for ((name, pin) <- pins :+ ("scl" -> synced(io.sfp.scl.read)) :+ ("sda" -> synced(io.sfp.sda.read)))
    STATUS.field(Bool(), AccessType.RO, 0, name)(SymbolName(name)) := pin

  val EVENTS = busif.newReg("Sticky pin changes, write 1 to clear")
  val clear = EVENTS.hitDoWrite ? busif.writeData | B(0, 32 bits)
  val events = for (((name, pin), k) <- pins.zipWithIndex) yield {
    val bit = RegInit(False)
    when(clear(k))(bit := False)
    when(pin =/= RegNext(pin).init(pin.getZero))(bit := True)
    EVENTS.field(Bool(), AccessType.RO, 0, s"$name changed")(SymbolName(name)) := bit
    bit
  }
  val IRQ_ENABLE = busif.newReg("Interrupt sources: EVENTS bits")
  val enables = IRQ_ENABLE.field(Bits(pins.length bits), AccessType.RW, 0, "same bits as EVENTS")(SymbolName("events"))
  io.irq := RegNext((Vec(events).asBits & enables).orR) init (False)

  busif.accept(DocCHeader("board_regs", "BOARD")) // board_regs.h next to the Verilog
}

/** OLT side of the board without the vendor blocks: HPS lightweight bridge (AXI4) -> APB3, 0x000 OltMac registers
  * (across to the upstream domain; an error answer while the PON clocks are down, so the CPU never hangs), 0x100
  * board registers; single 32-bit accesses only (readl/writel). The lane's tx clock (311.04 MHz) and half clock clock
  * the MAC, reset until the lane is ready. */
case class OltBoardCore() extends Component {
  val bridge = Axi4SharedToApb3Bridge(addressWidth = 9, dataWidth = 32, idWidth = 4)
  val io = new Bundle {
    val axi = slave(Axi4(bridge.io.axi.config))
    val dsClk, usClk = in Bool ()
    val lane = master(GtsLaneCtrl())
    val txWord = out Bits (8 bits)
    val rxWord = in Bits (8 bits)
    val refclkFail = in Bits (8 bits)
    val sfp = master(SfpPins())
    val ponIrq, boardIrq = out Bool ()
  }
  bridge.io.axi << io.axi.toShared()
  val apb = bridge.io.apb

  val board = BoardRegs()
  io.lane <> board.io.lane
  board.io.refclkFail := io.refclkFail
  io.sfp <> board.io.sfp
  io.boardIrq := board.io.irq

  val down = !io.lane.txReady // asynchronous: each domain leaves reset on its own clock
  val dsReset = ClockDomain(io.dsClk)(BufferCC(down))
  val usReset = ClockDomain(io.usClk)(BufferCC(down))
  val dsDomain = ClockDomain(io.dsClk, dsReset)
  val usDomain = ClockDomain(io.usClk, usReset)
  dsDomain.setSynchronousWith(usDomain)

  val pon = new ClockingArea(dsDomain) {
    val lanes = GtsPonLanes()
    lanes.io.usClk := io.usClk
    lanes.io.usReset := usReset
    io.txWord := lanes.io.oltTxWord
    lanes.io.oltRxWord := io.rxWord
    lanes.io.onuRxWord := 0
    lanes.io.onuUsLine := 0
    lanes.io.onuUsBen := 0

    val mac = gpon.OltMac()
    mac.io.usClk := io.usClk
    mac.io.usReset := usReset
    lanes.io.oltDsLine := mac.io.dsLine
    mac.io.usLine := lanes.io.oltUsLine
    mac.io.gem.valid := False
    mac.io.gem.payload.assignDontCare()
  }
  board.io.burstReset := pon.mac.io.rxReset
  io.ponIrq := BufferCC(pon.mac.io.irq, False)

  val cc = Apb3CC(pon.mac.io.apb.config, ClockDomain.current, usDomain)
  pon.mac.io.apb <> cc.io.output
  val toMac = !apb.PADDR(8)
  val macUp = board.io.alive
  for (slave <- Seq(cc.io.input, board.io.apb)) {
    slave.PADDR := apb.PADDR.resized
    slave.PENABLE := apb.PENABLE
    slave.PWRITE := apb.PWRITE
    slave.PWDATA := apb.PWDATA
  }
  cc.io.input.PSEL(0) := apb.PSEL(0) && toMac && macUp
  board.io.apb.PSEL(0) := apb.PSEL(0) && !toMac
  apb.PREADY := toMac ? (cc.io.input.PREADY || !macUp) | board.io.apb.PREADY
  apb.PRDATA := toMac ? cc.io.input.PRDATA | board.io.apb.PRDATA
  val error = toMac ? (cc.io.input.PSLVERROR || !macUp) | board.io.apb.PSLVERROR
  apb.PSLVERROR := RegNextWhen(error, apb.PSEL(0) && apb.PENABLE && apb.PREADY) init (False) // the bridge reads it after the access
}

/** OLT board top, DE25-Standard + HSMC card: OltBoardCore on the OLT GTS lane. The default clock domain is the HPS
  * management clock of the lightweight bridge (lw). */
case class OltBoard() extends Component {
  val core = OltBoardCore()
  val io = new Bundle {
    val pon_refclk = in Bool () // VCXO 155.52 MHz
    val olt_tx_p, olt_tx_n = out Bool ()
    val olt_rx_p, olt_rx_n = in Bool ()
    val lw = slave(Axi4(core.io.axi.config))
    val sfp = master(SfpPins())
    val ponIrq, boardIrq = out Bool ()
  }
  noIoPrefix()
  core.io.axi <> io.lw
  io.sfp <> core.io.sfp
  io.ponIrq := core.io.ponIrq
  io.boardIrq := core.io.boardIrq

  val srcss = vendor.GtsSrcss(4)
  srcss.i_src_rs_priority := 0
  srcss.i_src_rs_refclk_status_bus := 0
  srcss.i_refclk_on := 0
  core.io.refclkFail := srcss.o_refclk_fail_status

  val olt = vendor.GtsDuplex("olt", lockToRef = true)
  srcss.i_src_rs_req := B(0, 3 bits) ## olt.o_src_rs_req
  olt.i_src_rs_grant := srcss.o_src_rs_grant(0)
  olt.i_pma_cu_clk := srcss.o_pma_cu_clk
  olt.i_rx_cdr_refclk_p := io.pon_refclk
  olt.i_tx_pll_refclk_p := io.pon_refclk
  val clocks = vendor.PonClocks()
  clocks.inclk := olt.o_tx_clkout
  olt.i_tx_coreclkin := clocks.clock_div1x
  olt.i_rx_coreclkin := clocks.clock_div1x
  core.io.dsClk := clocks.clock_div1x
  core.io.usClk := clocks.clock_div2x
  io.olt_tx_p := olt.o_tx_serial_data
  io.olt_tx_n := olt.o_tx_serial_data_n
  olt.i_rx_serial_data := io.olt_rx_p
  olt.i_rx_serial_data_n := io.olt_rx_n
  olt.i_tx_parallel_data := B(0, 72 bits) ## core.io.txWord
  core.io.rxWord := olt.o_rx_parallel_data(7 downto 0)

  olt.i_tx_reset := core.io.lane.txReset
  olt.i_rx_reset := core.io.lane.rxReset
  olt.i_rx_set_locktoref := core.io.lane.rxLockToRef
  core.io.lane.txResetAck := olt.o_tx_reset_ack
  core.io.lane.rxResetAck := olt.o_rx_reset_ack
  core.io.lane.txReady := olt.o_tx_ready
  core.io.lane.rxReady := olt.o_rx_ready
  core.io.lane.txPllLocked := olt.o_tx_pll_locked
  core.io.lane.rxLockedToData := olt.o_rx_is_lockedtodata
  core.io.lane.rxLockedToRef := olt.o_rx_is_lockedtoref
}

object OltBoardVerilog extends App {
  SpinalConfig(targetDirectory = "out/rtl").generateVerilog(OltBoard()) // + board_regs.h, olt_regs.h
}
