// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif._
import spinal.lib.io.TriState
import BoardFields._

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

/** OLT board registers (APB3, management domain): OLT GTS lane, SFP cage pins, bit-banged module I2C. */
case class OltBoardRegs() extends Component {
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
  BoardFields.lane(busif, "OLT", io.lane)
  io.alive := synced(io.lane.txReady) && !io.lane.txReset

  val SFP_CTRL = busif.newReg("OLT SFP cage")
  io.sfp.txDisable := SFP_CTRL.field(Bool(), AccessType.RW, 1, "TX_DIS, also forced while not alive")(SymbolName("txDisable")) || !io.alive
  def rsPin(pin: TriState[Bool], name: String): Unit = {
    val mode = SFP_CTRL.field(UInt(2 bits), AccessType.RW, 0, s"$name: 0 hi-Z, 1 low, 2 high, 3 burst reset")(SymbolName(name))
    pin.writeEnable := mode =/= 0
    pin.write := (mode === 3) ? io.burstReset | mode(1)
  }
  rsPin(io.sfp.rs0, "rs0")
  rsPin(io.sfp.rs1, "rs1")
  openDrain(SFP_CTRL, io.sfp.scl, "sclLow")
  openDrain(SFP_CTRL, io.sfp.sda, "sdaLow")

  val STATUS = busif.newReg("OLT SFP cage pins, alive: the PON clocks run and the MAC registers answer")
  val pins = Seq("modAbs" -> synced(io.sfp.modAbs), "los" -> synced(io.sfp.los), "txFault" -> synced(io.sfp.txFault),
    "alive" -> io.alive)
  for ((name, pin) <- pins :+ ("scl" -> synced(io.sfp.scl.read)) :+ ("sda" -> synced(io.sfp.sda.read)))
    STATUS.field(Bool(), AccessType.RO, 0, name)(SymbolName(name)) := pin
  io.irq := events(busif, pins)
  val REFCLK = busif.newReg("GTS reference clocks")
  REFCLK.field(Bits(8 bits), AccessType.RO, 0, "fail: per reference clock")(SymbolName("fail")) := BufferCC(io.refclkFail, B(0, 8 bits))

  busif.accept(DocCHeader("olt_board_regs", "OLT_BOARD")) // olt_board_regs.h next to the Verilog
}

/** OLT side of the board without the vendor blocks: 0x000 OltMac registers (across to the upstream domain), 0x100
  * board registers, 0x200 GEM CPU queues (OMCI). MAC and queues answer an error while the PON clocks are down. The
  * lane's tx clock (311.04 MHz) and half clock clock the MAC, reset until the lane is ready; the queues reset with it. */
case class OltBoardCore() extends Component {
  val io = new Bundle {
    val axi = slave(Axi4(HpsBridge.axiConfig))
    val dsClk, usClk = in Bool ()
    val lane = master(GtsLaneCtrl())
    val txWord = out Bits (8 bits)
    val rxWord = in Bits (8 bits)
    val refclkFail = in Bits (8 bits)
    val sfp = master(SfpPins())
    val ponIrq, boardIrq, gemIrq = out Bool ()
  }
  val board = OltBoardRegs()
  io.lane <> board.io.lane
  board.io.refclkFail := io.refclkFail
  io.sfp <> board.io.sfp
  io.boardIrq := board.io.irq

  val domains = PonDomains(io.dsClk, io.usClk, !io.lane.txReady)
  val pon = new ClockingArea(domains.ds) {
    val lanes = GtsPonLanes()
    lanes.io.usClk := io.usClk
    lanes.io.usReset := domains.usReset
    io.txWord := lanes.io.oltTxWord
    lanes.io.oltRxWord := io.rxWord
    lanes.io.onuRxWord := 0
    lanes.io.onuUsLine := 0
    lanes.io.onuUsBen := 0

    val mac = gpon.OltMac()
    mac.io.usClk := io.usClk
    mac.io.usReset := domains.usReset
    lanes.io.oltDsLine := mac.io.dsLine
    mac.io.usLine := lanes.io.oltUsLine
  }
  board.io.burstReset := pon.mac.io.rxReset
  io.ponIrq := BufferCC(pon.mac.io.irq, False)

  val queuesDomain = ClockDomain.current.copy(reset = ClockDomain.current.readResetWire || RegNext(!board.io.alive).init(True))
  val queues = queuesDomain(gpon.GemCpuQueues(domains.ds, domains.us))
  pon.mac.io.gem << queues.io.tx
  queues.io.rx << pon.mac.io.usGem
  queues.io.rxCut := pon.mac.io.usGemCut
  io.gemIrq := queues.io.irq

  val cc = Apb3CC(pon.mac.io.apb.config, ClockDomain.current, domains.us)
  pon.mac.io.apb <> cc.io.output
  HpsBridge(io.axi, Seq(cc.io.input -> board.io.alive, board.io.apb -> True, queues.io.apb -> board.io.alive))
}
