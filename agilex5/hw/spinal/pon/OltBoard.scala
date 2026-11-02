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

/** SFP cage pins. RS0/RS1 (pins 7, 9) stay hi-Z until software knows the module. */
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

  io.irq := events(busif, sfpCage(busif, "OLT", io.sfp, io.alive, io.burstReset, "the PON clocks run and the MAC registers answer"))
  val REFCLK = busif.newReg("GTS reference clocks")
  REFCLK.field(Bits(8 bits), AccessType.RO, 0, "fail: per reference clock")(SymbolName("fail")) := BufferCC(io.refclkFail, B(0, 8 bits))

  busif.accept(DocCHeader("olt_board_regs", "OLT_BOARD")) // olt_board_regs.h next to the Verilog
}

/** OLT side of the board without the vendor blocks: 0x000 OltMac registers (across to the upstream domain), 0x100
  * board registers, 0x200 GEM CPU queues (OMCI), 0x300 Ethernet bridge. MAC and queues answer an error while the PON
  * clocks are down, the bridge also while the Ethernet lane is. The lane's tx clock (311.04 MHz) and half clock clock
  * the MAC, reset until the lane is ready; the queues reset with it, the bridge queues while either lane is down.
  * Upstream GEM goes through the bridge's Port-ID lookup (by default to the CPU, also while the Ethernet lane is down);
  * downstream, the CPU packets before the bridged ones. */
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
    val ethTxClk, ethRxClk, ethDown = in Bool () // the Ethernet lane: clocks, not ready (asynchronous)
    val eth = master(EthFrames())
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
  io.gemIrq := queues.io.irq

  val bridge = ethBridge(domains.us, domains.ds, io.ethTxClk, io.ethRxClk, !io.lane.txReady || io.ethDown, defaultAction = 1)
  bridge.io.ponRx << pon.mac.io.usGem
  bridge.io.ponRxCut := pon.mac.io.usGemCut
  bridge.io.cpuPort.setIdle()
  queues.io.rx << bridge.io.toCpu
  queues.io.rxCut := bridge.io.toCpuCut
  pon.mac.io.gem << new ClockingArea(domains.ds) {
    val ds = StreamArbiterFactory.lowerFirst.fragmentLock.onArgs(queues.io.tx, bridge.io.ponTx)
  }.ds
  io.eth <> bridge.io.eth
  val bridgeUp = board.io.alive && !BufferCC(io.ethDown, True)

  val cc = Apb3CC(pon.mac.io.apb.config, ClockDomain.current, domains.us)
  pon.mac.io.apb <> cc.io.output
  HpsBridge(io.axi, Seq(cc.io.input -> board.io.alive, board.io.apb -> True, queues.io.apb -> board.io.alive,
    bridge.io.apb -> bridgeUp))
}
