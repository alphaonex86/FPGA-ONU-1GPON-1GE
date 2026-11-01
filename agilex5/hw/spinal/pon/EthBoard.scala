// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif._
import BoardFields._

/** Ethernet board registers (APB3, management domain): Ethernet GTS lane, SFP cage pins, module I2C, PCS state. */
case class EthBoardRegs() extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(Apb3Config(addressWidth = 8, dataWidth = 32)))
    val lane = master(GtsLaneCtrl())
    val sfp = master(SfpPins())
    val sgmii = out Bool ()
    val anComplete, synced = in Bool () // lane clock domains
    val alive = out Bool () // both lane clocks run: the frame queues answer
    val irq = out Bool ()
  }
  val busif = BusInterface(io.apb, SizeMapping(0, 256))
  BoardFields.lane(busif, "ETH", io.lane)
  io.alive := synced(io.lane.txReady) && synced(io.lane.rxReady) && !io.lane.txReset && !io.lane.rxReset
  val cage = sfpCage(busif, "Ethernet", io.sfp, io.alive, False, "both lane clocks run and the frame queues answer")
  val PCS = busif.newReg("1000BASE-X / SGMII PCS")
  io.sgmii := PCS.field(Bool(), AccessType.RW, 0, "SGMII (copper SFP), else 1000BASE-X")(SymbolName("sgmii"))
  val pcs = Seq("anComplete" -> synced(io.anComplete), "synced" -> synced(io.synced))
  for ((name, pin) <- pcs) PCS.field(Bool(), AccessType.RO, 0, name)(SymbolName(name)) := pin
  io.irq := events(busif, cage ++ pcs)

  busif.accept(DocCHeader("eth_board_regs", "ETH_BOARD")) // eth_board_regs.h next to the Verilog
}

/** Ethernet side of the board without the vendor blocks: 0x000 board registers, 0x100 frame queues (GemCpuQueues,
  * Port-ID 0, one fragment per frame, a bad frame as cut). The lane's tx and rx clocks (125 MHz) clock the port, each
  * reset until its lane side is ready; the queues reset while either is down. anSpeedup: short autonegotiation timer
  * (simulation). */
case class EthBoardCore(anSpeedup: Boolean = false) extends Component {
  val io = new Bundle {
    val axi = slave(Axi4(HpsBridge.axiConfig))
    val txClk, rxClk = in Bool ()
    val lane = master(GtsLaneCtrl())
    val txWord = out Bits (10 bits)
    val rxWord = in Bits (10 bits)
    val sfp = master(SfpPins())
    val boardIrq, frameIrq = out Bool ()
  }
  val board = EthBoardRegs()
  io.lane <> board.io.lane
  io.sfp <> board.io.sfp
  io.boardIrq := board.io.irq

  def laneDomain(clk: Bool, down: Bool): ClockDomain = ClockDomain(clk, ClockDomain(clk)(BufferCC(down)))
  val txCd = laneDomain(io.txClk, !io.lane.txReady)
  val rxCd = laneDomain(io.rxClk, !io.lane.rxReady)
  val port = EthPort(txCd, rxCd)
  port.io.rawRx := io.rxWord
  io.txWord := port.io.rawTx
  port.io.sgmii := txCd(BufferCC(board.io.sgmii, False)) // taxi's autonegotiation runs on the tx clock
  port.io.anSpeedup := Bool(anSpeedup)
  board.io.anComplete := port.io.anComplete
  board.io.synced := port.io.synced

  val queuesDomain = ClockDomain.current.copy(reset = ClockDomain.current.readResetWire || RegNext(!board.io.alive).init(True))
  val queues = queuesDomain(gpon.GemCpuQueues(txCd, rxCd))
  port.io.tx << queues.io.tx.translateWith {
    val f = Fragment(Bits(8 bits))
    f.fragment := queues.io.tx.fragment.data
    f.last := queues.io.tx.last
    f
  }
  val rx = queues.io.rx
  val bad = port.io.rx.last && port.io.rxBad
  rx.valid := port.io.rx.valid
  rx.last := port.io.rx.last && !bad
  rx.allocId := 0
  rx.gem.portId := 0
  rx.gem.pti := B"001"
  rx.gem.data := port.io.rx.fragment
  queues.io.rxCut := port.io.rx.valid && bad
  io.frameIrq := queues.io.irq

  HpsBridge(io.axi, Seq(board.io.apb -> True, queues.io.apb -> board.io.alive))
}
