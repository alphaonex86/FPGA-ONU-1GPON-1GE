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
import pon.epon.{EponGem, EponOltMac}

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
    val burstReset = in Bool () // the MAC's rxReset, for an RS pin in burst reset mode
    val epon = in Bool () // the lane runs its 1.25 Gb/s profile
    val alive = out Bool () // the PON clocks run: the MAC registers answer
    val irq = out Bool ()
  }
  val busif = BusInterface(io.apb, SizeMapping(0, 256))
  BoardFields.lane(busif, "OLT", io.lane)
  io.alive := synced(io.lane.txReady) && !io.lane.txReset

  io.irq := events(busif, sfpCage(busif, "OLT", io.sfp, io.alive, io.burstReset, "the PON clocks run and the MAC registers answer"))
  val REFCLK = busif.newReg("GTS reference clocks")
  REFCLK.field(Bits(8 bits), AccessType.RO, 0, "fail: per reference clock")(SymbolName("fail")) := BufferCC(io.refclkFail, B(0, 8 bits))
  val PROFILE = busif.newReg("OLT lane profile, switched by the DR controller (0x900) while the lane is reset")
  PROFILE.field(Bool(), AccessType.RO, 0, "epon: 1.25 Gb/s, EPON OLT MAC (0x800), else GPON (0x000)")(SymbolName("epon")) := io.epon

  busif.accept(DocCHeader("olt_board_regs", "OLT_BOARD")) // olt_board_regs.h next to the Verilog
}

/** A lane's received words from its rx_clkout into the current domain (same frequency, any phase): a small
  * asynchronous FIFO read from half full on; a slip (empty) restarts it, zero words meanwhile. */
case class RxWordCC(width: Int) extends Component {
  val io = new Bundle {
    val rxClk = in Bool ()
    val down = in Bool () // asynchronous: the lane's receiver not ready
    val rxWord = in Bits (width bits) // on rxClk
    val word = out Bits (width bits)
  }
  val rxDomain = ClockDomain(io.rxClk, ClockDomain(io.rxClk)(BufferCC(io.down)))
  val fifo = StreamFifoCC(Bits(width bits), 16, rxDomain, ClockDomain.current)
  fifo.ram.addTag(ReadDuringWriteFree)
  fifo.io.push.valid := True
  fifo.io.push.payload := io.rxWord
  val reading = RegInit(False)
  when(fifo.io.popOccupancy >= 8)(reading := True)
  when(!fifo.io.pop.valid)(reading := False)
  fifo.io.pop.ready := reading
  io.word := RegNext((reading && fifo.io.pop.valid) ? fifo.io.pop.payload | B(0, width bits)) init (0)
}

/** Host CSR port of the GTS DR controller (Avalon-MM, byte addresses). */
case class DrHost() extends Bundle with IMasterSlave {
  val address = UInt(7 bits)
  val read, write = Bool()
  val writeData, readData = Bits(32 bits)
  val readDataValid, waitRequest = Bool()
  override def asMaster(): Unit = {
    out(address, read, write, writeData)
    in(readData, readDataValid, waitRequest)
  }

  /** An APB3 slave on it: a write ends when accepted, a read when its data comes. */
  def fromApb(apb: Apb3): Unit = {
    val access = apb.PSEL(0) && apb.PENABLE
    val reading = RegInit(False) // read accepted, its data not yet back
    address := apb.PADDR.resized
    writeData := apb.PWDATA
    write := access && apb.PWRITE
    read := access && !apb.PWRITE && !reading
    when(read && !waitRequest)(reading := True)
    when(readDataValid)(reading := False)
    apb.PREADY := apb.PWRITE ? !waitRequest | readDataValid
    apb.PRDATA := readData
    apb.PSLVERROR := False
  }
}

/** OLT side of the board without the vendor blocks: 0x000 OltMac registers (across to the upstream domain), 0x100
  * board registers, 0x200 GEM CPU queues (OMCI; EPON: frames with their FCS, Port-ID = LLID), 0x300 Ethernet bridge,
  * 0x400 / 0x500 its LAN / WAN frame queues, 0x600 / 0x700 their control frames (bridgeIrq: same order), 0x800
  * EponOltMac registers, 0x900 the lane's DR controller. The lane runs GPON or, after a DR switch (`epon`, changed
  * only while the lane is reset), 1.25 Gb/s for the EPON OLT MAC: its tx clock (311.04 / 125 MHz) and half clock
  * clock the MACs, each reset until the lane is ready in its mode; the received words cross from the profile's
  * rx clock. MACs and queues answer an error while the PON clocks are down (a MAC also outside its mode), the bridge
  * also while the Ethernet lane is down. Upstream GEM (EPON: LLID frames, EponGem) goes through the bridge's Port-ID
  * lookup (by default to the CPU, also while the Ethernet lane is down); downstream, the CPU packets before the
  * bridged ones. */
case class OltBoardCore() extends Component {
  val io = new Bundle {
    val axi = slave(Axi4(HpsBridge.axiConfig))
    val dsClk, usClk = in Bool ()
    val lane = master(GtsLaneCtrl())
    val epon = in Bool () // the lane's profile (DR controller, management domain)
    val dr = master(DrHost())
    val txWord = out Bits (10 bits) // GPON: bits 7-0
    val gponRxClk, eponRxClk = in Bool ()
    val gponRxWord = in Bits (8 bits)
    val eponRxWord = in Bits (10 bits)
    val refclkFail = in Bits (8 bits)
    val sfp = master(SfpPins())
    val ponIrq, boardIrq, gemIrq = out Bool ()
    val bridgeIrq = out Bits (4 bits) // LAN, WAN, LAN control, WAN control frame queues
    val ethTxClk, ethRxClk, ethDown = in Bool () // the Ethernet lane: clocks, not ready (asynchronous)
    val eth = master(EthFrames())
  }
  val board = OltBoardRegs()
  io.lane <> board.io.lane
  board.io.refclkFail := io.refclkFail
  io.sfp <> board.io.sfp
  io.boardIrq := board.io.irq
  board.io.epon := io.epon

  val down = !io.lane.txReady
  val domains = PonDomains(io.dsClk, io.usClk, down)
  val gponDomains = PonDomains(io.dsClk, io.usClk, down || io.epon)
  val eponDomain = PonDomains(io.dsClk, io.usClk, down || !io.epon).ds
  val pon = new ClockingArea(gponDomains.ds) {
    val lanes = GtsPonLanes()
    lanes.io.usClk := io.usClk
    lanes.io.usReset := gponDomains.usReset
    val rx = RxWordCC(8)
    rx.io.rxClk := io.gponRxClk
    rx.io.down := !io.lane.rxReady || io.epon
    rx.io.rxWord := io.gponRxWord
    lanes.io.oltRxWord := rx.io.word
    lanes.io.onuRxWord := 0
    lanes.io.onuUsLine := 0
    lanes.io.onuUsBen := 0

    val mac = gpon.OltMac()
    mac.io.usClk := io.usClk
    mac.io.usReset := gponDomains.usReset
    lanes.io.oltDsLine := mac.io.dsLine
    mac.io.usLine := lanes.io.oltUsLine
  }
  val epon = new ClockingArea(eponDomain) {
    val mac = EponOltMac()
    val rx = RxWordCC(10)
    rx.io.rxClk := io.eponRxClk
    rx.io.down := !io.lane.rxReady || !io.epon
    rx.io.rxWord := io.eponRxWord
    mac.io.rxCode := rx.io.word
  }
  board.io.burstReset := io.epon ? epon.mac.io.rxReset | pon.mac.io.rxReset
  io.ponIrq := BufferCC(pon.mac.io.irq, False) || BufferCC(epon.mac.io.irq, False)

  val queuesDomain = ClockDomain.current.copy(reset = ClockDomain.current.readResetWire || RegNext(!board.io.alive).init(True))
  val queues = queuesDomain(gpon.GemCpuQueues(domains.ds, domains.ds))
  io.gemIrq := queues.io.irq

  val bridge = ethBridge(domains.ds, domains.ds, io.ethTxClk, io.ethRxClk, down || io.ethDown, defaultAction = 1)
  bridge.io.cpuPort.setIdle()
  queues.io.rx << bridge.io.toCpu
  queues.io.rxCut := bridge.io.toCpuCut
  io.eth <> bridge.io.eth
  val ds = new ClockingArea(domains.ds) {
    val eponMode = BufferCC(io.epon, False)
    val usBeat = domains.usBeat
    val gponRx = Flow(Fragment(gpon.UsGemByte()))
    gponRx.valid := RegNext(pon.mac.io.usGem.valid && usBeat) init (False)
    gponRx.payload := RegNext(pon.mac.io.usGem.payload)
    val gponCut = RegNext(pon.mac.io.usGemCut && usBeat) init (False)
    val (eponRx, eponCut) = EponGem.toGem(epon.mac.io.dataRx, epon.mac.io.dataRxGood)
    bridge.io.ponRx << (eponMode ? eponRx | gponRx)
    bridge.io.ponRxCut := eponMode ? eponCut | gponCut
    val packets = StreamArbiterFactory.lowerFirst.fragmentLock.onArgs(queues.io.tx, bridge.io.ponTx(0))
    val toMac = StreamDemux(packets, eponMode.asUInt, 2)
    pon.mac.io.gem << toMac(0).s2mPipe().m2sPipe() // registered both ways at 311.04 MHz
    epon.mac.io.dataTx << EponGem.toFrames(toMac(1))
    io.txWord := eponMode ? epon.mac.io.txCode | pon.lanes.io.oltTxWord.resized
  }
  val bridgeUp = board.io.alive && !BufferCC(io.ethDown, True)
  val bridgeQueues = BridgeQueues(bridge, !bridgeUp)
  io.bridgeIrq := bridgeQueues.irqs

  val gponCC = Apb3CC(pon.mac.io.apb.config, ClockDomain.current, gponDomains.us)
  pon.mac.io.apb <> gponCC.io.output
  val eponCC = Apb3CC(epon.mac.io.apb.config, ClockDomain.current, eponDomain)
  epon.mac.io.apb <> eponCC.io.output
  val drApb = Apb3(Apb3Config(addressWidth = 8, dataWidth = 32))
  io.dr.fromApb(drApb)
  val eponMode = synced(io.epon)
  HpsBridge(io.axi, Seq(gponCC.io.input -> (board.io.alive && !eponMode), board.io.apb -> True, queues.io.apb -> board.io.alive,
    bridge.io.apb -> bridgeUp) ++ bridgeQueues.slaves(bridgeUp) ++
    Seq(eponCC.io.input -> (board.io.alive && eponMode), drApb -> True))
}
