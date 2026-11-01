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

/** ONU optics pins of the HSMC card (BOSA driver, cutoff, AD5593R I2C). BEN_IO is the fitted-by-hand fallback
  * (normally hi-Z); BURST_CUT is high while the hardware cutoff holds the laser off. */
case class OnuPins() extends Bundle with IMasterSlave {
  val laserDisable = Bool()
  val rxSd, burstCut = Bool()
  val scl, sda, benIo = TriState(Bool())
  override def asMaster(): Unit = {
    out(laserDisable)
    in(rxSd, burstCut)
    master(scl, sda, benIo)
  }
}

case class VcxoConfig() extends Bundle {
  val enable = Bool()
  val center, kp, ki = UInt(16 bits)
}

case class VcxoState() extends Bundle {
  val phase = SInt(16 bits)
  val duty = UInt(16 bits)
}

/** ONU board registers (APB3, management domain): ONU and burst enable GTS lanes, optics pins, I2C, VCXO loop
  * (see VcxoTracker). The laser stays disabled while the ONU clocks are down or the burst enable lane is not ready. */
case class OnuBoardRegs() extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(Apb3Config(addressWidth = 8, dataWidth = 32)))
    val onuLane, benLane = master(GtsLaneCtrl())
    val pins = master(OnuPins())
    val burstEnable = in Bool () // any line bit of the MAC burst enable, for BEN_IO
    val vcxo = out(VcxoConfig())
    val vcxoState = slave Flow (VcxoState()) // from the ONU clock domain, once per loop update
    val vcxoPwm = out Bool ()
    val alive = out Bool () // the ONU clocks run: the MAC registers answer
    val irq = out Bool ()
  }
  val busif = BusInterface(io.apb, SizeMapping(0, 256))
  BoardFields.lane(busif, "ONU", io.onuLane)
  BoardFields.lane(busif, "BEN", io.benLane)
  io.alive := synced(io.onuLane.txReady) && !io.onuLane.txReset

  val OPTICS = busif.newReg("ONU optics")
  val laserOff = OPTICS.field(Bool(), AccessType.RW, 1, "LASER_DIS, also forced while not alive or BEN lane not ready")(SymbolName("laserDisable"))
  io.pins.laserDisable := laserOff || !io.alive || !synced(io.benLane.txReady) || io.benLane.txReset
  val benMode = OPTICS.field(UInt(2 bits), AccessType.RW, 0, "benIo: 0 hi-Z, 1 low, 2 burst enable (fallback)")(SymbolName("benIo"))
  io.pins.benIo.writeEnable := benMode =/= 0
  io.pins.benIo.write := benMode === 2 && io.burstEnable
  openDrain(OPTICS, io.pins.scl, "sclLow")
  openDrain(OPTICS, io.pins.sda, "sdaLow")

  val STATUS = busif.newReg("ONU optics pins, alive: the ONU clocks run and the MAC registers answer")
  val pins = Seq("rxSd" -> synced(io.pins.rxSd), "burstCut" -> synced(io.pins.burstCut), "alive" -> io.alive)
  for ((name, pin) <- pins :+ ("scl" -> synced(io.pins.scl.read)) :+ ("sda" -> synced(io.pins.sda.read)))
    STATUS.field(Bool(), AccessType.RO, 0, name)(SymbolName(name)) := pin
  io.irq := events(busif, pins)

  val VCXO_CTRL = busif.newReg("VCXO loop")
  io.vcxo.enable := VCXO_CTRL.field(Bool(), AccessType.RW, 0, "enable: track the downstream clock")(SymbolName("enable"))
  VCXO_CTRL.reserved(15 bits)
  io.vcxo.center := VCXO_CTRL.field(UInt(16 bits), AccessType.RW, 0x8000, "center: duty while off, loop origin")(SymbolName("center"))
  val VCXO_GAIN = busif.newReg("VCXO loop gains (per clock of phase error)")
  io.vcxo.kp := VCXO_GAIN.field(UInt(16 bits), AccessType.RW, 0, "kp: duty / 256")(SymbolName("kp"))
  io.vcxo.ki := VCXO_GAIN.field(UInt(16 bits), AccessType.RW, 0, "ki: duty / 65536 per update")(SymbolName("ki"))
  val state = RegNextWhen(io.vcxoState.payload, io.vcxoState.valid)
  val tracking = io.alive && io.vcxo.enable
  val VCXO_STATUS = busif.newReg("VCXO loop state (updated every 2^18 ONU clocks)")
  VCXO_STATUS.field(SInt(16 bits), AccessType.RO, 0, "phase error, clocks")(SymbolName("phase")) := state.phase
  VCXO_STATUS.field(UInt(16 bits), AccessType.RO, 0, "duty on the pin")(SymbolName("duty")) := state.duty
  // the pin runs here, on the always-on clock: the shared VCXO holds center while the ONU clocks are down
  val modulator = SigmaDelta()
  modulator.io.duty := RegNext(tracking ? state.duty | io.vcxo.center) init (0x8000)
  io.vcxoPwm := modulator.io.pin

  busif.accept(DocCHeader("onu_board_regs", "ONU_BOARD")) // onu_board_regs.h next to the Verilog
}

/** ONU side of the board without the vendor blocks: 0x000 OnuMac registers (across to the upstream domain, an error
  * answer while the ONU clocks are down), 0x100 board registers. The ONU lane's tx clock (311.04 MHz, from the VCXO)
  * and half clock clock everything, the downstream words included (GTS RX phase compensation FIFO); the VCXO loop
  * keeps that clock on the recovered one (rxClk). */
case class OnuBoardCore() extends Component {
  val io = new Bundle {
    val axi = slave(Axi4(HpsBridge.axiConfig))
    val dsClk, usClk, rxClk = in Bool ()
    val onuLane, benLane = master(GtsLaneCtrl())
    val txWord, benWord = out Bits (8 bits)
    val rxWord = in Bits (8 bits)
    val pins = master(OnuPins())
    val vcxoPwm = out Bool ()
    val ponIrq, boardIrq = out Bool ()
  }
  val board = OnuBoardRegs()
  io.onuLane <> board.io.onuLane
  io.benLane <> board.io.benLane
  io.pins <> board.io.pins
  io.boardIrq := board.io.irq

  val domains = PonDomains(io.dsClk, io.usClk, !io.onuLane.txReady)
  val rxDomain = ClockDomain(io.rxClk, ClockDomain(io.rxClk)(BufferCC(!io.onuLane.rxReady)))
  val pon = new ClockingArea(domains.ds) {
    val lanes = GtsPonLanes()
    lanes.io.usClk := io.usClk
    lanes.io.usReset := domains.usReset
    lanes.io.oltDsLine := 0
    lanes.io.oltRxWord := 0
    io.txWord := lanes.io.onuTxWord
    io.benWord := lanes.io.benTxWord
    lanes.io.onuRxWord := io.rxWord

    val mac = gpon.OnuMac()
    mac.io.usClk := io.usClk
    mac.io.usReset := domains.usReset
    mac.io.dsLine := lanes.io.onuDsLine
    lanes.io.onuUsLine := mac.io.usLine
    lanes.io.onuUsBen := mac.io.usBen

    val tracker = VcxoTracker(rxDomain)
    val config = BufferCC(board.io.vcxo) // quasi-static settings
    tracker.io.enable := config.enable
    tracker.io.center := config.center
    tracker.io.kp := config.kp
    tracker.io.ki := config.ki
    val state = Flow(VcxoState())
    state.valid := tracker.io.update
    state.phase := tracker.io.phase
    state.duty := tracker.io.duty
  }
  board.io.vcxoState << FlowCCByToggle(pon.state, domains.ds, ClockDomain.current)
  io.vcxoPwm := board.io.vcxoPwm
  board.io.burstEnable := pon.mac.io.usBen.orR
  io.ponIrq := BufferCC(pon.mac.io.irq, False)

  val cc = Apb3CC(pon.mac.io.apb.config, ClockDomain.current, domains.us)
  pon.mac.io.apb <> cc.io.output
  HpsBridge(io.axi, Seq(cc.io.input -> board.io.alive, board.io.apb -> True))
}
