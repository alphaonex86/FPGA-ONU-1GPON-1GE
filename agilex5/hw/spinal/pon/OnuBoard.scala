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
import pon.epon.{EponGem, EponOnuMac}

/** ONU optics pins of the HSMC card (BOSA driver, cutoff, AD5593R I2C, upstream sniffer's signal detect). BEN_IO is the
  * fitted-by-hand fallback (normally hi-Z); BURST_CUT is high while the hardware cutoff holds the laser off. */
case class OnuPins() extends Bundle with IMasterSlave {
  val laserDisable = Bool()
  val rxSd, burstCut, usSd = Bool()
  val scl, sda, benIo = TriState(Bool())
  override def asMaster(): Unit = {
    out(laserDisable)
    in(rxSd, burstCut, usSd)
    master(scl, sda, benIo)
  }
}

case class VcxoConfig() extends Bundle {
  val enable = Bool()
  val center, kp, ki = UInt(16 bits)
  val center156 = UInt(16 bits) // the 156.25 MHz VCXO's (EPON loop)
}

case class VcxoState() extends Bundle {
  val phase = SInt(16 bits)
  val duty = UInt(16 bits)
}

/** ONU board registers (APB3, management domain): ONU and burst enable GTS lanes, optics pins, I2C, VCXO loop
  * (see VcxoTracker) on the VCXO of the lane's profile, the other VCXO at its center. The laser stays disabled while
  * the ONU clocks are down or the burst enable lane is not ready. */
case class OnuBoardRegs() extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(Apb3Config(addressWidth = 8, dataWidth = 32)))
    val onuLane, benLane = master(GtsLaneCtrl())
    val pins = master(OnuPins())
    val burstEnable = in Bool () // any line bit of the MAC burst enable, for BEN_IO
    val vcxo = out(VcxoConfig())
    val vcxoState = slave Flow (VcxoState()) // from the helper clock domain, once per loop update
    val helperLocked = in Bool () // the VCXO loop's helper clock runs
    val vcxoPwm, vcxo156Pwm = out Bool ()
    val epon = in Bool () // the lane runs its 1.25 Gb/s profile
    val sniffing = in Bool () // the sniffer records (asynchronous)
    val alive = out Bool () // the ONU clocks run: the MAC registers answer
    val irq = out Bool ()
  }
  val busif = BusInterface(io.apb, SizeMapping(0, 256))
  BoardFields.lane(busif, "ONU", io.onuLane)
  BoardFields.lane(busif, "BEN", io.benLane)
  io.alive := synced(io.onuLane.txReady) && !io.onuLane.txReset

  val OPTICS = busif.newReg("ONU optics")
  val laserOff = OPTICS.field(Bool(), AccessType.RW, 1, "LASER_DIS, also forced while not alive, BEN lane not ready or sniffing")(SymbolName("laserDisable"))
  io.pins.laserDisable := laserOff || !io.alive || !synced(io.benLane.txReady) || io.benLane.txReset || synced(io.sniffing)
  val benMode = OPTICS.field(UInt(2 bits), AccessType.RW, 0, "benIo: 0 hi-Z, 1 low, 2 burst enable (fallback)")(SymbolName("benIo"))
  io.pins.benIo.writeEnable := benMode =/= 0
  io.pins.benIo.write := benMode === 2 && io.burstEnable
  openDrain(OPTICS, io.pins.scl, "sclLow")
  openDrain(OPTICS, io.pins.sda, "sdaLow")

  val STATUS = busif.newReg("ONU optics pins, alive: the ONU clocks run and the MAC registers answer")
  val pins = Seq("rxSd" -> synced(io.pins.rxSd), "burstCut" -> synced(io.pins.burstCut), "alive" -> io.alive,
    "usSd" -> synced(io.pins.usSd))
  val helperLocked = synced(io.helperLocked)
  for ((name, pin) <- pins ++ Seq("scl" -> synced(io.pins.scl.read), "sda" -> synced(io.pins.sda.read), "helperLocked" -> helperLocked))
    STATUS.field(Bool(), AccessType.RO, 0, name)(SymbolName(name)) := pin
  io.irq := events(busif, pins)

  val VCXO_CTRL = busif.newReg("VCXO loop: the 155.52 MHz VCXO in GPON, the 156.25 MHz one in EPON (VCXO156)")
  io.vcxo.enable := VCXO_CTRL.field(Bool(), AccessType.RW, 0, "enable: track the downstream clock")(SymbolName("enable"))
  VCXO_CTRL.reserved(15 bits)
  io.vcxo.center := VCXO_CTRL.field(UInt(16 bits), AccessType.RW, 0x8000, "center: 155.52 MHz VCXO duty while not tracking, loop origin")(SymbolName("center"))
  val VCXO_GAIN = busif.newReg("VCXO loop gains (per helper cycle of phase)")
  io.vcxo.kp := VCXO_GAIN.field(UInt(16 bits), AccessType.RW, 0, "kp: duty / 4096")(SymbolName("kp"))
  io.vcxo.ki := VCXO_GAIN.field(UInt(16 bits), AccessType.RW, 0, "ki: duty / 2^32 per beat")(SymbolName("ki"))
  val state = RegNextWhen(io.vcxoState.payload, io.vcxoState.valid)
  val tracking = io.alive && io.vcxo.enable && helperLocked
  val VCXO_STATUS = busif.newReg("VCXO loop state (updated every beat, ~3 us)")
  VCXO_STATUS.field(SInt(16 bits), AccessType.RO, 0, "phase, helper cycles (6.7 ps)")(SymbolName("phase")) := state.phase
  VCXO_STATUS.field(UInt(16 bits), AccessType.RO, 0, "duty on the pin")(SymbolName("duty")) := state.duty
  val VCXO156 = busif.newReg("156.25 MHz VCXO (Ethernet and the 1.25 Gb/s profiles; the loop's in EPON)")
  io.vcxo.center156 := VCXO156.field(UInt(16 bits), AccessType.RW, 0x8000, "center: duty while not tracking, loop origin")(SymbolName("center156"))
  val eponMode = synced(io.epon)
  val PROFILE = busif.newReg("ONU lane profile, switched by the DR controller (OLT side 0x900) while the lane is reset")
  PROFILE.field(Bool(), AccessType.RO, 0, "epon: 1.25 Gb/s, EPON ONU MAC (0x800); else GPON (0x000)")(SymbolName("epon")) := eponMode
  // the pins run here, on the always-on clock: a shared VCXO holds its center while the ONU does not track it
  def pin(duty: UInt): Bool = {
    val modulator = SigmaDelta()
    modulator.io.duty := RegNext(duty) init (0x8000)
    modulator.io.pin
  }
  io.vcxoPwm := pin((tracking && !eponMode) ? state.duty | io.vcxo.center)
  io.vcxo156Pwm := pin((tracking && eponMode) ? state.duty | io.vcxo.center156)

  busif.accept(DocCHeader("onu_board_regs", "ONU_BOARD")) // onu_board_regs.h next to the Verilog
}

/** ONU side of the board without the vendor blocks: 0x000 OnuMac registers (across to the upstream domain, an error
  * answer while the ONU clocks are down), 0x100 board registers, 0x200 GEM CPU queues (OMCI; reset with the clocks),
  * 0x300 Ethernet bridge (also an error while the Ethernet lane is down; by default downstream GEM is dropped, the OMCI
  * Port-ID goes to the CPU; upstream one queue per data T-CONT, DATA_ALLOC_k), 0x400 and 0x500 the bridge's LAN and
  * WAN frame queues (as the bridge: flows-mode misses and the CPU's frames, EPON OAM out), 0x600 and 0x700 their
  * control frames, 0x800 EponOnuMac registers, 0x900 the GPON sniffer (SnifferCore: downstream from the ONU lane, the
  * upstream tap on the burst enable lane's receiver; laser off while it records). The ONU lane runs GPON or, after a DR switch (`epon`, changed only
  * while the lane is reset), 1.25 Gb/s for the EPON ONU MAC: its tx clock (311.04 / 125 MHz, from the profile's VCXO)
  * and half clock clock everything, the received words included (GTS RX phase compensation FIFO); a VCXO loop per
  * profile keeps that clock on the recovered one, both sampled by a helper clock (VcxoTracker). The bridge's upstream
  * queues run on the tx clock: into the EPON MAC, or a byte per half clock into the OnuMac. The burst enable lane
  * switches profile with the ONU lane (DR), on the same clock (its receiver too, through its RX FIFO); its TX reset
  * with the ONU lane's. */
case class OnuBoardCore() extends Component {
  val io = new Bundle {
    val axi = slave(Axi4(HpsBridge.axiConfig))
    val dsClk, usClk = in Bool ()
    val gponRxClk, eponRxClk = in Bool () // each profile's recovered clock, for its VCXO loop
    val helperClk, eponHelperClk, helperLocked = in Bool () // 155.357 / 62.143 MHz from a PLL on a board oscillator
    val epon = in Bool () // the ONU lane's profile (DR controller, management domain)
    val onuLane, benLane = master(GtsLaneCtrl())
    val txWord = out Bits (10 bits) // GPON: bits 7-0
    val benWord = out Bits (10 bits) // GPON: bits 7-0
    val rxWord = in Bits (10 bits) // GPON: bits 7-0
    val benRxWord = in Bits (10 bits) // the burst enable lane's receiver (upstream tap), GPON: bits 7-0
    val pins = master(OnuPins())
    val vcxoPwm, vcxo156Pwm = out Bool ()
    val ponIrq, boardIrq, gemIrq, snifferIrq = out Bool ()
    val bridgeIrq = out Bits (4 bits) // LAN, WAN, LAN control, WAN control frame queues
    val ethTxClk, ethRxClk, ethDown = in Bool () // the Ethernet lane: clocks, not ready (asynchronous)
    val eth = master(EthFrames())
  }
  val board = OnuBoardRegs()
  io.onuLane <> board.io.onuLane
  io.benLane <> board.io.benLane
  io.benLane.txReset.removeAssignments() := board.io.benLane.txReset || board.io.onuLane.txReset // switched with it (DR)
  io.pins <> board.io.pins
  io.boardIrq := board.io.irq
  board.io.epon := io.epon

  val down = !io.onuLane.txReady
  val domains = PonDomains(io.dsClk, io.usClk, down)
  val gponDomains = PonDomains(io.dsClk, io.usClk, down || io.epon)
  val eponDomain = PonDomains(io.dsClk, io.usClk, down || !io.epon).ds
  val pon = new ClockingArea(gponDomains.ds) {
    val lanes = GtsPonLanes()
    lanes.io.usClk := io.usClk
    lanes.io.usReset := gponDomains.usReset
    lanes.io.oltDsLine := 0
    lanes.io.oltRxWord := io.benRxWord(7 downto 0) // the upstream tap, 2x oversampled as an OLT's
    lanes.io.onuRxWord := io.rxWord(7 downto 0)

    val mac = gpon.OnuMac()
    mac.io.usClk := io.usClk
    mac.io.usReset := gponDomains.usReset
    mac.io.dsLine := lanes.io.onuDsLine
    lanes.io.onuUsLine := mac.io.usLine
    lanes.io.onuUsBen := mac.io.usBen

    val tap = sniffer.SnifferCore(gponDomains.us)
    tap.io.ds := sniffer.DsFields.of(mac.deframer)
    tap.io.usLine := lanes.io.oltUsLine
  }
  board.io.sniffing := pon.tap.io.enabled
  io.snifferIrq := BufferCC(pon.tap.io.irq, False)
  val epon = new ClockingArea(eponDomain) {
    val mac = EponOnuMac()
    mac.io.rxCode := io.rxWord
  }
  def halved(clk: Bool): Bool = ClockDomain(clk, config = ClockDomainConfig(resetKind = BOOT)) {
    val q = RegInit(False)
    q := !q
    q
  }
  val localHalf = halved(io.dsClk)
  /** The VCXO loop of one profile, enabled while the lane runs it. */
  class VcxoLoop(helperClk: Bool, rxClk: Bool, isEpon: Boolean, glitchCycles: Int) extends Area {
    val helper = ClockDomain(helperClk, ClockDomain(helperClk)(BufferCC(!io.helperLocked)))
    val rxHalf = halved(rxClk)
    val loop = new ClockingArea(helper) {
      val tracker = VcxoTracker(glitchCycles = glitchCycles)
      tracker.io.rxClk := rxHalf
      tracker.io.localClk := localHalf
      val config = BufferCC(board.io.vcxo) // quasi-static settings
      val running = board.io.alive && io.onuLane.rxReady && (if (isEpon) io.epon else !io.epon)
      tracker.io.enable := config.enable && BufferCC(running, False)
      tracker.io.center := (if (isEpon) config.center156 else config.center)
      tracker.io.kp := config.kp
      tracker.io.ki := config.ki
      val state = Flow(VcxoState())
      state.valid := tracker.io.update
      state.phase := tracker.io.phase
      state.duty := tracker.io.duty
    }
    val state = FlowCCByToggle(loop.state, helper, ClockDomain.current)
  }
  val gponVcxo = new VcxoLoop(io.helperClk, io.gponRxClk, isEpon = false, glitchCycles = 128)
  val eponVcxo = new VcxoLoop(io.eponHelperClk, io.eponRxClk, isEpon = true, glitchCycles = 32) // beat: 175 cycles
  board.io.vcxoState << (synced(io.epon) ? eponVcxo.state | gponVcxo.state)
  board.io.helperLocked := io.helperLocked
  io.vcxoPwm := board.io.vcxoPwm
  io.vcxo156Pwm := board.io.vcxo156Pwm
  board.io.burstEnable := pon.mac.io.usBen.orR || epon.mac.io.burst
  io.ponIrq := BufferCC(pon.mac.io.irq, False) || BufferCC(epon.mac.io.irq, False)

  val queuesDomain = ClockDomain.current.copy(reset = ClockDomain.current.readResetWire || RegNext(!board.io.alive).init(True))
  val queues = queuesDomain(gpon.GemCpuQueues(domains.us, domains.ds))
  pon.mac.io.gemTx << queues.io.tx
  pon.mac.io.gemTxBacklog := queues.io.txBacklog
  io.gemIrq := queues.io.irq

  val bridge = ethBridge(domains.ds, domains.ds, io.ethTxClk, io.ethRxClk, down || io.ethDown, defaultAction = 0,
    ponQueues = pon.mac.dataTconts)
  bridge.io.cpuPort << pon.mac.io.omciPort
  queues.io.rx << bridge.io.toCpu
  queues.io.rxCut := bridge.io.toCpuCut
  io.eth <> bridge.io.eth
  val ds = new ClockingArea(domains.ds) {
    val eponMode = BufferCC(io.epon, False)
    val (eponRx, eponCut) = EponGem.toGem(epon.mac.io.dataRx, epon.mac.io.dataRxGood)
    bridge.io.ponRx << (eponMode ? eponRx | pon.mac.io.gemRx)
    bridge.io.ponRxCut := eponMode ? eponCut | pon.mac.io.gemRxCut
    val toMac = bridge.io.ponTx.map(StreamDemux(_, eponMode.asUInt, 2))
    for ((tx, queue) <- pon.mac.io.dataTx.zip(toMac)) tx << gponDomains.us(domains.toUs(queue(0)).s2mPipe().m2sPipe())
    val backlog = RegNext(bridge.io.ponTxBacklog) // only for DBRu and REPORT: registered (both sides of the crossing)
    pon.mac.io.dataTxBacklog := gponDomains.us(RegNext(backlog))
    val (frames, report) = EponGem.toUpstream(toMac.map(_(1)), backlog)
    epon.mac.io.dataTx << frames
    epon.mac.io.report := report
    io.txWord := eponMode ? epon.mac.io.txCode | pon.lanes.io.onuTxWord.resized
  }
  io.benWord := ds.eponMode ? B(10 bits, default -> epon.mac.io.burst) | pon.lanes.io.benTxWord.resized
  val bridgeUp = board.io.alive && !BufferCC(io.ethDown, True)
  val bridgeQueues = BridgeQueues(bridge, !bridgeUp)
  io.bridgeIrq := bridgeQueues.irqs

  val gponCC = Apb3CC(pon.mac.io.apb.config, ClockDomain.current, gponDomains.us)
  pon.mac.io.apb <> gponCC.io.output
  val eponCC = Apb3CC(epon.mac.io.apb.config, ClockDomain.current, eponDomain)
  epon.mac.io.apb <> eponCC.io.output
  val tapCC = Apb3CC(pon.tap.io.apb.config, ClockDomain.current, gponDomains.ds)
  pon.tap.io.apb <> tapCC.io.output
  val eponMode = synced(io.epon)
  HpsBridge(io.axi, Seq(gponCC.io.input -> (board.io.alive && !eponMode), board.io.apb -> True,
    queues.io.apb -> board.io.alive, bridge.io.apb -> bridgeUp) ++ bridgeQueues.slaves(bridgeUp) ++
    Seq(eponCC.io.input -> (board.io.alive && eponMode), tapCC.io.input -> (board.io.alive && !eponMode)))
}
