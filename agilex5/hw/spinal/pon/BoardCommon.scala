// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.axi._
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

/** Register fields shared by the board register files (management domain, asynchronous pins synchronized). */
object BoardFields {
  def synced(pin: Bool): Bool = BufferCC(pin, False)

  /** <prefix>_CTRL and <prefix>_STATUS of a GTS lane. Reset by software: set the reset bit, wait its ack, clear it,
    * wait ready. */
  def lane(busif: BusIf, prefix: String, lane: GtsLaneCtrl): Unit = {
    val ctrl = busif.newReg(s"$prefix lane control")(SymbolName(s"${prefix}_CTRL"))
    lane.txReset := ctrl.field(Bool(), AccessType.RW, 0, "TX reset")(SymbolName("txReset"))
    lane.rxReset := ctrl.field(Bool(), AccessType.RW, 0, "RX reset")(SymbolName("rxReset"))
    lane.rxLockToRef := ctrl.field(Bool(), AccessType.RW, 0, "RX CDR locked to the reference")(SymbolName("lockToRef"))
    val status = busif.newReg(s"$prefix lane status")(SymbolName(s"${prefix}_STATUS"))
    for ((name, pin) <- Seq("txResetAck" -> lane.txResetAck, "rxResetAck" -> lane.rxResetAck, "txReady" -> lane.txReady,
           "rxReady" -> lane.rxReady, "txPllLocked" -> lane.txPllLocked, "rxLockedToData" -> lane.rxLockedToData,
           "rxLockedToRef" -> lane.rxLockedToRef))
      status.field(Bool(), AccessType.RO, 0, name)(SymbolName(name)) := synced(pin)
  }

  /** SFP_CTRL and STATUS of an SFP cage (TX_DIS forced while not alive); the status pins, for events. */
  def sfpCage(busif: BusIf, cage: String, sfp: SfpPins, alive: Bool, burstReset: Bool, aliveDoc: String): Seq[(String, Bool)] = {
    val SFP_CTRL = busif.newReg(s"$cage SFP cage")
    sfp.txDisable := SFP_CTRL.field(Bool(), AccessType.RW, 1, "TX_DIS, also forced while not alive")(SymbolName("txDisable")) || !alive
    def rsPin(pin: TriState[Bool], name: String): Unit = {
      val mode = SFP_CTRL.field(UInt(2 bits), AccessType.RW, 0, s"$name: 0 hi-Z, 1 low, 2 high, 3 burst reset")(SymbolName(name))
      pin.writeEnable := mode =/= 0
      pin.write := (mode === 3) ? burstReset | mode(1)
    }
    rsPin(sfp.rs0, "rs0")
    rsPin(sfp.rs1, "rs1")
    openDrain(SFP_CTRL, sfp.scl, "sclLow")
    openDrain(SFP_CTRL, sfp.sda, "sdaLow")
    val STATUS = busif.newReg(s"$cage SFP cage pins, alive: $aliveDoc")
    val pins = Seq("modAbs" -> synced(sfp.modAbs), "los" -> synced(sfp.los), "txFault" -> synced(sfp.txFault), "alive" -> alive)
    for ((name, pin) <- pins :+ ("scl" -> synced(sfp.scl.read)) :+ ("sda" -> synced(sfp.sda.read)))
      STATUS.field(Bool(), AccessType.RO, 0, name)(SymbolName(name)) := pin
    pins
  }

  def openDrain(reg: RegInst, pin: TriState[Bool], name: String): Unit = {
    pin.writeEnable := reg.field(Bool(), AccessType.RW, 0, s"$name: pull the open-drain line low")(SymbolName(name))
    pin.write := False
  }

  /** EVENTS (pin changes, sticky, write 1 to clear) and IRQ_ENABLE: the interrupt. */
  def events(busif: BusIf, pins: Seq[(String, Bool)]): Bool = {
    val EVENTS = busif.newReg("Sticky pin changes, write 1 to clear")
    val clear = EVENTS.hitDoWrite ? busif.writeData | B(0, 32 bits)
    val bits = for (((name, pin), k) <- pins.zipWithIndex) yield {
      val bit = RegInit(False)
      when(clear(k))(bit := False)
      when(pin =/= RegNext(pin).init(False))(bit := True)
      EVENTS.field(Bool(), AccessType.RO, 0, s"$name changed")(SymbolName(name)) := bit
      bit
    }
    val IRQ_ENABLE = busif.newReg("Interrupt sources: EVENTS bits")
    val enables = IRQ_ENABLE.field(Bits(pins.length bits), AccessType.RW, 0, "same bits as EVENTS")(SymbolName("events"))
    RegNext((Vec(bits).asBits & enables).orR) init (False)
  }

  /** A PON side's Ethernet bridge: the Port-ID lookup in the MAC's GEM receive domain, the queues on the same clocks
    * reset while `down` (asynchronous: either lane not ready); 1K IPv4 flows per direction (block RAM budget). */
  def ethBridge(ponRx: ClockDomain, ponTx: ClockDomain, ethTxClk: Bool, ethRxClk: Bool, down: Bool,
                defaultAction: Int, ponQueues: Int = 1): GemEthBridge = {
    def domain(clk: Bool): ClockDomain = ClockDomain(clk, ClockDomain(clk)(BufferCC(down)))
    GemEthBridge(ponRx, domain(ponRx.clock), domain(ponTx.clock), domain(ethTxClk), domain(ethRxClk), defaultAction, ponQueues,
      flowBucketBits = 9)
  }

  /** The bridge's frame queues for the CPU (GemCpuQueues), on the bridge's clocks, reset while `down`: LAN (Ethernet
    * frames in and out, Port-ID 0), WAN (PON frames in, with their Port-ID, and out), then the control frames of each
    * side (rate limited by the bridge; receive only, their TX refuses every packet). */
  case class BridgeQueues(bridge: GemEthBridge, down: Bool) extends Area {
    val cd = ClockDomain.current.copy(reset = ClockDomain.current.readResetWire || RegNext(down).init(True))
    val lan = cd(gpon.GemCpuQueues(bridge.ethTxCd, bridge.ponTxCd))
    val wan = cd(gpon.GemCpuQueues(bridge.ponTxCd, bridge.ethTxCd))
    val lanControl = cd(gpon.GemCpuQueues(bridge.ethTxCd, bridge.ponTxCd, txWords = 4, rxWords = 512))
    val wanControl = cd(gpon.GemCpuQueues(bridge.ponTxCd, bridge.ethTxCd, txWords = 4, rxWords = 512))
    lan.io.rx << bridge.ponTxCd(bridge.io.lanToCpu.stage()) // registered before the queue RAMs (311.04 MHz)
    bridge.io.cpuToLan << lan.io.tx
    wan.io.rx << bridge.ethTxCd(bridge.io.wanToCpu.stage())
    bridge.io.cpuToWan << wan.io.tx
    lanControl.io.rx << bridge.io.lanControlToCpu
    wanControl.io.rx << bridge.io.wanControlToCpu
    val all = Seq(lan, wan, lanControl, wanControl)
    for (q <- all) q.io.rxCut := False
    for (q <- Seq(lanControl, wanControl)) q.io.tx.ready := False
    def slaves(up: Bool): Seq[(Apb3, Bool)] = all.map(_.io.apb -> up)
    def irqs: Bits = Vec(all.map(_.io.irq)).asBits
  }
}

/** HPS lightweight bridge (AXI4, single 32-bit accesses: readl/writel) -> APB3 slaves of 256 bytes each, slave k at
  * 0x100 * k; a slave whose `up` is low, or no slave, answers an error at once, so the CPU never hangs. */
object HpsBridge {
  val addressWidth = 12
  def axiConfig: Axi4Config = Axi4SharedToApb3Bridge.getConfigs(addressWidth, 32, 4)._1

  def apply(axi: Axi4, slaves: Seq[(Apb3, Bool)]): Unit = {
    val bridge = Axi4SharedToApb3Bridge(addressWidth, 32, 4)
    bridge.io.axi << axi.toShared()
    val apb = bridge.io.apb
    val index = apb.PADDR(addressWidth - 1 downto 8)
    apb.PREADY := True
    apb.PRDATA := 0
    val error = CombInit(True)
    for (((slave, up), k) <- slaves.zipWithIndex) {
      slave.PADDR := apb.PADDR.resized
      slave.PENABLE := apb.PENABLE
      slave.PWRITE := apb.PWRITE
      slave.PWDATA := apb.PWDATA
      slave.PSEL(0) := apb.PSEL(0) && index === k && up
      when(index === k) {
        apb.PREADY := slave.PREADY || !up
        apb.PRDATA := slave.PRDATA
        error := slave.PSLVERROR || !up
      }
    }
    // Spinal 1.15.1's bridge reads PSLVERROR after the access: latched
    apb.PSLVERROR := RegNextWhen(error, apb.PSEL(0) && apb.PENABLE && apb.PREADY) init (False)
  }
}

/** Clock domains of a PON MAC on a GTS lane: downstream (core clock) and upstream (half), both held in reset while
  * `down` (asynchronous), each leaving it on its own clock. */
case class PonDomains(dsClk: Bool, usClk: Bool, down: Bool) {
  val dsReset = ClockDomain(dsClk)(BufferCC(down))
  val usReset = ClockDomain(usClk)(BufferCC(down))
  val ds = ClockDomain(dsClk, dsReset)
  val us = ClockDomain(usClk, usReset)
  ds.setSynchronousWith(us)

  /** Downstream domain: high on one downstream clock per upstream clock, to take each upstream beat once. */
  lazy val usBeat: Bool = {
    val toggle = us(RegInit(False))
    toggle := !toggle
    val seen = ds(RegNext(toggle) init (False))
    seen =/= ds(RegNext(seen) init (False))
  }
}
