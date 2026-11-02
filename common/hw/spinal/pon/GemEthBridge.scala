// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif._
import pon.gpon.{GemRxByte, GemTxByte, UsGemByte}

object Crc32 {
  /** Ethernet CRC-32 (reflected 0xEDB88320, register initialised to all ones), one byte, bit 0 first. */
  def step(crc: Bits, byte: Bits): Bits =
    (0 until 8).foldLeft(crc)((c, i) => (c |>> 1) ^ ((c(0) ^ byte(i)) ? B(BigInt("EDB88320", 16), 32 bits) | B(0, 32 bits)))
  val residue = BigInt("DEBB20E3", 16) // the register after a frame and its FCS
  def model(bytes: Seq[Int]): Long = ~bytes.foldLeft(0xFFFFFFFFL)((c, b) =>
    (0 until 8).foldLeft(c ^ b)((x, _) => if ((x & 1) == 1) (x >>> 1) ^ 0xEDB88320L else x >>> 1)) & 0xFFFFFFFFL
}

/** Frames between an Ethernet port and its user: master sends to the port (whole frames, the FCS the MAC's). */
case class EthFrames() extends Bundle with IMasterSlave {
  val tx = Stream(Fragment(Bits(8 bits)))
  val rx = Flow(Fragment(Bits(8 bits)))
  val rxBad = Bool() // with rx.last: FCS or code error
  override def asMaster(): Unit = { master(tx); slave(rx); in(rxBad) }
}

/** L2 bridge between a PON MAC's GEM streams and an Ethernet port (Ethernet over GEM, G.984.3 I.3: the GEM payload is
  * the frame from DA to FCS; the Ethernet MAC adds and checks the FCS itself). PON -> Ethernet: per Port-ID action
  * (drop, CPU, Ethernet), whole frames with a good FCS (stripped) and at least 64 bytes; Ethernet -> PON: the outer
  * VLAN ID (TPID 0x8100 / 0x88A8) or the untagged default picks the Port-ID, the FCS is appended. Frames are stored
  * whole (descriptor after the data), dropped when a queue is full or the frame bad. Tables written by the CPU.
  * Clocks: the Port-ID lookup and the CPU stream on ponRxCd (reset with the PON side only: the CPU keeps its GEM
  * while the Ethernet lane is down); the queues on ponQueueCd (ponRxCd's clock), ponTxCd, ethTxCd and ethRxCd, all
  * reset while either side is down. cpuPort (the OMCI Port-ID) always takes the CPU action. */
case class GemEthBridge(ponRxCd: ClockDomain, ponQueueCd: ClockDomain, ponTxCd: ClockDomain, ethTxCd: ClockDomain,
                        ethRxCd: ClockDomain, defaultAction: Int, queueBytes: Int = 16384, frames: Int = 64) extends Component {
  ponQueueCd.setSynchronousWith(ponRxCd)
  val io = new Bundle {
    val apb = slave(Apb3(Apb3Config(addressWidth = 8, dataWidth = 32)))
    val ponRx = slave Flow (Fragment(UsGemByte())) // ponRxCd: GEM from the MAC
    val ponRxCut = in Bool ()
    val cpuPort = slave Flow (UInt(12 bits)) // ponRxCd
    val toCpu = master Flow (Fragment(UsGemByte())) // ponRxCd: the CPU action, two clocks later
    val toCpuCut = out Bool ()
    val ponTx = master Stream (Fragment(GemTxByte())) // ponTxCd: whole packets, one byte per clock once started
    val ponTxBacklog = out UInt (16 bits) // ponTxCd: bytes queued
    val eth = master(EthFrames()) // tx in ethTxCd, rx in ethRxCd
  }
  val drop = 0
  val cpu = 1
  val eth = 2
  val busif = BusInterface(io.apb, SizeMapping(0, 256))
  val PORT_ACTION = busif.newReg("PON -> Ethernet: write sets the action of a Port-ID")
  PORT_ACTION.field(UInt(12 bits), AccessType.WO, 0, "Port-ID")(SymbolName("portId"))
  PORT_ACTION.field(UInt(2 bits), AccessType.WO, 0, "action: 0 drop, 1 CPU, 2 Ethernet")(SymbolName("action"))
  val VLAN_MAP = busif.newReg("Ethernet -> PON: write maps a VLAN ID")
  VLAN_MAP.field(UInt(12 bits), AccessType.WO, 0, "VLAN ID")(SymbolName("vid"))
  VLAN_MAP.field(UInt(12 bits), AccessType.WO, 0, "Port-ID")(SymbolName("portId"))
  VLAN_MAP.field(Bool(), AccessType.WO, 0, "valid, else frames of this VLAN are dropped")(SymbolName("valid"))
  val UNTAGGED = busif.newReg("Ethernet -> PON: untagged frames")
  val untaggedPort = UNTAGGED.field(UInt(12 bits), AccessType.RW, 0, "Port-ID")(SymbolName("portId"))
  val untaggedValid = UNTAGGED.field(Bool(), AccessType.RW, 0, "valid, else dropped")(SymbolName("valid"))
  def written(reg: RegInst): Flow[Bits] = { val f = Flow(Bits(32 bits)); f.valid := reg.hitDoWrite; f.payload := busif.writeData; f }
  val actionWrite = FlowCCByToggle(written(PORT_ACTION), ClockDomain.current, ponRxCd)
  val vlanWrite = FlowCCByToggle(written(VLAN_MAP), ClockDomain.current, ethRxCd)

  case class EthDesc() extends Bundle { val bytes = UInt(12 bits); val good = Bool() }
  case class PonDesc() extends Bundle { val bytes = UInt(12 bits); val portId = UInt(12 bits); val good = Bool() }
  val ethData = StreamFifoCC(Bits(8 bits), queueBytes, ponQueueCd, ethTxCd)
  val ethDescs = StreamFifoCC(EthDesc(), frames, ponQueueCd, ethTxCd)
  val ponData = StreamFifoCC(Bits(8 bits), queueBytes, ethRxCd, ponTxCd)
  val ponDescs = StreamFifoCC(PonDesc(), frames, ethRxCd, ponTxCd)

  val lookup = new ClockingArea(ponRxCd) {
    val actions = Mem(UInt(2 bits), 4096) initBigInt (Seq.fill(4096)(BigInt(defaultAction)))
    actions.write(actionWrite.payload(11 downto 0).asUInt, actionWrite.payload(13 downto 12).asUInt, actionWrite.valid)
    val forced = RegNext(io.cpuPort.valid && io.cpuPort.payload === io.ponRx.gem.portId) init (False)
    val action = RegNext(forced ? U(cpu, 2 bits) | actions.readSync(io.ponRx.gem.portId)) // after the RAM: 311.04 MHz in the ONU
    val in = Delay(io.ponRx, 2, init = io.ponRx.getZero)
    val cut = Delay(io.ponRxCut, 2, init = False)
    io.toCpu.valid := in.valid && action === cpu
    io.toCpu.payload := in.payload
    io.toCpuCut := cut
  }

  val ponToEth = new ClockingArea(ponQueueCd) { // one clock behind the lookup (311.04 MHz in the ONU)
    val in = RegNext(lookup.in)
    val byte = RegNext(lookup.in.valid && lookup.action === eth) init (False)
    val cut = RegNext(lookup.cut) init (False)
    // the frame to Ethernet: its fragments in a row up to the end-of-frame PTI; no descriptor room: dropped whole
    val open, bad = RegInit(False)
    val skip = Reg(Bool())
    val bytes = Reg(UInt(12 bits))
    val crc = Reg(Bits(32 bits))
    val starting = byte && !open
    val skipNow = starting ? !ethDescs.io.push.ready | skip
    val crcNow = Crc32.step(starting ? B(32 bits, default -> True) | crc, in.gem.data)
    val stored = starting ? U(0, 12 bits) | bytes
    val take = byte && !skipNow && stored =/= 2047 // longer frames: bad, the rest not stored
    ethData.io.push.valid := take
    ethData.io.push.payload := in.gem.data
    val ends = byte && in.last && in.gem.pti(0) || cut && open
    val badNow = (starting ? False | bad) || take && !ethData.io.push.ready || byte && !take
    ethDescs.io.push.valid := ends && !skipNow
    ethDescs.io.push.bytes := stored + ethData.io.push.fire.asUInt
    val long = stored >= 64 || stored === 63 && ethData.io.push.fire // bytes >= 64, off the adder
    ethDescs.io.push.good := !badNow && !(cut && open) && crcNow === Crc32.residue && long
    when(byte) {
      open := True
      skip := skipNow
      bad := badNow
      crc := crcNow
      bytes := ethDescs.io.push.bytes
    }
    when(ends)(open := False)
    val dropped = ends && (skipNow || !ethDescs.io.push.good)
  }

  val ethSender = new ClockingArea(ethTxCd) {
    val desc = ethDescs.io.pop
    val left = Reg(UInt(12 bits)) init (0)
    val send = Reg(Bool())
    desc.ready := left === 0
    when(desc.fire) { left := desc.bytes; send := desc.good }
    val data = ethData.io.pop
    val payload = send && left > 4 // the FCS (last 4 bytes) is the MAC's
    io.eth.tx.valid := left =/= 0 && payload && data.valid
    io.eth.tx.fragment := data.payload
    io.eth.tx.last := left === 5
    data.ready := left =/= 0 && (payload ? io.eth.tx.ready | True)
    when(data.fire)(left := left - 1)
  }

  val ethToPon = new ClockingArea(ethRxCd) {
    val vlans = Mem(Bits(13 bits), 4096) initBigInt (Seq.fill(4096)(BigInt(0)))
    vlans.write(vlanWrite.payload(11 downto 0).asUInt, vlanWrite.payload(24 downto 12), vlanWrite.valid) // valid ## Port-ID
    val untagged = BufferCC(untaggedValid ## untaggedPort, B(0, 13 bits)) // static configuration
    val rx = io.eth.rx
    val open, skip, bad, appending = RegInit(False)
    val index = Reg(UInt(12 bits))
    val tpid = Reg(Bits(8 bits))
    val vid = Reg(Bits(4 bits))
    val tagged = Reg(Bool())
    val lookup = Reg(Bool())
    val crc = Reg(Bits(32 bits))
    val fcsLeft = Reg(UInt(3 bits)) init (0)
    val starting = rx.valid && !open
    val skipNow = starting ? (!ponDescs.io.push.ready || appending) | skip
    val position = starting ? U(0, 12 bits) | index // received bytes
    val stored = Reg(UInt(12 bits))                   // bytes in the queue, the FCS included
    val take = rx.valid && !skipNow && position < 2040 // longer frames: bad, the rest not stored
    when(rx.valid) {
      open := !rx.last
      skip := skipNow
      index := position + (position =/= 4095).asUInt
      crc := Crc32.step(starting ? B(32 bits, default -> True) | crc, rx.fragment)
      when(position === 12)(tpid := rx.fragment)
      when(position === 13)(tagged := (tpid ## rx.fragment) === 0x8100 || (tpid ## rx.fragment) === 0x88A8)
      when(position === 14)(vid := rx.fragment(3 downto 0))
      bad := (starting ? False | bad) || rx.last && io.eth.rxBad || !take || take && !ponData.io.push.ready
    }
    val map = vlans.readSync((vid ## rx.fragment).asUInt, rx.valid && position === 15)
    when(rx.valid && position === 15)(lookup := True)
    when(starting)(lookup := False)
    val target = (tagged && lookup) ? map | untagged
    val frameEnd = rx.valid && rx.last
    when(frameEnd && !skipNow) { appending := True; fcsLeft := 4 }
    // the frame, then its FCS (the register complemented, low byte first) in the gap before the next frame
    val fcs = ~crc
    ponData.io.push.valid := take || fcsLeft =/= 0
    ponData.io.push.payload := (fcsLeft === 0) ? rx.fragment | fcs.subdivideIn(8 bits).read((U(4, 3 bits) - fcsLeft).resize(2))
    val fcsBad = fcsLeft =/= 0 && !ponData.io.push.ready
    when(fcsLeft =/= 0) { fcsLeft := fcsLeft - 1; bad := bad || fcsBad }
    val storedNow = ((starting && !skipNow) ? U(0, 12 bits) | stored) + ponData.io.push.fire.asUInt
    stored := storedNow
    ponDescs.io.push.valid := fcsLeft === 1
    ponDescs.io.push.bytes := storedNow
    ponDescs.io.push.portId := target(11 downto 0).asUInt
    ponDescs.io.push.good := !bad && !fcsBad && target(12) && index >= 14
    when(fcsLeft === 1)(appending := False)
    val dropped = frameEnd && skipNow || ponDescs.io.push.valid && !ponDescs.io.push.good
  }

  val ponSender = new ClockingArea(ponTxCd) {
    val desc = ponDescs.io.pop
    val left = Reg(UInt(12 bits)) init (0)
    val send = Reg(Bool())
    val portId = Reg(UInt(12 bits))
    val length = Reg(UInt(12 bits))
    desc.ready := left === 0
    when(desc.fire) { left := desc.bytes; length := desc.bytes; send := desc.good; portId := desc.portId }
    val data = ponData.io.pop
    val out = Stream(Fragment(GemTxByte()))
    out.valid := left =/= 0 && send && data.valid
    out.fragment.data := data.payload
    out.fragment.portId := portId
    out.fragment.length := length.resized
    out.last := left === 1
    data.ready := left =/= 0 && (send ? out.ready | True)
    when(data.fire)(left := left - 1)
    io.ponTx << out.pipelined(m2s = true, s2m = true)
    io.ponTxBacklog := ponData.io.popOccupancy.resized
  }

  val DROPS = busif.newReg("Frames dropped (bad, queue full, unmapped), counts")
  val ponDrops = Reg(UInt(16 bits)) init (0)
  val ethDrops = Reg(UInt(16 bits)) init (0)
  when(PulseCCByToggle(ponToEth.dropped, ponQueueCd, ClockDomain.current))(ponDrops := ponDrops + 1)
  when(PulseCCByToggle(ethToPon.dropped, ethRxCd, ClockDomain.current))(ethDrops := ethDrops + 1)
  DROPS.field(UInt(16 bits), AccessType.RO, 0, "PON -> Ethernet")(SymbolName("ponToEth")) := ponDrops
  DROPS.field(UInt(16 bits), AccessType.RO, 0, "Ethernet -> PON")(SymbolName("ethToPon")) := ethDrops
  busif.accept(DocCHeader("bridge_regs", "BRIDGE"))
}
