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
  * VLAN ID (TPID 0x8100 / 0x88A8) or the untagged default picks the Port-ID, directly or through an 802.1p mapper
  * (Port-ID by PCP, G.988 ME 130; untagged frames: a default PCP), and the Port-ID's queue (its T-CONT) takes the
  * frame: ponQueues > 1 moves each frame from the Ethernet queue into its own queue, which drops when full (no
  * head-of-line blocking). VLAN treatments (G.988 ME 171 subset: remove and/or add the outer tag, PCP set or kept),
  * upstream by the frame's entry, downstream by its outer VID; treatment 0 is transparent. The FCS is appended after
  * the rewrite. Frames are stored whole (descriptor after the data), dropped when a queue is full or the frame bad.
  * Tables written by the CPU.
  * Clocks: the Port-ID lookup and the CPU stream on ponRxCd (reset with the PON side only: the CPU keeps its GEM
  * while the Ethernet lane is down); the queues on ponQueueCd (ponRxCd's clock), ponTxCd, ethTxCd and ethRxCd, all
  * reset while either side is down. cpuPort (the OMCI Port-ID) always takes the CPU action. */
case class GemEthBridge(ponRxCd: ClockDomain, ponQueueCd: ClockDomain, ponTxCd: ClockDomain, ethTxCd: ClockDomain,
                        ethRxCd: ClockDomain, defaultAction: Int, ponQueues: Int = 1, queueBytes: Int = 16384,
                        frames: Int = 64) extends Component {
  ponQueueCd.setSynchronousWith(ponRxCd)
  val io = new Bundle {
    val apb = slave(Apb3(Apb3Config(addressWidth = 8, dataWidth = 32)))
    val ponRx = slave Flow (Fragment(UsGemByte())) // ponRxCd: GEM from the MAC
    val ponRxCut = in Bool ()
    val cpuPort = slave Flow (UInt(12 bits)) // ponRxCd
    val toCpu = master Flow (Fragment(UsGemByte())) // ponRxCd: the CPU action, two clocks later
    val toCpuCut = out Bool ()
    val ponTx = Vec(master Stream (Fragment(GemTxByte())), ponQueues) // ponTxCd: whole packets, a byte per clock once started
    val ponTxBacklog = out Vec (UInt(16 bits), ponQueues) // ponTxCd: bytes queued
    val eth = master(EthFrames()) // tx in ethTxCd, rx in ethRxCd
  }
  val drop = 0
  val cpu = 1
  val eth = 2
  // a write waits while a table write is crossing (the CPU may write back to back)
  val bus = Apb3(io.apb.config)
  val tableBusy = Bool()
  val stall = io.apb.PWRITE && tableBusy
  bus.PADDR := io.apb.PADDR
  bus.PSEL := io.apb.PSEL
  bus.PWRITE := io.apb.PWRITE
  bus.PWDATA := io.apb.PWDATA
  bus.PENABLE := io.apb.PENABLE && !stall
  io.apb.PREADY := bus.PREADY && !stall
  io.apb.PRDATA := bus.PRDATA
  io.apb.PSLVERROR := bus.PSLVERROR
  val busif = BusInterface(bus, SizeMapping(0, 256))
  val PORT_ACTION = busif.newReg("PON -> Ethernet: write sets the action of a Port-ID")
  PORT_ACTION.field(UInt(12 bits), AccessType.WO, 0, "Port-ID")(SymbolName("portId"))
  PORT_ACTION.field(UInt(2 bits), AccessType.WO, 0, "action: 0 drop, 1 CPU, 2 Ethernet")(SymbolName("action"))
  val VLAN_MAP = busif.newReg("Ethernet -> PON: write maps a VLAN ID")
  VLAN_MAP.field(UInt(12 bits), AccessType.WO, 0, "VLAN ID")(SymbolName("vid"))
  VLAN_MAP.field(UInt(12 bits), AccessType.WO, 0, "Port-ID, or the mapper in bits 2-0")(SymbolName("portId"))
  VLAN_MAP.field(Bool(), AccessType.WO, 0, "valid, else frames of this VLAN are dropped")(SymbolName("valid"))
  VLAN_MAP.field(Bool(), AccessType.WO, 0, "mapper: the Port-ID by PCP")(SymbolName("mapper"))
  VLAN_MAP.field(UInt(4 bits), AccessType.WO, 0, "upstream VLAN treatment")(SymbolName("treatment"))
  val UNTAGGED = busif.newReg("Ethernet -> PON: untagged frames")
  val untagged = UNTAGGED.field(Bits(18 bits), AccessType.RW, 0, "as VLAN_MAP bits 29-12")(SymbolName("entry"))
  val untaggedPcp = UNTAGGED.field(Bits(3 bits), AccessType.RW, 0, "PCP for the mapper")(SymbolName("pcp"))
  val DROPS = busif.newReg("Frames dropped (bad, queue full, unmapped), counts")
  val PCP_MAP = busif.newReg("Ethernet -> PON: write sets a mapper's Port-ID for a PCP")
  PCP_MAP.field(UInt(12 bits), AccessType.WO, 0, "Port-ID")(SymbolName("portId"))
  PCP_MAP.field(UInt(3 bits), AccessType.WO, 0, "PCP")(SymbolName("pcp"))
  PCP_MAP.field(UInt(3 bits), AccessType.WO, 0, "mapper")(SymbolName("mapper"))
  val queueBits = log2Up(ponQueues) max 1
  val PORT_QUEUE = busif.newReg("Ethernet -> PON: write sets the queue (T-CONT) of a Port-ID")
  PORT_QUEUE.field(UInt(12 bits), AccessType.WO, 0, "Port-ID")(SymbolName("portId"))
  PORT_QUEUE.field(UInt(queueBits bits), AccessType.WO, 0, s"queue, 0-${ponQueues - 1}")(SymbolName("queue"))
  def treatmentReg(name: String, doc: String): RegInst = {
    val reg = busif.newReg(s"$doc: write sets a VLAN treatment")(SymbolName(name))
    reg.field(UInt(4 bits), AccessType.WO, 0, "treatment, 1-15 (0: transparent)")(SymbolName("index"))
    for ((f, d) <- Seq("remove" -> "remove the outer tag", "add" -> "add a tag", "pcpCopy" -> "its PCP from the frame",
           "sTag" -> "its TPID 0x88A8, else 0x8100"))
      reg.field(Bool(), AccessType.WO, 0, d)(SymbolName(f))
    reg.field(Bits(16 bits), AccessType.WO, 0, "its TCI (PCP, DEI, VID)")(SymbolName("tci"))
    reg
  }
  val US_TREATMENT = treatmentReg("US_TREATMENT", "Ethernet -> PON")
  val DS_VLAN = busif.newReg("PON -> Ethernet: write sets the VLAN treatment of an outer VID (untagged: transparent)")
  DS_VLAN.field(UInt(12 bits), AccessType.WO, 0, "VLAN ID")(SymbolName("vid"))
  DS_VLAN.field(UInt(4 bits), AccessType.WO, 0, "treatment")(SymbolName("treatment"))
  val DS_TREATMENT = treatmentReg("DS_TREATMENT", "PON -> Ethernet")
  /** The register's writes into the table's domain, one in flight (busy); the sending side resets with the table's
    * domain, so a domain going down does not hold the bus. */
  val busies = scala.collection.mutable.ArrayBuffer[Bool]()
  def tableWrite(reg: RegInst, cd: ClockDomain): Flow[Bits] = {
    val write = Stream(Bits(32 bits))
    write.valid := reg.hitDoWrite
    write.payload := busif.writeData
    val down = BufferCC(cd.isResetActive, True)
    val crossed = StreamCCByToggle(write, ClockDomain.current.copy(reset = ClockDomain.current.readResetWire || down), cd)
    busies += !write.ready
    crossed.toFlow
  }
  val actionWrite = tableWrite(PORT_ACTION, ponRxCd)
  val vlanWrite = tableWrite(VLAN_MAP, ethRxCd)
  val pcpWrite = tableWrite(PCP_MAP, ethRxCd)
  val queueWrite = tableWrite(PORT_QUEUE, ethRxCd)
  val usTreatmentWrite = tableWrite(US_TREATMENT, ethRxCd)
  val dsVlanWrite = tableWrite(DS_VLAN, ponQueueCd)
  val dsTreatmentWrite = tableWrite(DS_TREATMENT, ponQueueCd)
  tableBusy := busies.orR

  /** Treatments 1-15 (written: index 3-0, then remove, add, pcpCopy, sTag, TCI); 0 stays transparent. */
  case class Treatments(write: Flow[Bits]) extends Area {
    val table = Mem(Bits(20 bits), 16) initBigInt (Seq.fill(16)(BigInt(0)))
    table.write(write.payload(3 downto 0).asUInt, write.payload(23 downto 4), write.valid && write.payload(3 downto 0) =/= 0)
    /** The treatment of the frame: its tag (if tagged) and PCP. */
    def resolve(entry: Bits, tagged: Bool, pcp: Bits): VlanTreatment = {
      val t = VlanTreatment()
      t.remove := entry(0) && tagged
      t.add := entry(1)
      t.tag := (entry(3) ? B"16'x88A8" | B"16'x8100") ## (entry(2) ? pcp | entry(19 downto 17)) ## entry(16 downto 4)
      t
    }
  }

  case class EthDesc() extends Bundle { val bytes = UInt(12 bits); val good = Bool(); val vlan = VlanTreatment() }
  case class PonDesc() extends Bundle {
    val bytes, portId = UInt(12 bits)
    val queue = UInt(queueBits bits)
    val good = Bool()
    val vlan = VlanTreatment()
  }
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
    // outer tag: VID entry read at byte 15, the treatment at 17, both registered (311.04 MHz)
    val tpid = Reg(Bits(8 bits))
    val vid = Reg(Bits(4 bits))
    val pcp = Reg(Bits(3 bits))
    val tagged = Reg(Bool())
    when(byte) {
      when(stored === 12)(tpid := in.gem.data)
      when(stored === 13)(tagged := (tpid ## in.gem.data) === 0x8100 || (tpid ## in.gem.data) === 0x88A8)
      when(stored === 14) { vid := in.gem.data(3 downto 0); pcp := in.gem.data(7 downto 5) }
    }
    val dsVlans = Mem(UInt(4 bits), 4096) initBigInt (Seq.fill(4096)(BigInt(0)))
    dsVlans.write(dsVlanWrite.payload(11 downto 0).asUInt, dsVlanWrite.payload(15 downto 12).asUInt, dsVlanWrite.valid)
    val treatmentIndex = RegNext(tagged ? dsVlans.readSync((vid ## in.gem.data).asUInt, byte && stored === 15) | U(0, 4 bits))
    val treatments = Treatments(dsTreatmentWrite)
    val treatment = RegNext(treatments.resolve(treatments.table.readSync(treatmentIndex, byte && stored === 17), tagged, pcp))
    val ends = byte && in.last && in.gem.pti(0) || cut && open
    val badNow = (starting ? False | bad) || take && !ethData.io.push.ready || byte && !take
    ethDescs.io.push.valid := ends && !skipNow
    ethDescs.io.push.bytes := stored + ethData.io.push.fire.asUInt
    val long = stored >= 64 || stored === 63 && ethData.io.push.fire // bytes >= 64, off the adder
    ethDescs.io.push.good := !badNow && !(cut && open) && crcNow === Crc32.residue && long
    ethDescs.io.push.vlan := treatment
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
    val vlan = Reg(VlanTreatment())
    desc.ready := left === 0
    when(desc.fire) { left := desc.bytes; send := desc.good; vlan := desc.vlan }
    val data = ethData.io.pop
    val payload = send && left > 4 // the FCS (last 4 bytes) is the MAC's
    val rewrite = VlanRewrite(appendFcs = false)
    rewrite.io.input.valid := left =/= 0 && payload && data.valid
    rewrite.io.input.fragment := data.payload
    rewrite.io.input.last := left === 5
    rewrite.io.treatment := vlan
    io.eth.tx << rewrite.io.output
    data.ready := left =/= 0 && (payload ? rewrite.io.input.ready | True)
    when(data.fire)(left := left - 1)
  }

  val ethToPon = new ClockingArea(ethRxCd) {
    // entries: treatment ## mapper ## valid ## Port-ID (or mapper)
    val vlans = Mem(Bits(18 bits), 4096) initBigInt (Seq.fill(4096)(BigInt(0)))
    vlans.write(vlanWrite.payload(11 downto 0).asUInt, vlanWrite.payload(29 downto 12), vlanWrite.valid)
    val pcpMaps = Mem(UInt(12 bits), 64)
    pcpMaps.write(pcpWrite.payload(17 downto 12).asUInt, pcpWrite.payload(11 downto 0).asUInt, pcpWrite.valid)
    val queues = Mem(UInt(queueBits bits), 4096) initBigInt (Seq.fill(4096)(BigInt(0)))
    queues.write(queueWrite.payload(11 downto 0).asUInt, queueWrite.payload(12 + queueBits - 1 downto 12).asUInt, queueWrite.valid)
    val untaggedEntry = BufferCC(untaggedPcp ## untagged, B(0, 21 bits)) // static configuration
    val treatments = Treatments(usTreatmentWrite)
    val rx = io.eth.rx
    val open, skip, bad = RegInit(False)
    val index = Reg(UInt(12 bits))
    val tpid = Reg(Bits(8 bits))
    val vid = Reg(Bits(4 bits))
    val pcp = Reg(Bits(3 bits))
    val tagged = Reg(Bool())
    val lookup = Reg(Bool())
    val starting = rx.valid && !open
    val skipNow = starting ? !ponDescs.io.push.ready | skip
    val position = starting ? U(0, 12 bits) | index // received bytes
    val stored = Reg(UInt(12 bits))                   // bytes in the queue
    val take = rx.valid && !skipNow && position < 2040 // longer frames: bad, the rest not stored (room for the FCS)
    val badNow = (starting ? False | bad) || rx.last && io.eth.rxBad || !take || take && !ponData.io.push.ready
    when(rx.valid) {
      open := !rx.last
      skip := skipNow
      index := position + (position =/= 4095).asUInt
      when(position === 12)(tpid := rx.fragment)
      when(position === 13)(tagged := (tpid ## rx.fragment) === 0x8100 || (tpid ## rx.fragment) === 0x88A8)
      when(position === 14) { vid := rx.fragment(3 downto 0); pcp := rx.fragment(7 downto 5) }
      bad := badNow
    }
    val map = vlans.readSync((vid ## rx.fragment).asUInt, rx.valid && position === 15)
    when(rx.valid && position === 15)(lookup := True)
    when(starting)(lookup := False)
    // the entry at byte 16; the mapper's Port-ID and the treatment at 17, the queue at 18: frames of 19 bytes or more
    val entry = (tagged && lookup) ? map | untaggedEntry(17 downto 0)
    val framePcp = tagged ? pcp | untaggedEntry(20 downto 18)
    val mapped = pcpMaps.readSync((entry(2 downto 0) ## framePcp).asUInt, rx.valid && position === 16)
    val portId = entry(13) ? mapped | entry(11 downto 0).asUInt
    val queue = queues.readSync(portId, rx.valid && position === 17)
    val treatment = treatments.resolve(treatments.table.readSync(entry(17 downto 14).asUInt, rx.valid && position === 16), tagged, framePcp)
    val frameEnd = rx.valid && rx.last
    ponData.io.push.valid := take
    ponData.io.push.payload := rx.fragment
    val storedNow = ((starting && !skipNow) ? U(0, 12 bits) | stored) + ponData.io.push.fire.asUInt
    stored := storedNow
    ponDescs.io.push.valid := frameEnd && !skipNow
    ponDescs.io.push.bytes := storedNow
    ponDescs.io.push.portId := portId
    ponDescs.io.push.payload.queue := queue
    ponDescs.io.push.good := !badNow && entry(12) && position >= 18
    ponDescs.io.push.vlan := treatment
    val dropped = frameEnd && skipNow || ponDescs.io.push.valid && !ponDescs.io.push.good
  }

  /** Whole packets of a queue, rewritten, the FCS appended; its bad ones skipped. */
  def sender(desc: Stream[PonDesc], data: Stream[Bits]): Stream[Fragment[GemTxByte]] = new Area {
    val left = Reg(UInt(12 bits)) init (0)
    val head = Reg(PonDesc())
    val rewrite = VlanRewrite(appendFcs = true)
    desc.ready := left === 0 && rewrite.io.idle // the packet's Port-ID and length held to its FCS
    when(desc.fire) { left := desc.bytes; head := desc.payload }
    rewrite.io.input.valid := left =/= 0 && head.good && data.valid
    rewrite.io.input.fragment := data.payload
    rewrite.io.input.last := left === 1
    rewrite.io.treatment := head.vlan
    data.ready := left =/= 0 && (head.good ? rewrite.io.input.ready | True)
    when(data.fire)(left := left - 1)
    val length = head.bytes + 4 - (head.vlan.remove ? U(4) | U(0)) + (head.vlan.add ? U(4) | U(0))
    val out = rewrite.io.output.translateWith {
      val g = Fragment(GemTxByte())
      g.fragment.data := rewrite.io.output.fragment
      g.fragment.portId := head.portId
      g.fragment.length := length.resized
      g.last := rewrite.io.output.last
      g
    }
  }.out.pipelined(m2s = true, s2m = true)

  val ponSide = new ClockingArea(ponTxCd) {
    val dropped = Bool()
    if (ponQueues == 1) {
      dropped := False
      io.ponTx(0) << sender(ponDescs.io.pop, ponData.io.pop)
      io.ponTxBacklog(0) := ponData.io.popOccupancy.resized
    } else {
      // each frame into its queue if it fits there (room reserved by its descriptor), else dropped
      val queues = Seq.fill(ponQueues)(new Area {
        val data = StreamFifo(Bits(8 bits), queueBytes / ponQueues)
        val descs = StreamFifo(PonDesc(), frames / ponQueues)
      })
      val desc = ponDescs.io.pop
      val left = Reg(UInt(12 bits)) init (0)
      val keep = Reg(Bool())
      val head = Reg(PonDesc())
      val room = Vec(queues.map(q => q.data.io.availability >= desc.bytes && q.descs.io.availability =/= 0))
      desc.ready := left === 0
      when(desc.fire) { left := desc.bytes; keep := desc.good && room(desc.payload.queue); head := desc.payload }
      dropped := desc.fire && desc.good && !room(desc.payload.queue)
      val data = ponData.io.pop
      data.ready := left =/= 0
      when(data.fire)(left := left - 1)
      for ((q, k) <- queues.zipWithIndex) {
        val mine = keep && head.queue === k
        q.data.io.push.valid := data.fire && mine
        q.data.io.push.payload := data.payload
        q.descs.io.push.valid := data.fire && left === 1 && mine
        q.descs.io.push.payload := head
        io.ponTx(k) << sender(q.descs.io.pop, q.data.io.pop)
        io.ponTxBacklog(k) := q.data.io.occupancy.resized
      }
    }
  }

  val ponDrops = Reg(UInt(16 bits)) init (0)
  val ethDrops = Reg(UInt(16 bits)) init (0)
  when(PulseCCByToggle(ponToEth.dropped, ponQueueCd, ClockDomain.current))(ponDrops := ponDrops + 1)
  ethDrops := ethDrops + PulseCCByToggle(ethToPon.dropped, ethRxCd, ClockDomain.current).asUInt +
    PulseCCByToggle(ponSide.dropped, ponTxCd, ClockDomain.current).asUInt
  DROPS.field(UInt(16 bits), AccessType.RO, 0, "PON -> Ethernet")(SymbolName("ponToEth")) := ponDrops
  DROPS.field(UInt(16 bits), AccessType.RO, 0, "Ethernet -> PON")(SymbolName("ethToPon")) := ethDrops
  busif.accept(DocCHeader("bridge_regs", "BRIDGE"))
}
