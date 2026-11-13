// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.lib._
import spinal.lib.bus.regif._
import pon.gpon._

/** One bit of a 4096-entry filter map (Alloc-ID or Port-ID). */
case class KeepWrite() extends Bundle {
  val id = UInt(12 bits)
  val keep = Bool()
}

object KeepWrite {
  /** A write-only register that sets one bit of a map: the id in bits 11-0, keep in bit 12. */
  def register(busif: BusIf, name: String, doc: String): Flow[KeepWrite] = {
    val reg = busif.newReg(doc)(SymbolName(name))
    reg.field(UInt(12 bits), AccessType.WO, 0, "id")(SymbolName("id"))
    reg.field(Bool(), AccessType.WO, 0, "keep")(SymbolName("keep"))
    val write = Flow(KeepWrite())
    write.valid := reg.hitDoWrite
    write.id := busif.writeData(11 downto 0).asUInt
    write.keep := busif.writeData(12)
    write
  }
}

/** The registers the host decoder (common/sw/sniffer) writes: the first four of every sniffer's map. */
case class FilterRegs(busif: BusIf, enableDoc: String) extends Area {
  val CTRL = busif.newReg("Sniffer control")(SymbolName("CTRL"))
  val enable = CTRL.field(Bool(), AccessType.RW, 0, s"enable: $enableDoc")(SymbolName("enable"))
  val allPloams = CTRL.field(Bool(), AccessType.RW, 0, "allPloams: every PLOAM, else to onuId or broadcast")(SymbolName("allPloams"))
  CTRL.reserved(6 bits)
  val onuId = CTRL.field(Bits(8 bits), AccessType.RW, 0, "onuId: the tapped ONU's")(SymbolName("onuId"))
  val delimiterErrors = CTRL.field(UInt(2 bits), AccessType.RW, 1, "delimiterErrors: wrong bits allowed")(SymbolName("delimiterErrors"))
  // reset to the usual one: zero would match the quiet line
  val DELIMITER = busif.newReg("Upstream delimiter, from the Upstream_Overhead PLOAM")(SymbolName("DELIMITER"))
  val delimiter = DELIMITER.field(Bits(24 bits), AccessType.RW, pon.gpon.GponCodes.usDelimiter, "delimiter")(SymbolName("delimiter"))
  val allocMap = KeepWrite.register(busif, "ALLOC_MAP", "Write: keep (or not) the BWmap entries of an Alloc-ID")
  val portMap = KeepWrite.register(busif, "PORT_MAP", "Write: keep (or not) the GEM fragments of a Port-ID")
}

/** What SnifferDs takes from a GtcDsDeframer (the ONU's own: one deframer, with FEC, per line). */
case class DsFields() extends Bundle {
  val synced, frameStart = Bool()
  val ploam = Flow(Checked(Bits(96 bits)))
  val alloc = Flow(Checked(Allocation()))
  val gem = Flow(Fragment(GemRxByte()))
  val gemCut = Bool()
  val gemHeader = Flow(GemHeaderEvent())
}

object DsFields {
  /** The fields of `deframer`, read from anywhere above it in the hierarchy. */
  def of(deframer: GtcDsDeframer): DsFields = {
    val f = DsFields()
    f.synced := deframer.io.synced.pull()
    f.frameStart := deframer.io.frameStart.pull()
    f.ploam := deframer.io.ploam.pull()
    f.alloc := deframer.io.alloc.pull()
    f.gem := deframer.io.gem.pull()
    f.gemCut := deframer.io.gemCut.pull()
    f.gemHeader := deframer.io.gemHeader.pull()
    f
  }
}

/** Kind 2 record head: Port-ID (16), PLI (16), crypto-counter (48), then the fragment's bytes as received, still
  * ciphered (the software decrypts with the counter); flags 2..0: PTI, 3: cut short. */
object GemRecord {
  def head(portId: UInt, pli: UInt, counter: UInt, pti: Bits, cut: Bool, time: UInt): RecordHead = {
    val h = RecordHead(10)
    h.kind := RecordKind.gem
    h.flags := B(0, 4 bits) ## cut ## pti
    h.time := time
    h.extra := B(0, 4 bits) ## portId ## B(0, 4 bits) ## pli ## B(0, 2 bits) ## counter
    h
  }
}

/** The PCBd records of one ONU, from any deframer's fields.
  * PLOAM (kind 0, 12 bytes): to `onuId` or broadcast, all with `allPloams`; flag 0: bad CRC.
  * Allocation (kind 1, 7 bytes as on the line): the BWmap entries of the Alloc-IDs set in the alloc map; flag 0: bad CRC. */
case class SnifferPcbd() extends Component {
  val io = new Bundle {
    val ploam = slave Flow (Checked(Bits(96 bits)))
    val alloc = slave Flow (Checked(Allocation()))
    val time = in UInt (64 bits)
    val onuId = in Bits (8 bits)
    val allPloams = in Bool ()
    val allocMap = slave Flow (KeepWrite())
    val kept = master Flow (Allocation()) // the kept entries with a good CRC, at their record's time
    val outputs = Vec(master(Stream(Fragment(Bits(8 bits)))), 2)
    val drops = out UInt (32 bits)
    val truncations = out UInt (32 bits)
  }
  val ploams = RecordWriter(12, 2, 8)
  val ploam = io.ploam
  val ploamOnu = ploam.value(95 downto 88)
  ploams.io.data.setIdle()
  ploams.io.close.valid := ploam.valid && (ploamOnu === io.onuId || ploamOnu === B"8'xFF" || io.allPloams)
  ploams.io.close.kind := RecordKind.ploam
  ploams.io.close.flags := B(0, 7 bits) ## !ploam.crcOk
  ploams.io.close.time := io.time
  ploams.io.close.extra := ploam.value

  val allocs = RecordWriter(7, 2, 16)
  val allocMap = Mem(Bool(), 4096) init (Seq.fill(4096)(False))
  allocMap.write(io.allocMap.id, io.allocMap.keep, io.allocMap.valid)
  val allocHit = allocMap.readSync(io.alloc.value.allocId)
  val alloc = io.alloc.stage()
  allocs.io.data.setIdle()
  allocs.io.close.valid := alloc.valid && allocHit
  allocs.io.close.kind := RecordKind.alloc
  allocs.io.close.flags := B(0, 7 bits) ## !alloc.crcOk
  allocs.io.close.time := io.time
  allocs.io.close.extra := alloc.value.line
  io.kept.valid := alloc.valid && allocHit && alloc.crcOk
  io.kept.payload := alloc.value

  io.outputs(0) << ploams.io.output
  io.outputs(1) << allocs.io.output
  io.drops := ploams.io.drops + allocs.io.drops
  io.truncations := ploams.io.truncations + allocs.io.truncations
}

/** Downstream sniffer on a GTC deframer's fields (311.04 MHz): the records of one ONU, PCBd (SnifferPcbd) and
  * GEM fragments (GemRecord) of the Port-IDs set in the port map. */
case class SnifferDs(gemDepth: Int = 16384) extends Component {
  val io = new Bundle {
    val fields = in(DsFields())
    val time = in UInt (64 bits)
    val onuId = in Bits (8 bits)
    val allPloams = in Bool ()
    val allocMap = slave Flow (KeepWrite())
    val portMap = slave Flow (KeepWrite())
    val kept = master Flow (Allocation()) // the kept entries with a good CRC, at their record's time
    val output = master Stream (Fragment(Bits(8 bits)))
    val drops = out UInt (32 bits)
    val truncations = out UInt (32 bits)
  }
  val pcbd = SnifferPcbd()
  pcbd.io.ploam := io.fields.ploam
  pcbd.io.alloc := io.fields.alloc
  pcbd.io.time := io.time
  pcbd.io.onuId := io.onuId
  pcbd.io.allPloams := io.allPloams
  pcbd.io.allocMap << io.allocMap
  io.kept << pcbd.io.kept

  // GEM: the header event comes a clock before the fragment's first byte, the map answers on that byte
  val gems = RecordWriter(10, gemDepth, 32)
  val portMap = Mem(Bool(), 4096) init (Seq.fill(4096)(False))
  portMap.write(io.portMap.id, io.portMap.keep, io.portMap.valid)
  val header = io.fields.gemHeader
  val gem = io.fields.gem
  val portHit = portMap.readSync(header.portId)
  val headerSeen = RegNext(header.valid) init (False)
  val kept = Reg(Bool()) init (False)
  val keep = headerSeen ? portHit | kept
  kept := keep
  val meta = RegNextWhen(header.payload, header.valid)
  val start = RegNextWhen(io.time, header.valid)
  val pti = RegNextWhen(gem.pti, gem.valid)
  val open = Reg(Bool()) init (False) // a kept fragment has bytes and no end yet
  val ended = gem.valid && gem.last
  val cut = !ended && open && (io.fields.gemCut || header.valid) // the frame end, or a new header, before its last byte
  val data = Flow(Bits(8 bits))
  val close = Flow(RecordHead(10))
  data.valid := gem.valid && keep
  data.payload := gem.data
  close.valid := keep && (ended || cut)
  close.payload := GemRecord.head(meta.portId, meta.pli, meta.counter, gem.valid ? gem.pti | pti, cut, start)
  gems.io.data << data.stage() // both a clock later: the map's RAM output is not the header push's (311.04 MHz)
  gems.io.close << close.stage()
  open := (open || data.valid) && !close.valid

  io.output << StreamArbiterFactory.roundRobin.fragmentLock.on(pcbd.io.outputs ++ Seq(gems.io.output))
  io.drops := pcbd.io.drops + gems.io.drops
  io.truncations := pcbd.io.truncations + gems.io.truncations
}
