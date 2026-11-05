// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.lib._
import pon.gpon._

/** One bit of a 4096-entry filter map (Alloc-ID or Port-ID). */
case class KeepWrite() extends Bundle {
  val id = UInt(12 bits)
  val keep = Bool()
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

/** Downstream sniffer on a GTC deframer's fields (311.04 MHz): the records of one ONU.
  * PLOAM (kind 0, 12 bytes): to `onuId` or broadcast, all with `allPloams`; flag 0: bad CRC.
  * Allocation (kind 1, 7 bytes as on the line): the BWmap entries of the Alloc-IDs set in the alloc map; flag 0: bad CRC.
  * GEM fragment (kind 2): Port-ID (16), PLI (16), crypto-counter (48), then the bytes as received, still ciphered:
  * the software decrypts with the counter. Port-IDs from the port map; flags 2..0: PTI, 3: cut short. */
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
  val ploams = RecordWriter(12, 2, 8)
  val ploam = io.fields.ploam
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
  val allocHit = allocMap.readSync(io.fields.alloc.value.allocId)
  val alloc = io.fields.alloc.stage()
  allocs.io.data.setIdle()
  allocs.io.close.valid := alloc.valid && allocHit
  allocs.io.close.kind := RecordKind.alloc
  allocs.io.close.flags := B(0, 7 bits) ## !alloc.crcOk
  allocs.io.close.time := io.time
  allocs.io.close.extra := alloc.value.line
  io.kept.valid := alloc.valid && allocHit && alloc.crcOk
  io.kept.payload := alloc.value

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
  close.kind := RecordKind.gem
  close.flags := B(0, 4 bits) ## cut ## (gem.valid ? gem.pti | pti)
  close.time := start
  close.extra := B(0, 4 bits) ## meta.portId ## B(0, 4 bits) ## meta.pli ## B(0, 2 bits) ## meta.counter
  gems.io.data << data.stage() // both a clock later: the map's RAM output is not the header push's (311.04 MHz)
  gems.io.close << close.stage()
  open := (open || data.valid) && !close.valid

  val writers = Seq(ploams, allocs, gems)
  io.output << StreamArbiterFactory.roundRobin.fragmentLock.on(writers.map(_.io.output))
  io.drops := writers.map(_.io.drops).reduce(_ + _)
  io.truncations := writers.map(_.io.truncations).reduce(_ + _)
}
