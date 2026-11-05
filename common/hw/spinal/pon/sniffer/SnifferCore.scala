// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif._

/** Record bytes in 32-bit words, first byte in bits 31-24; a record's last word is padded with zeros. */
case class RecordWords() extends Component {
  val io = new Bundle {
    val input = slave Stream (Fragment(Bits(8 bits)))
    val output = master Stream (Bits(32 bits))
  }
  val held = Reg(Bits(24 bits))
  val count = Reg(UInt(2 bits)) init (0) // bytes held
  val full = io.input.last || count === 3
  val word = held ## io.input.fragment
  io.output.valid := io.input.valid && full
  io.output.payload := word |<< ((U(3, 2 bits) - count) << 3)
  io.input.ready := !full || io.output.ready
  when(io.input.fire) {
    held := word(23 downto 0)
    count := full ? U(0, 2 bits) | count + 1
  }
}

/** GPON sniffer of a passive tap: SnifferDs on a downstream deframer's fields, SnifferUs on the upstream line. APB3
  * and the downstream in the default domain (311.04 MHz), the upstream in usDomain (155.52 MHz, synchronous 2:1).
  * Nothing is taken while disabled. Each direction's records (RecordWriter format) are read as words (RecordWords),
  * a read of DS_DATA / US_DATA pops one; the record header gives its length. Time: default domain clocks. */
case class SnifferCore(usDomain: ClockDomain, gemDepth: Int = 4096, burstDepth: Int = 8192, usWords: Int = 512) extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(Apb3Config(addressWidth = 8, dataWidth = 32)))
    val ds = in(DsFields()) // the ONU's deframer
    val usLine = in Bits (8 bits) // usDomain
    val enabled = out Bool ()
    val irq = out Bool () // record words wait
  }
  val busif = BusInterface(io.apb, SizeMapping(0, 256))
  val CTRL = busif.newReg("Sniffer control")
  val enable = CTRL.field(Bool(), AccessType.RW, 0, "enable: record (the ONU laser is forced off meanwhile)")(SymbolName("enable"))
  val allPloams = CTRL.field(Bool(), AccessType.RW, 0, "allPloams: every PLOAM, else to onuId or broadcast")(SymbolName("allPloams"))
  CTRL.reserved(6 bits)
  val onuId = CTRL.field(Bits(8 bits), AccessType.RW, 0, "onuId: the tapped ONU's")(SymbolName("onuId"))
  val delimiterErrors = CTRL.field(UInt(2 bits), AccessType.RW, 1, "delimiterErrors: wrong bits allowed")(SymbolName("delimiterErrors"))
  val DELIMITER = busif.newReg("Upstream delimiter, from the Upstream_Overhead PLOAM")
  val delimiter = DELIMITER.field(Bits(24 bits), AccessType.RW, 0, "delimiter")(SymbolName("delimiter"))
  def mapReg(name: String, doc: String): Flow[KeepWrite] = {
    val reg = busif.newReg(doc)(SymbolName(name))
    reg.field(UInt(12 bits), AccessType.WO, 0, "id")(SymbolName("id"))
    reg.field(Bool(), AccessType.WO, 0, "keep")(SymbolName("keep"))
    val write = Flow(KeepWrite())
    write.valid := reg.hitDoWrite
    write.id := busif.writeData(11 downto 0).asUInt
    write.keep := busif.writeData(12)
    write
  }
  val allocMap = mapReg("ALLOC_MAP", "Write: keep (or not) the BWmap entries of an Alloc-ID")
  val portMap = mapReg("PORT_MAP", "Write: keep (or not) the GEM fragments of a Port-ID")
  io.enabled := enable

  val time = Reg(UInt(64 bits)) init (0)
  time := time + 1

  val ds = SnifferDs(gemDepth)
  val fields = ds.io.fields // gated, a clock later: the deframer also feeds the ONU MAC (311.04 MHz)
  fields.synced := RegNext(io.ds.synced) init (False)
  fields.ploam := io.ds.ploam.takeWhen(enable).stage()
  fields.alloc := io.ds.alloc.takeWhen(enable).stage()
  fields.gem := io.ds.gem.takeWhen(enable).stage()
  fields.gemCut := RegNext(io.ds.gemCut && enable) init (False)
  fields.gemHeader := io.ds.gemHeader.takeWhen(enable).stage()
  ds.io.time := time
  ds.io.onuId := onuId
  ds.io.allPloams := allPloams
  ds.io.allocMap << allocMap
  ds.io.portMap << portMap
  val dsWords = RecordWords()
  dsWords.io.input << ds.io.output.m2sPipe() // registered both sides at 311.04 MHz
  val dsFifo = StreamFifo(Bits(32 bits), 128)
  dsFifo.io.push << dsWords.io.output.m2sPipe()

  val us = usDomain(SnifferUs(depth = burstDepth))
  us.io.line := enable ? io.usLine | B(0, 8 bits)
  us.io.time := time
  us.io.delimiter := delimiter
  us.io.delimiterErrors := delimiterErrors
  val usWordsArea = new ClockingArea(usDomain) {
    val words = RecordWords()
    words.io.input << us.io.output
  }
  val usFifo = StreamFifoCC(Bits(32 bits), usWords, usDomain, ClockDomain.current)
  usFifo.io.push << usWordsArea.words.io.output

  val LEVEL = busif.newReg("Record words waiting, and the downstream sync")
  LEVEL.field(UInt(8 bits), AccessType.RO, 0, "dsWords")(SymbolName("dsWords")) := dsFifo.io.occupancy.resized
  LEVEL.field(UInt(16 bits), AccessType.RO, 0, "usWords")(SymbolName("usWords")) := usFifo.io.popOccupancy.resized
  LEVEL.field(Bool(), AccessType.RO, 0, "synced: downstream frames found")(SymbolName("synced")) := io.ds.synced
  def dataReg(name: String, fifo: Stream[Bits], doc: String): Unit = {
    val reg = busif.newReg(s"$doc record word, first byte in bits 31-24: reading it pops the word (check LEVEL first)")(SymbolName(name))
    reg.field(Bits(32 bits), AccessType.RO, 0, "bytes")(SymbolName("data")) := fifo.payload
    fifo.ready := reg.hitDoRead
  }
  dataReg("DS_DATA", dsFifo.io.pop, "Downstream")
  dataReg("US_DATA", usFifo.io.pop, "Upstream")
  for ((name, drops, truncations) <- Seq(("DS", ds.io.drops, ds.io.truncations), ("US", us.io.drops, us.io.truncations))) {
    val reg = busif.newReg(s"$name records lost whole (no header room) and cut short (data FIFO full), counts")(SymbolName(s"${name}_LOST"))
    reg.field(UInt(16 bits), AccessType.RO, 0, "drops")(SymbolName("drops")) := drops.resized
    reg.field(UInt(16 bits), AccessType.RO, 0, "truncations")(SymbolName("truncations")) := truncations.resized
  }
  io.irq := RegNext(dsFifo.io.pop.valid || usFifo.io.pop.valid) init (False)

  busif.accept(DocCHeader("sniffer_regs", "SNIFFER")) // sniffer_regs.h next to the Verilog
}
