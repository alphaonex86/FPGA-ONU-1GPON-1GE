// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif._

/** The dump side of the in-line sniffer, in its 77.76 MHz domain: the records of one ONU to an FT2232H (Ft245), its
  * registers written and read over the same link. Per clock: 32 downstream and 16 upstream line bits, first in the MSB.
  * Records as the passive sniffer's (RecordWriter format): PLOAM, BWmap entry, GEM fragment (WideSnifferDs), burst
  * (SnifferUs behind UsReplay), each followed by zeros to a multiple of 4 bytes (RecordPad): the stream is a dump file
  * of the host tool. Time in downstream bytes (4 per clock).
  * Host to device, 6 bytes: 0x5A, the register's number (its address / 4; bit 7: write), the value (32 bits LE, any
  * for a read). A read is answered by a record of kind 4: the number, the value (LE). A command stops being parsed
  * after 2^22 clocks without a byte. The I2C lines move 256 clocks apart at least (a write to I2C holds the next
  * command that long), so a batch of writes is a 150 kHz bus. */
case class InlineDump(benDelay: Int = 0) extends Component {
  val io = new Bundle {
    val dsWord = in Bits (32 bits)
    val usWord = in Bits (16 bits)
    val usDropped = in Bool () // a word of a burst found the queue to this clock full
    val link = new Bundle {
      val rxfN, txeN = in Bool ()
      val rdN, wrN = out Bool ()
      val read = in Bits (8 bits)
      val write = out Bits (8 bits)
      val drive = out Bool ()
    }
    val sclLow, scl2Low, sdaLow = out Bool () // open drain: pulls the line low
    val scl, sda = in Bool ()
    // to and from the line side, another clock: steady or synchronized there
    val dsLaser, usLaser, apdOn = out Bool ()
    val head = out UInt (3 bits)
    val garble = out UInt (2 bits)
    val benDelay = out UInt (4 bits)
    val dsSignal, dsOn, burstCut = in Bool ()
    val duty = in UInt (16 bits)
    val level = slave Flow (Bool()) // an upstream burst ended; its APD current had passed the comparator's level
    val synced = out Bool ()
  }
  val apb = Apb3(Apb3Config(addressWidth = 8, dataWidth = 32))
  val busif = BusInterface(apb, SizeMapping(0, 256))
  val filter = FilterRegs(busif, "record")
  import filter._
  val ID = busif.newReg("Identity")
  ID.field(Bits(32 bits), AccessType.RO, 0, "magic: 0x47530001")(SymbolName("magic")) := B(0x47530001L, 32 bits)
  val LASERS = busif.newReg("Lasers: off until the host has set their levels, kept on when it leaves")
  io.dsLaser := LASERS.field(Bool(), AccessType.RW, 0, "ds: downstream laser on while the downstream is received")(SymbolName("ds"))
  io.usLaser := LASERS.field(Bool(), AccessType.RW, 0, "us: upstream laser on during the ONU's bursts")(SymbolName("us"))
  io.apdOn := LASERS.field(Bool(), AccessType.RW, 0, "apd: both APD bias supplies on; a DAC at 0 V gives the highest bias, set them first")(SymbolName("apd"))
  val REGEN = busif.newReg("Upstream regenerator (UsRegen), half-words of 5 line bits")
  io.head := REGEN.field(UInt(3 bits), AccessType.RW, 3, "head: laser on before the first received one")(SymbolName("head"))
  REGEN.reserved(1 bits)
  io.garble := REGEN.field(UInt(2 bits), AccessType.RW, 2, "garble: rebuilt after the first received one")(SymbolName("garble"))
  REGEN.reserved(2 bits)
  io.benDelay := REGEN.field(UInt(4 bits), AccessType.RW, benDelay, "benDelay: laser enable after the data word (the SerDes TX latency)")(SymbolName("benDelay"))
  val I2C = busif.newReg("I2C lines of the level DACs / ADC, bit-banged; the two DACs have one address: each its clock, one data line")
  io.sclLow := !I2C.field(Bool(), AccessType.RW, 1, "scl: released (1) or low; the ADC and the OLT port's DAC")(SymbolName("scl"))
  io.sdaLow := !I2C.field(Bool(), AccessType.RW, 1, "sda: released (1) or low")(SymbolName("sda"))
  io.scl2Low := !I2C.field(Bool(), AccessType.RW, 1, "scl2: released (1) or low; the ONU port's DAC")(SymbolName("scl2"))
  I2C.reserved(5 bits)
  I2C.field(Bool(), AccessType.RO, 0, "sclIn: the line")(SymbolName("sclIn")) := BufferCC(io.scl, True)
  I2C.field(Bool(), AccessType.RO, 0, "sdaIn: the line")(SymbolName("sdaIn")) := BufferCC(io.sda, True)

  val time = Reg(UInt(64 bits)) init (0)
  time := time + 4

  val deframer = WideDsDeframer()
  deframer.io.word := io.dsWord
  io.synced := deframer.io.synced
  val ds = WideSnifferDs()
  ds.io.ploam := deframer.io.ploam.takeWhen(enable)
  ds.io.alloc := deframer.io.alloc.takeWhen(enable)
  ds.io.gem.header := deframer.io.gem.header.takeWhen(enable) // a fragment begun is finished
  ds.io.gem.chunk := deframer.io.gem.chunk
  ds.io.time := time
  ds.io.onuId := onuId
  ds.io.allPloams := allPloams
  ds.io.allocMap << allocMap
  ds.io.portMap << portMap

  val crossDrops = Reg(UInt(16 bits)) init (0)
  when(io.usDropped)(crossDrops := crossDrops + 1)
  val replay = UsReplay()
  replay.io.word := enable ? io.usWord | B(0, 16 bits)
  replay.io.time := time
  val us = SnifferUs(depth = 8192)
  us.io.line := replay.io.line
  us.io.time := replay.io.lineTime
  us.io.delimiter := delimiter
  us.io.delimiterErrors := delimiterErrors

  val STATUS = busif.newReg("State of the line")
  STATUS.field(Bool(), AccessType.RO, 0, "synced: downstream frames found")(SymbolName("synced")) := deframer.io.synced
  STATUS.field(Bool(), AccessType.RO, 0, "fec: downstream FEC on")(SymbolName("fec")) := deframer.io.fecIndication
  STATUS.field(Bool(), AccessType.RO, 0, "dsSignal: light and CDR lock at the OLT port")(SymbolName("dsSignal")) := BufferCC(io.dsSignal, False)
  STATUS.field(Bool(), AccessType.RO, 0, "dsOn: downstream sent to the ONU")(SymbolName("dsOn")) := BufferCC(io.dsOn, False)
  STATUS.field(Bool(), AccessType.RO, 0, "burstCut: the max-burst cutoff holds the upstream laser off")(SymbolName("burstCut")) := BufferCC(io.burstCut, False)
  STATUS.reserved(11 bits)
  STATUS.field(UInt(16 bits), AccessType.RO, 0, "duty: VCXO control, 0x8000 its middle")(SymbolName("duty")) := io.duty
  val ERRORS = busif.newReg("Downstream errors, counts")
  ERRORS.field(UInt(16 bits), AccessType.RO, 0, "hec: frames whose GEM stopped at a bad header")(SymbolName("hec")) := deframer.io.hecErrors.resized
  ERRORS.field(UInt(8 bits), AccessType.RO, 0, "plend: frames without a usable PLend")(SymbolName("plend")) := deframer.io.plendErrors.resized
  ERRORS.field(UInt(8 bits), AccessType.RO, 0, "syncLosses")(SymbolName("syncLosses")) := deframer.io.syncLosses.resized
  for ((name, drops, truncations) <- Seq(("DS", ds.io.drops, ds.io.truncations), ("US", us.io.drops, us.io.truncations))) {
    val reg = busif.newReg(s"$name records lost whole (no room) and cut short (data FIFO full), counts")(SymbolName(s"${name}_LOST"))
    reg.field(UInt(16 bits), AccessType.RO, 0, "drops")(SymbolName("drops")) := drops.resized
    reg.field(UInt(16 bits), AccessType.RO, 0, "truncations")(SymbolName("truncations")) := truncations.resized
  }
  val US_OVERFLOWS = busif.newReg("Bursts left out or cut short before the upstream sniffer (replay queue full), count")
  US_OVERFLOWS.field(UInt(16 bits), AccessType.RO, 0, "overflows")(SymbolName("overflows")) := (replay.io.overflows + crossDrops).resized
  val US_LEVEL = busif.newReg("Upstream bursts since the last write here, counts held at 255: the host moves the comparator's DAC between two")
  val bursts = Reg(UInt(8 bits)) init (0)
  val above = Reg(UInt(8 bits)) init (0)
  US_LEVEL.field(UInt(8 bits), AccessType.RO, 0, "bursts")(SymbolName("bursts")) := bursts
  US_LEVEL.field(UInt(8 bits), AccessType.RO, 0, "above: those whose APD current passed the comparator's level")(SymbolName("above")) := above
  when(io.level.valid) {
    when(!bursts.andR)(bursts := bursts + 1)
    when(io.level.payload && !above.andR)(above := above + 1)
  }
  when(US_LEVEL.hitDoWrite) {
    bursts := 0
    above := 0
  }
  busif.accept(DocCHeader("inline_regs", "INLINE")) // inline_regs.h next to the Verilog

  val link = Ft245()
  link.io.rxfN := io.link.rxfN
  link.io.txeN := io.link.txeN
  link.io.read := io.link.read
  io.link.rdN := link.io.rdN
  io.link.wrN := link.io.wrN
  io.link.write := link.io.write
  io.link.drive := link.io.drive

  // commands: parsed, then one APB access; a read answered before the next command is taken
  val command = new Area {
    val index = Reg(UInt(3 bits)) init (0)
    val shift = Reg(Bits(40 bits)) // the number, then the value
    val quiet = Reg(UInt(22 bits)) init (0)
    val phase = Reg(UInt(2 bits)) init (0) // 0 none, 1 setup, 2 access, 3 its reply or hold
    val hold = Reg(UInt(8 bits)) init (0)
    val number = shift(7 downto 0)
    val value = Reg(Bits(32 bits))
    val stamp = Reg(UInt(64 bits))
    val replying = Reg(Bool()) init (False)
    val replyIndex = Reg(UInt(5 bits))
    link.io.rxReady := phase === 0
    quiet := quiet + 1
    when(quiet.andR)(index := 0)
    when(link.io.rx.valid) {
      quiet := 0
      when(index === 0) {
        when(link.io.rx.payload === 0x5A)(index := 1)
      } otherwise {
        shift := link.io.rx.payload ## shift(39 downto 8)
        index := index + 1
        when(index === 5) {
          index := 0
          phase := 1
        }
      }
    }
    apb.PSEL := (phase === 1 || phase === 2).asBits
    apb.PENABLE := phase === 2
    apb.PWRITE := number(7)
    apb.PADDR := (number(5 downto 0) ## B"00").asUInt
    apb.PWDATA := shift(39 downto 8)
    when(hold =/= 0)(hold := hold - 1)
    switch(phase) {
      is(1)(phase := 2)
      is(2) {
        phase := 3
        value := apb.PRDATA
        stamp := time
        replyIndex := 0
        replying := !number(7)
        when(number(7) && apb.PADDR === I2C.getAddr())(hold := 255)
      }
      is(3)(when(!replying && hold === 0)(phase := 0))
    }
    val reply = Stream(Fragment(Bits(8 bits)))
    val bytes = Vec(RecordHead.bytes(U(RecordKind.reply, 3 bits), B(0, 8 bits), U(5, 16 bits), stamp) ++
      Seq(B"0" ## number(6 downto 0)) ++ value.subdivideIn(8 bits))
    reply.valid := replying
    reply.fragment := bytes(replyIndex)
    reply.last := replyIndex === bytes.length - 1
    when(reply.fire) {
      replyIndex := replyIndex + 1
      when(reply.last)(replying := False)
    }
  }

  // registered both ways (a byte every other clock, the link takes one in ten): the writers' paths end here
  val records = StreamArbiterFactory.roundRobin.fragmentLock.on(Seq(ds.io.output, us.io.output, command.reply).map(_.halfPipe()))
  val padded = RecordPad()
  padded.io.input << records
  link.io.tx << padded.io.output
}

/** In-line GPON sniffer: a repeater in one ONU's drop fibre that dumps its exchange with the OLT, vendor-neutral.
  * No VCXO: the reference is a fixed oscillator. Default domain: the downstream transmitter's clock (124.416 MHz),
  * which `txFaster` pulls onto the received downstream (the direction of the transmitter's phase interpolator steps,
  * each value held 8 clocks: SnifferBridge, in-line). `ds`: the downstream receiver's recovered clock. `out`: 77.76
  * MHz (the default x5/8), the dump (InlineDump). `us`: the oscillator's own 124.416 MHz, the whole upstream path:
  * the lane runs at 4 samples per bit, 40 per clock, the ONU's bits drift against it (UsClean follows them).
  * Lane words: first bit in bit 0. The downstream is sent again as received, once it has crossed without a slip for
  * 2^steadyBits clocks; the laser is off and the word zero otherwise. The upstream's cleaned samples go through
  * UsRegen at a constant delay, 1010 (in bits) while its laser is off; `usBen` is the laser enable for a DDR output
  * (bit 1 the first half of the word), `benDelay` half-words late. The dump gets the decided bits through a queue
  * filled only around bursts (a `tail` of zero words after each: the two clocks differ).
  * Upstream level: `usAbove` is a comparator on the APD current against a DAC; the bursts and those that passed
  * the level are counted (US_LEVEL), the host searches the level. `benDelay`: its reset value; `zeros`: half-words
  * of zeros after a burst's enable, then 1010. */
case class InlineSniffer(steadyBits: Int = 22, tail: Int = 4, benDelay: Int = 0, zeros: Int = 4) extends Component {
  val dsCd = ClockDomain.external("ds")
  val outCd = ClockDomain.external("out")
  val usCd = ClockDomain.external("us")
  val io = new Bundle {
    val dsRx = in Bits (20 bits) // ds domain
    val usRx = in Bits (40 bits) // us domain, as usTx, usBen and usLaser
    val dsSignal = in Bool () // async: light at the OLT port and its CDR locked
    val dsTx = out Bits (20 bits)
    val dsLaser = out Bool ()
    val usTx = out Bits (40 bits)
    val usBen = out Bits (2 bits)
    val usLaser = out Bool () // the host lets the upstream laser burst
    val usAbove = in Bool () // async, a comparator: the upstream APD current is over the level DAC's
    val burstCut = in Bool () // async: the board's max-burst cutoff holds the upstream laser off
    val apdOn = out Bool () // out domain: the APD bias supplies on
    val txFaster = out Bool ()
    val link = new Bundle { // out domain
      val rxfN, txeN = in Bool ()
      val rdN, wrN = out Bool ()
      val read = in Bits (8 bits)
      val write = out Bits (8 bits)
      val drive = out Bool ()
    }
    val sclLow, scl2Low, sdaLow = out Bool ()
    val scl, sda = in Bool ()
    val synced = out Bool () // out domain, for a LED
  }
  val bridge = SnifferBridge(inline = true, dsDomain = dsCd, outDomain = outCd)
  bridge.io.dsWord := io.dsRx
  bridge.io.dsSignal := io.dsSignal
  io.txFaster := bridge.io.vcxo

  val dump = outCd(InlineDump(benDelay))
  dump.io.dsWord := bridge.io.dsPins
  dump.io.link <> io.link
  dump.io.scl := io.scl
  dump.io.sda := io.sda
  io.sclLow := dump.io.sclLow
  io.scl2Low := dump.io.scl2Low
  io.sdaLow := dump.io.sdaLow
  io.synced := dump.io.synced
  dump.io.dsSignal := io.dsSignal
  dump.io.burstCut := io.burstCut
  io.apdOn := dump.io.apdOn

  val steady = Reg(UInt(steadyBits + 1 bits)) init (0)
  when(!bridge.io.dsValid)(steady := 0) elsewhen (!steady.msb)(steady := steady + 1)
  val dsOn = RegNext(steady.msb && BufferCC(dump.io.dsLaser, False)) init (False)
  io.dsLaser := RegNext(dsOn) init (False)
  io.dsTx := RegNext(dsOn ? bridge.io.dsLine.reversed | B(0, 20 bits)) init (0)
  dump.io.dsOn := dsOn

  // the duty to the dump's clock: sent every 2^16 clocks
  val dutyTick = Reg(UInt(16 bits)) init (0)
  dutyTick := dutyTick + 1
  val dutyFlow = Flow(UInt(16 bits))
  dutyFlow.valid := dutyTick === 0
  dutyFlow.payload := bridge.io.duty
  val dutyOut = FlowCCByToggle(dutyFlow, clockDomain, outCd)
  dump.io.duty := outCd(RegNextWhen(dutyOut.payload, dutyOut.valid) init (0x8000))

  val upstream = new ClockingArea(usCd) {
    val clean = UsClean()
    clean.io.samples := io.usRx.reversed
    val regen = UsRegen(spb = 4)
    regen.io.bits := clean.io.clean
    regen.io.enable := BufferCC(dump.io.usLaser, False)
    io.usLaser := RegNext(regen.io.enable) init (False)
    // the comparator sees the light before the burst has crossed the receiver: its last 8 clocks count
    val over = Reg(Bool()) init (False)
    over := regen.io.busy && (over || History(BufferCC(io.usAbove, False), 8).orR)
    val burstEnd = Flow(Bool())
    burstEnd.valid := regen.io.busy.fall(False)
    burstEnd.payload := over
    dump.io.level << FlowCCByToggle(burstEnd, usCd, outCd)
    regen.io.head := BufferCC(dump.io.head)
    regen.io.garble := BufferCC(dump.io.garble)
    // dark half-words carry 1010: the laser driver's AC-coupled input stays centred for the next burst. The first
    // `zeros` after a burst carry zeros: the laser enable may end a little after its data (the SerDes latency is not
    // whole half-words, the board adds its own) without sending light.
    val dark = B(BigInt("F0F0F0F0F0", 16), 40 bits)
    val benPast = Reg(Bits(16 bits)) init (0)
    val bens = benPast ## regen.io.ben // bit 1 and 0 the newest half-words, the older above
    benPast := bens(15 downto 0)
    val line = Cat((0 to 1).map { h =>
      val half = 20 * h + 19 downto 20 * h
      regen.io.ben(h) ? regen.io.output(half) | (bens(h + zeros downto h).orR ? B(0, 20 bits) | dark(half))
    })
    io.usTx := line.reversed
    val benDelay = BufferCC(dump.io.benDelay)
    io.usBen := RegNext(bens(benDelay + 1) ## bens(benDelay)) init (0)

    // the dump's copy: 16-bit words of the decided bits, from a burst's first word to `tail` zero words after it
    val buffer = Reg(Bits(26 bits)) init (0)
    val fill = Reg(UInt(5 bits)) init (0)
    val merged = buffer | ((clean.io.bits ## B(0, 15 bits)) |>> fill)
    val total = fill + clean.io.count
    val full = total >= 16
    buffer := full ? (merged |<< 16) | merged
    fill := full ? (total - 16) | total
    val since = Reg(UInt(log2Up(tail + 1) bits)) init (tail) // zero words since the last other one
    val some = merged(25 downto 10) =/= 0
    when(full)(since := some ? U(0) | (since + (since =/= tail).asUInt))
    val toDump = StreamFifoCC(Bits(16 bits), 16, usCd, outCd)
    toDump.io.push.valid := full && (some || since =/= tail)
    toDump.io.push.payload := merged(25 downto 10)
    val dropped = toDump.io.push.valid && !toDump.io.push.ready
  }
  // read once 4 words wait, to the last: the clocks' difference over a burst is under a word
  val usOut = new ClockingArea(outCd) {
    val pop = upstream.toDump.io.pop
    val started = Reg(Bool()) init (False)
    when(!started && upstream.toDump.io.popOccupancy >= 4)(started := True)
    when(started && !pop.valid)(started := False)
    pop.ready := started
    dump.io.usWord := RegNext((started && pop.valid) ? pop.payload | B(0, 16 bits)) init (0)
    dump.io.usDropped := PulseCCByToggle(upstream.dropped, usCd, outCd)
  }
}
