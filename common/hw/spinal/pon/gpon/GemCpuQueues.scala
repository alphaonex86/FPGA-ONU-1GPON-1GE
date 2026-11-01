// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif._

/** GEM frames between the CPU and a MAC (OMCI and every other CPU port), APB3 in the CPU clock domain. TX (OLT:
  * downstream, ONU: upstream): write TX_HEAD (port, length) then the data words, first byte in bits 31-24; a head is
  * refused unless the previous packet is complete and the whole packet fits; a packet goes to the framer (txDomain)
  * only once all its words are in, then one byte per clock. RX (rxDomain): each GEM fragment as its data words and a
  * descriptor; RX_DESC_0.ready tells a whole fragment waits; reading RX_DATA pops its words, the last one (or reading
  * RX_DESC_1 of a fragment without words) pops the descriptor. Software reassembles fragments (RX_DESC_1.end).
  * ERRORS bits are sticky, cleared by writing 1. */
case class GemCpuQueues(txDomain: ClockDomain, rxDomain: ClockDomain, txWords: Int = 512, rxWords: Int = 1024,
                        rxFragments: Int = 64) extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(Apb3Config(addressWidth = 8, dataWidth = 32)))
    val tx = master Stream (Fragment(GemTxByte())) // txDomain
    val rx = slave Flow (Fragment(UsGemByte()))    // rxDomain
    val rxCut = in Bool ()                         // rxDomain, with io.rx: the open fragment was cut
    val irq = out Bool ()
  }
  def wordsOf(bytes: UInt): UInt = ((bytes.resize(13) + 3) >> 2).resize(11)

  case class RxDesc() extends Bundle {
    val allocId, portId, bytes = UInt(12 bits)
    val words = UInt(11 bits)
    val end, cut, truncated = Bool()
  }
  val txFifo = StreamFifoCC(Bits(32 bits), txWords, ClockDomain.current, txDomain)
  val rxData = StreamFifoCC(Bits(32 bits), rxWords, rxDomain, ClockDomain.current)
  val rxDescs = StreamFifoCC(RxDesc(), rxFragments, rxDomain, ClockDomain.current)

  val sender = new ClockingArea(txDomain) {
    val pop = txFifo.io.pop
    val stage = Reg(Bits(32 bits)) // the next word, so the queue read enable only sees registers (311 MHz)
    val staged = RegInit(False)
    pop.ready := !staged
    when(pop.fire) {
      stage := pop.payload
      staged := True
    }
    val loading, sending = RegInit(False)
    val portId = Reg(UInt(12 bits))
    val length, left = Reg(UInt(12 bits))
    val word = Reg(Bits(32 bits))
    val byteIndex = Reg(UInt(2 bits))
    val headLength = stage(27 downto 16).asUInt
    val enough = RegNext(staged && txFifo.io.popOccupancy >= wordsOf(headLength)) init (False) // the staged head's words are in
    val start = !loading && !sending && staged && enough
    when(start) {
      staged := False
      loading := True
      portId := stage(11 downto 0).asUInt
      length := headLength
      left := headLength
    }
    when(loading && staged) {
      word := stage
      staged := False
      loading := False
      sending := True
      byteIndex := 0
    }
    io.tx.valid := sending
    io.tx.last := left === 1
    io.tx.portId := portId
    io.tx.length := length.resized
    io.tx.data := word.subdivideIn(8 bits).reverse(byteIndex)
    when(io.tx.fire) {
      byteIndex := byteIndex + 1
      left := left - 1
      when(left === 1) {
        sending := False
      } elsewhen (byteIndex === 3) { // staged 3 clocks ago at the latest: the packet is all in the queue
        word := stage
        staged := False
      }
    }
  }

  val receiver = new ClockingArea(rxDomain) {
    val open, dropping = RegInit(False)
    val desc = Reg(RxDesc()) // the open fragment
    val word = Reg(Bits(32 bits))
    val byteIndex = Reg(UInt(2 bits)) init (0)
    val descRoom = rxDescs.io.push.ready
    val starting = io.rx.valid && !open
    val skip = starting ? !descRoom | dropping // no descriptor room: the whole fragment is dropped
    val cur = CombInit(desc)
    when(starting) {
      cur.allocId := io.rx.allocId
      cur.portId := io.rx.gem.portId
      cur.bytes := 0
      cur.words := 0
      cur.truncated := False
    }
    val last = io.rx.valid && io.rx.last
    val ends = last || io.rxCut && (open || starting)
    val take = io.rx.valid && !skip
    val filled = word(23 downto 0) ## io.rx.gem.data
    val flush = take && (byteIndex === 3 || ends) || !take && ends && !skip && byteIndex =/= 0
    val shiftBytes = take ? (U(3, 3 bits) - byteIndex) | (U(4, 3 bits) - byteIndex)
    rxData.io.push.valid := flush
    rxData.io.push.payload := (take ? filled | word) |<< (shiftBytes << 3)

    val next = RxDesc()
    next.allocId := cur.allocId
    next.portId := cur.portId
    next.bytes := cur.bytes + take.asUInt
    next.words := cur.words + (flush && rxData.io.push.ready).asUInt
    next.truncated := cur.truncated || flush && !rxData.io.push.ready
    next.end := last && io.rx.gem.pti(0)
    next.cut := !last
    desc := next
    rxDescs.io.push.valid := ends && !skip
    rxDescs.io.push.payload := next

    when(take) {
      word := filled
      byteIndex := byteIndex + 1
    }
    when(flush || ends)(byteIndex := 0)
    when(starting) {
      open := True
      dropping := !descRoom
    }
    when(ends)(open := False)
    val dropped = starting && !descRoom
    val truncated = flush && !rxData.io.push.ready
  }

  val busif = BusInterface(io.apb, SizeMapping(0, 256))
  val TX_HEAD = busif.newReg("TX packet head: queues the packet, its data words follow in TX_DATA")
  TX_HEAD.field(UInt(12 bits), AccessType.WO, 0, "GEM Port-ID")(SymbolName("port"))
  TX_HEAD.reserved(4 bits)
  TX_HEAD.field(UInt(12 bits), AccessType.WO, 0, "length, bytes (1-4095)")(SymbolName("length"))
  val TX_DATA = busif.newReg("TX packet data word, first byte in bits 31-24")
  TX_DATA.field(Bits(32 bits), AccessType.WO, 0, "bytes")(SymbolName("data"))
  val txLeft = Reg(UInt(11 bits)) init (0)
  val headLength = busif.writeData(27 downto 16).asUInt
  val txFree = U(txWords, 11 bits) - txFifo.io.pushOccupancy.resize(11)
  val headOk = txLeft === 0 && headLength =/= 0 && txFree >= wordsOf(headLength) + 1
  val txHead = TX_HEAD.hitDoWrite && headOk
  val txData = TX_DATA.hitDoWrite && txLeft =/= 0
  txFifo.io.push.valid := txHead || txData
  txFifo.io.push.payload := busif.writeData
  when(txHead)(txLeft := wordsOf(headLength))
  when(txData)(txLeft := txLeft - 1)

  val descs = rxDescs.io.pop
  val taken = Reg(UInt(11 bits)) init (0)
  val wordsLeft = descs.words - taken
  val rxReady = descs.valid && rxData.io.popOccupancy >= wordsLeft
  val RX_DESC_0 = busif.newReg("RX fragment")
  RX_DESC_0.field(UInt(12 bits), AccessType.RO, 0, "GEM Port-ID")(SymbolName("port")) := descs.portId
  RX_DESC_0.reserved(4 bits)
  RX_DESC_0.field(UInt(12 bits), AccessType.RO, 0, "Alloc-ID")(SymbolName("alloc")) := descs.allocId
  RX_DESC_0.reserved(3 bits)
  RX_DESC_0.field(Bool(), AccessType.RO, 0, "ready: a whole fragment waits")(SymbolName("ready")) := rxReady
  val RX_DESC_1 = busif.newReg("RX fragment size and flags: reading it pops a fragment without data words")
  RX_DESC_1.field(UInt(12 bits), AccessType.RO, 0, "bytes")(SymbolName("bytes")) := descs.bytes
  RX_DESC_1.reserved(4 bits)
  RX_DESC_1.field(UInt(11 bits), AccessType.RO, 0, "data words to read")(SymbolName("words")) := descs.words
  RX_DESC_1.reserved(1 bits)
  RX_DESC_1.field(Bool(), AccessType.RO, 0, "end: last fragment of the user frame")(SymbolName("end")) := descs.end
  RX_DESC_1.field(Bool(), AccessType.RO, 0, "cut by the allocation end: user frame lost")(SymbolName("cut")) := descs.cut
  RX_DESC_1.field(Bool(), AccessType.RO, 0, "truncated: RX data queue full")(SymbolName("truncated")) := descs.truncated
  val RX_DATA = busif.newReg("RX data word, first byte in bits 31-24: reading it pops the word")
  RX_DATA.field(Bits(32 bits), AccessType.RO, 0, "bytes")(SymbolName("data")) := rxData.io.pop.payload
  val rxPop = RX_DATA.hitDoRead && rxReady && wordsLeft =/= 0 && rxData.io.pop.valid
  rxData.io.pop.ready := rxPop
  when(rxPop)(taken := taken + 1)
  descs.ready := rxPop && wordsLeft === 1 || RX_DESC_1.hitDoRead && rxReady && descs.words === 0
  when(descs.ready)(taken := 0)

  val STATUS = busif.newReg("Queues")
  STATUS.field(UInt(11 bits), AccessType.RO, 0, "txFree: TX queue free words")(SymbolName("txFree")) := txFree
  STATUS.reserved(5 bits)
  STATUS.field(Bool(), AccessType.RO, 0, "txBusy: TX_DATA words still expected")(SymbolName("txBusy")) := txLeft =/= 0

  def sticky(hit: Bool, clear: Bool): Bool = {
    val bit = RegInit(False)
    when(clear)(bit := False)
    when(hit)(bit := True)
    bit
  }
  val ERRORS = busif.newReg("Sticky errors, write 1 to clear")
  val clearErrors = ERRORS.hitDoWrite ? busif.writeData | B(0, 32 bits)
  val errors = Seq(
    "txRefused" -> (TX_HEAD.hitDoWrite && !headOk, "TX_HEAD refused: packet incomplete, length 0 or no room"),
    "txExtra" -> (TX_DATA.hitDoWrite && txLeft === 0, "TX_DATA beyond the packet length"),
    "rxUnderflow" -> (RX_DATA.hitDoRead && !rxPop, "RX_DATA read without a ready word"),
    "rxDropped" -> (PulseCCByToggle(receiver.dropped, rxDomain, ClockDomain.current), "fragment dropped, descriptor queue full"),
    "rxTruncated" -> (PulseCCByToggle(receiver.truncated, rxDomain, ClockDomain.current), "fragment truncated, data queue full"))
  val errorBits = for (((name, (hit, doc)), k) <- errors.zipWithIndex) yield {
    val bit = sticky(hit, clearErrors(k))
    ERRORS.field(Bool(), AccessType.RO, 0, s"$name: $doc")(SymbolName(name)) := bit
    bit
  }
  val IRQ_ENABLE = busif.newReg("Interrupt sources")
  val irqRx = IRQ_ENABLE.field(Bool(), AccessType.RW, 0, "a whole RX fragment waits")(SymbolName("rx"))
  val irqError = IRQ_ENABLE.field(Bool(), AccessType.RW, 0, "any ERRORS bit")(SymbolName("error"))
  io.irq := RegNext(irqRx && rxReady || irqError && errorBits.reduce(_ || _)) init (False)

  busif.accept(DocCHeader("gem_queues", "GEMQ")) // gem_queues.h next to the Verilog
}
