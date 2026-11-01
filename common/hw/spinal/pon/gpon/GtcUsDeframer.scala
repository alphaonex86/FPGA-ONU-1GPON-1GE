// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.lib._
import GponCodes._

case class UsGrant() extends Bundle {
  val alloc = Allocation()
  val time = UInt(24 bits)   // OLT byte time of alloc.start
  val window = UInt(16 bits) // delimiter search, +- bytes around its expected place (wide when ranging)
  def bytes: UInt = alloc.stop - alloc.start + 1
}

case class UsBurst() extends Bundle {
  val found = Bool()            // false: no delimiter inside the window, the burst is lost
  val time = UInt(24 bits)      // grant time of its first allocation: which grant this report answers
  val allocId = UInt(12 bits)   // first allocation of the burst
  val onuId = Bits(8 bits)
  val ind = Bits(8 bits)
  val delayBits = SInt(28 bits) // arrival after the expected place
  val bipErrors = UInt(4 bits)  // against the BIP kept for onuId (meaningless on its first burst)
}

case class UsDbru() extends Bundle {
  val allocId = UInt(12 bits)
  val report = Bits(16 bits) // mode 0: 8-bit report in the low byte
  val crcOk = Bool()
}

case class UsGemByte() extends Bundle {
  val allocId = UInt(12 bits)
  val gem = GemRxByte()
}

object UsState extends SpinalEnum { val IDLE, HUNT, HEADER, ALLOC, DROP = newElement() }

/** OLT upstream GTC burst receiver (G.984.3 clause 8.2), no FEC, one line byte per clock (155.52 MHz).
  * Grants come in time order before their window opens; contiguous grants form one burst. The
  * delimiter is found on any bit alignment with up to `delimiterErrors` wrong bits. CRC/HEC detect only. */
case class GtcUsDeframer() extends Component {
  val io = new Bundle {
    val line = in Bits (8 bits) // raw line bytes, any bit alignment, MSB first
    val time = in UInt (24 bits) // OLT byte time of `line`, +1 per clock
    val grant = slave Stream (UsGrant())
    val delimiter = in Bits (24 bits)
    val delimiterErrors = in UInt (2 bits)
    val burst = master Flow (UsBurst())
    val ploam = master Flow (Checked(Bits(96 bits)))
    val dbru = master Flow (UsDbru())
    val gem = master Flow (Fragment(UsGemByte()))
    val hecErrors = out UInt (32 bits)
    val gemCuts = out UInt (32 bits) // GEM fragments cut by the allocation end
  }

  // delimiter search, registered: the burst is processed from `history`, one byte behind the line
  val history = Reg(Bits(32 bits)) init (0)
  val window = history(23 downto 0) ## io.line
  history := window
  val found = RegNext(Vec((0 until 8).map(o => CountOne(window(o + 23 downto o) ^ io.delimiter) <= io.delimiterErrors)))
  val alignment = Reg(UInt(3 bits))
  val byte = (history >> alignment)(7 downto 0)
  val now = RegNext(io.time) // time of the line byte in `found`

  val scramblerReset = RegNext(False) init (False)
  val scrambler = Reg(Bits(7 bits))
  val (keystream, scramblerNext) = scramblerByte(scramblerReset ? B"7'x7F" | scrambler)
  scrambler := scramblerNext
  val clear = byte ^ keystream

  def after(a: UInt, b: UInt): Bool = !(a - b).msb // a at or after b on the wrapping time base
  val head = io.grant.payload
  val expected = head.time - 4 // last delimiter byte, just before BIP, ONU-ID, Ind
  val huntStart = expected - head.window
  val huntEnd = expected + head.window
  io.grant.ready := False

  val state = RegInit(UsState.IDLE)
  val cur = Reg(UsGrant()) // allocation being received
  val contiguous = io.grant.valid && head.time === cur.time + cur.bytes

  val headerIndex = Reg(UInt(2 bits))
  val bipRx = Reg(Bits(8 bits))
  val onuId = Reg(Bits(8 bits))
  val bipRunning = Reg(Bits(8 bits)) // since the BIP field of this burst
  val bipStore = Mem(Bits(8 bits), 256) // per ONU, from its last burst
  val bipStored = bipStore.readSync(clear.asUInt, state === UsState.HEADER && headerIndex === 1)

  val allocLeft = Reg(UInt(16 bits))
  val ploamLeft = Reg(UInt(4 bits))
  val dbruLeft = Reg(UInt(2 bits))
  val dbruMode1 = Reg(Bool())
  val fieldStart = Reg(Bool()) // next overhead byte opens a CRC field
  val crc = Reg(Bits(8 bits))
  val crcIn = fieldStart ? B(0, 8 bits) | crc
  val crcOk = crcIn === clear
  val field = Reg(Bits(96 bits)) // PLOAMu or DBRu bytes, newest in the low byte
  val payloadByte = state === UsState.ALLOC && ploamLeft === 0 && dbruLeft === 0

  def startAlloc(g: UsGrant): Unit = {
    cur := g
    allocLeft := g.bytes
    ploamLeft := g.alloc.flags(10) ? U(13, 4 bits) | U(0, 4 bits)
    dbruLeft := g.alloc.flags(8 downto 7).mux(1 -> U(2, 2 bits), 2 -> U(3, 2 bits), default -> U(0, 2 bits))
    dbruMode1 := g.alloc.flags(8 downto 7) === 2
    fieldStart := True
  }

  val burst = Flow(UsBurst())
  burst.valid := False
  burst.found := True
  burst.time := cur.time
  burst.allocId := cur.alloc.allocId
  burst.onuId := onuId
  burst.ind := clear
  burst.bipErrors := CountOne(bipStored ^ bipRx)
  val delayBits = Reg(SInt(28 bits))
  burst.delayBits := delayBits
  io.burst << burst.stage()

  val ploam = Flow(Checked(Bits(96 bits)))
  ploam.valid := False
  ploam.value := field
  ploam.crcOk := crcOk
  io.ploam << ploam.stage()

  val dbru = Flow(UsDbru())
  dbru.valid := False
  dbru.allocId := cur.alloc.allocId
  dbru.report := dbruMode1 ? field(15 downto 0) | (B(0, 8 bits) ## field(7 downto 0))
  dbru.crcOk := crcOk
  io.dbru << dbru.stage()

  switch(state) {
    is(UsState.IDLE) {
      when(io.grant.valid && after(now, huntStart))(state := UsState.HUNT)
    }
    is(UsState.HUNT) {
      when(found.orR) {
        val o = OHToUInt(OHMasking.first(found.asBits))
        alignment := o
        delayBits := ((now - expected).asSInt.resize(28 bits) |<< 3) - o.intoSInt.resize(28 bits)
        scramblerReset := True
        headerIndex := 0
        io.grant.ready := True
        cur := head
        state := UsState.HEADER
      } elsewhen (!after(huntEnd, now)) { // window closed: burst lost, with its contiguous grants
        burst.valid := True
        burst.found := False
        burst.time := head.time
        burst.allocId := head.alloc.allocId
        io.grant.ready := True
        cur := head
        state := UsState.DROP
      }
    }
    is(UsState.HEADER) {
      headerIndex := headerIndex + 1
      switch(headerIndex) {
        is(0)(bipRx := clear)
        is(1) { onuId := clear; bipRunning := clear }
        default {
          bipRunning := bipRunning ^ clear
          burst.valid := True
          startAlloc(cur)
          state := UsState.ALLOC
        }
      }
    }
    is(UsState.ALLOC) {
      bipRunning := bipRunning ^ clear
      allocLeft := allocLeft - 1
      when(!payloadByte) {
        fieldStart := False
        crc := crc8Step(crcIn, clear)
        field := (field ## clear)(95 downto 0)
      }
      when(ploamLeft =/= 0) {
        ploamLeft := ploamLeft - 1
        when(ploamLeft === 1) {
          ploam.valid := True
          fieldStart := True // a DBRu may follow
        }
      } elsewhen (dbruLeft =/= 0) {
        dbruLeft := dbruLeft - 1
        dbru.valid := dbruLeft === 1
      }
      when(allocLeft === 1) {
        when(contiguous) {
          io.grant.ready := True
          startAlloc(head)
        } otherwise {
          bipStore.write(onuId.asUInt, bipRunning ^ clear)
          state := UsState.IDLE
        }
      }
    }
    is(UsState.DROP) {
      when(contiguous) {
        io.grant.ready := True
        cur := head
      } otherwise (state := UsState.IDLE)
    }
  }

  val delineator = GemDelineator()
  delineator.io.restart := !payloadByte || allocLeft === 1
  delineator.io.byte.valid := payloadByte
  delineator.io.byte.payload := clear
  val gem = Flow(Fragment(UsGemByte()))
  gem.valid := delineator.io.gem.valid
  gem.last := delineator.io.gem.last
  gem.allocId := cur.alloc.allocId
  gem.gem := delineator.io.gem.fragment
  io.gem << gem.stage()

  val hecError = RegNext(delineator.io.hecError) init (False)
  val hecErrors = Reg(UInt(32 bits)) init (0)
  io.hecErrors := hecErrors
  hecErrors := hecErrors + hecError.asUInt
  val gemCut = RegNext(delineator.io.cut) init (False)
  val gemCuts = Reg(UInt(32 bits)) init (0)
  io.gemCuts := gemCuts
  gemCuts := gemCuts + gemCut.asUInt
}
