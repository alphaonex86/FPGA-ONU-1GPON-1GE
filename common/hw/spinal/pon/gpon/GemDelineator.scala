// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.lib._
import GponCodes._

object GemState extends SpinalEnum { val HEADER, DATA, HUNT, SKIP = newElement() } // SKIP: pre-sync

/** A fragment with data starts: its header, and the crypto-counter at the header's first byte. */
case class GemHeaderEvent() extends Bundle {
  val portId, pli = UInt(12 bits)
  val counter = UInt(46 bits)
}

/** GEM frame delineation (G.984.3 clause 8.3.2), one byte per clock, HEC detect only.
  * `restart`: the next byte starts a GTC payload section, so it is a GEM header. `header` fires one clock before
  * the fragment's first data byte.
  * `cut`: a fragment was cut by the section end, its user frame is lost. */
case class GemDelineator() extends Component {
  val io = new Bundle {
    val restart = in Bool ()
    val byte = slave Flow (Bits(8 bits))
    val counter = in UInt (46 bits) // crypto-counter of `byte`
    val header = master Flow (GemHeaderEvent())
    val gem = master Flow (Fragment(GemRxByte())) // last: end of the GEM fragment
    val hecError = out Bool ()
    val cut = out Bool ()
  }

  // HEADER/DATA in sync, HUNT and SKIP (pre-sync) after a bad header
  val state = RegInit(GemState.HEADER)
  val headerCount = Reg(UInt(3 bits)) init (0)
  val left = Reg(UInt(12 bits)) // fragment bytes to deliver (DATA) or to skip (SKIP)
  val portId = Reg(UInt(12 bits))
  val pti = Reg(Bits(3 bits))
  val shift = Reg(Bits(32 bits))
  when(io.byte.valid)(shift := (shift ## io.byte.payload)(31 downto 0))
  val header = (shift ## io.byte.payload) ^ B(gemMask, 40 bits)
  val headerOk = gemHeaderValid(header)
  val pli = header(39 downto 28).asUInt
  val headerCounter = Reg(UInt(46 bits))

  def nextHeader(): Unit = { state := GemState.HEADER; headerCount := 0 }

  io.gem.valid := False
  io.gem.portId := portId
  io.gem.pti := pti
  io.gem.data := io.byte.payload
  io.gem.last := left === 1
  io.hecError := False
  io.header.valid := False
  io.header.portId := header(27 downto 16).asUInt
  io.header.pli := pli
  io.header.counter := headerCounter

  when(io.byte.valid) {
    switch(state) {
      is(GemState.HEADER) {
        headerCount := headerCount + 1
        when(headerCount === 0)(headerCounter := io.counter)
        when(headerCount === 4) {
          when(headerOk) {
            portId := header(27 downto 16).asUInt
            pti := header(15 downto 13)
            left := pli
            io.header.valid := pli =/= 0
            when(pli === 0)(nextHeader()) otherwise (state := GemState.DATA)
          } otherwise {
            io.hecError := True
            state := GemState.HUNT
          }
        }
      }
      is(GemState.DATA) {
        io.gem.valid := True
        left := left - 1
        when(left === 1)(nextHeader())
      }
      is(GemState.HUNT) {
        when(headerOk) {
          left := pli
          when(pli === 0)(nextHeader()) otherwise (state := GemState.SKIP)
        }
      }
      is(GemState.SKIP) {
        left := left - 1
        when(left === 1)(nextHeader())
      }
    }
  }

  io.cut := io.restart && state === GemState.DATA && !(io.byte.valid && left === 1)
  when(io.restart)(nextHeader())
}
