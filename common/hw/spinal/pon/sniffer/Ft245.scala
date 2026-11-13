// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.sniffer

import spinal.core._
import spinal.lib._

/** One FT2232H channel in asynchronous 245 FIFO mode (a serial port on the host). A byte from the host first (only
  * while `rxReady`), else a byte to it. RD# and WR# are low `strobe` clocks (30 ns at least), the data is set a clock
  * before WR# and held a clock after, and RXF# / TXE# are read again `recover` clocks after a strobe (they answer
  * within 14 ns, then two clocks of synchronizer). Defaults for 77.76 MHz: 10 clocks per byte, 7.8 MB/s. */
case class Ft245(strobe: Int = 3, recover: Int = 5) extends Component {
  val io = new Bundle {
    val tx = slave Stream (Bits(8 bits))
    val rx = master Flow (Bits(8 bits))
    val rxReady = in Bool ()
    val rxfN, txeN = in Bool ()
    val rdN, wrN = out Bool ()
    val read = in Bits (8 bits)
    val write = out Bits (8 bits)
    val drive = out Bool ()
  }
  object State extends SpinalEnum { val IDLE, READ, SETUP, WRITE, RECOVER = newElement() }
  val state = RegInit(State.IDLE)
  val count = Reg(UInt(log2Up(strobe max recover) bits))
  val rxf = !BufferCC(io.rxfN, True)
  val txe = !BufferCC(io.txeN, True)
  val rdN, wrN = RegInit(True)
  val drive = RegInit(False)
  val data = Reg(Bits(8 bits))
  val got = RegNext(False) init (False)
  io.rdN := rdN
  io.wrN := wrN
  io.drive := drive
  io.write := data
  io.rx.valid := got
  io.rx.payload := data
  io.tx.ready := False
  count := count + 1
  switch(state) {
    is(State.IDLE) {
      count := 0
      when(rxf && io.rxReady) {
        rdN := False
        state := State.READ
      } elsewhen (txe && io.tx.valid) {
        io.tx.ready := True
        data := io.tx.payload
        drive := True
        state := State.SETUP
      }
    }
    is(State.READ) {
      when(count === strobe - 1) {
        data := io.read
        got := True
        rdN := True
        count := 0
        state := State.RECOVER
      }
    }
    is(State.SETUP) {
      wrN := False
      count := 0
      state := State.WRITE
    }
    is(State.WRITE) {
      when(count === strobe - 1) {
        wrN := True
        count := 0
        state := State.RECOVER
      }
    }
    is(State.RECOVER) {
      drive := False
      when(count === recover - 1)(state := State.IDLE)
    }
  }
}
