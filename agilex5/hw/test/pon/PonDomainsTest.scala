// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import pon.gpon.GtcDsTest.check

case class ToUsDut() extends Component {
  val io = new Bundle {
    val dsClk, usClk, down = in Bool ()
    val input = slave Stream (Fragment(UInt(8 bits)))
    val output = master Stream (Fragment(UInt(8 bits)))
  }
  io.output << PonDomains(io.dsClk, io.usClk, io.down).toUs(io.input)
}

/** PonDomains.toUs: downstream packets without gaps (bubbles between them) into the upstream domain; every byte once,
  * in order, under a random upstream ready; with ready held high, a packet stays without gaps. */
object PonDomainsTest extends App {
  SimConfig.compile(ToUsDut()).doSim(seed = 3) { dut =>
    val io = dut.io
    io.dsClk #= false
    io.usClk #= false
    io.down #= true
    io.input.valid #= false
    io.output.ready #= false
    fork {
      var n = 0
      while (true) { sleep(2); io.dsClk #= !io.dsClk.toBoolean; if (n % 2 == 0) io.usClk #= !io.usClk.toBoolean; n += 1 }
    }
    def dsEdge(): Unit = { waitUntil(!io.dsClk.toBoolean); waitUntil(io.dsClk.toBoolean) }
    def usEdge(): Unit = { waitUntil(!io.usClk.toBoolean); waitUntil(io.usClk.toBoolean) }
    for (_ <- 0 until 10) dsEdge()
    io.down #= false
    for (_ <- 0 until 10) dsEdge()

    val packets = Seq.tabulate(40)(p => Seq.tabulate(1 + p * 7 % 23)(i => (p * 31 + i) & 0xFF))
    val sent = packets.flatten
    var received = Seq[Int]()
    var gaps = 0
    var randomReady = true
    fork { // a whole packet valid every downstream clock, a bubble after it
      for (p <- packets; (b, i) <- p.zipWithIndex) {
        io.input.valid #= true
        io.input.fragment #= b
        io.input.last #= i == p.length - 1
        do dsEdge() while (!io.input.ready.toBoolean)
        if (i == p.length - 1) { io.input.valid #= false; dsEdge() }
      }
      io.input.valid #= false
    }
    fork {
      var inPacket = false
      while (true) {
        val ready = !randomReady || scala.util.Random.nextBoolean()
        io.output.ready #= ready
        usEdge()
        if (io.output.valid.toBoolean && ready) {
          received :+= io.output.fragment.toInt
          inPacket = !io.output.last.toBoolean
        } else if (inPacket && !randomReady && !io.output.valid.toBoolean) gaps += 1
      }
    }
    while (received.length < sent.length / 2) usEdge()
    randomReady = false
    for (_ <- 0 until 2000 if received.length < sent.length) usEdge()
    for (_ <- 0 until 20) usEdge()
    check(received == sent, s"every byte once, in order (${received.length} of ${sent.length})")
    check(gaps == 0, "ready held high: no gap inside a packet")
  }
  println("PASS PonDomainsTest")
}
