// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba4.axi.Axi4

/** HPS lightweight bridge master for the board tests: single 32-bit accesses, returns the AXI response. */
class HpsAxi(axi: Axi4, cd: ClockDomain) {
  val SLVERR = 2
  axi.aw.valid #= false
  axi.w.valid #= false
  axi.ar.valid #= false
  axi.b.ready #= true
  axi.r.ready #= true

  def write(address: Int, data: Long): Int = {
    axi.aw.addr #= address
    axi.aw.id #= 1
    axi.aw.len #= 0
    axi.aw.size #= 2
    axi.aw.valid #= true
    axi.w.data #= data
    axi.w.strb #= 0xF
    axi.w.last #= true
    axi.w.valid #= true
    var (awDone, wDone) = (false, false)
    while (!(awDone && wDone)) {
      cd.waitSampling()
      if (axi.aw.valid.toBoolean && axi.aw.ready.toBoolean) { awDone = true; axi.aw.valid #= false }
      if (axi.w.valid.toBoolean && axi.w.ready.toBoolean) { wDone = true; axi.w.valid #= false }
    }
    cd.waitSamplingWhere(1000)(axi.b.valid.toBoolean)
    axi.b.resp.toInt
  }
  def read(address: Int): (Long, Int) = {
    axi.ar.addr #= address
    axi.ar.id #= 2
    axi.ar.len #= 0
    axi.ar.size #= 2
    axi.ar.valid #= true
    cd.waitSamplingWhere(1000)(axi.ar.ready.toBoolean)
    axi.ar.valid #= false
    cd.waitSamplingWhere(1000)(axi.r.valid.toBoolean)
    (axi.r.data.toLong, axi.r.resp.toInt)
  }
}
