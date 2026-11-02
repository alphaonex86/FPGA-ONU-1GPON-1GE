// SPDX-License-Identifier: CERN-OHL-S-2.0
package vendor

import spinal.core._
import spinal.lib._

/** Spinal's Verilog RAMs read the old word during a write, which Quartus never maps to MLABs. RAMs that never read a
  * word as it is written (single-clock StreamFifo: empty or full then; Mems tagged pon.ReadDuringWriteFree) get
  * no_rw_check, and the small ones (Quartus keeps them in M20Ks otherwise) MLAB. */
object Quartus {
  def ramStyles(top: Component): Unit = top.walkComponents { c =>
    c.dslBody.walkDeclarations {
      case m: Mem[_] if c.isInstanceOf[StreamFifo[_]] || m.hasTag(pon.ReadDuringWriteFree) =>
        m.addAttribute("ramstyle", if (m.wordCount <= 128) "MLAB, no_rw_check" else "no_rw_check")
      case _ =>
    }
  }
}
