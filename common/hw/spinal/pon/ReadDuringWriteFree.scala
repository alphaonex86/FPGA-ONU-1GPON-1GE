// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._

/** Tag of a Mem never read at the clock its read word is written: the vendor flow may drop the read-during-write
  * behaviour (small RAM). */
object ReadDuringWriteFree extends SpinalTag
