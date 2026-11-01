// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._

/** Verilog of every top into out/rtl/<Top>.v */
object Generate extends App {
  val config = SpinalConfig(targetDirectory = "out/rtl")
  config.generateVerilog(gpon.GtcDsFramer())
  config.generateVerilog(gpon.GtcDsDeframer())
  config.generateVerilog(gpon.GtcUsDeframer())
  config.generateVerilog(gpon.OltScheduler())
  config.generateVerilog(gpon.OltMac())
}
