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
  config.generateVerilog(gpon.OltMac()) // + olt_regs.h
  config.generateVerilog(gpon.OnuMac()) // + onu_regs.h
  config.generateVerilog(epon.EponOnuMac()) // + epon_onu_regs.h
  config.generateVerilog(epon.EponOltMac()) // + epon_olt_regs.h
  config.generateVerilog(sniffer.SnifferDs())
  config.generateVerilog(sniffer.SnifferUs())
  config.generateVerilog(sniffer.SnifferBridge()) // in sniffer/ecp5/sniffer_ecp5.v
  config.generateVerilog(sniffer.InlineSniffer()) // in ../FPGA-USB-GPON-sniffer/ecp5 (+ inline_regs.h)
}
