# SPDX-License-Identifier: CERN-OHL-S-2.0
# qsys-script on the GHRD after its IP upgrade. PonBoard needs the logic and the M20K: the JTAG debug masters removed,
# with them the HPS FPGA-to-HPS and FPGA-to-SDRAM bridges they alone used (off: an HPS bridge without a master does
# not synthesize), the on-chip RAM shrunk. Then PonBoard's three lightweight windows (4 KB each, AXI4 like HpsBridge)
# exported as AXI4 masters behind the HPS lightweight bridge, on the 100 MHz clock.
package require -exact qsys 26.1
set hps_gone {f2sdram_clk f2sdram_rst fpga2hps_clk fpga2hps_rst fpga2hps h2f_warm_reset_handshake
  f2sdram_adapter_clk f2sdram_adapter_rst f2sdram_adapter_axi4_sub}

load_system hps_subsys/hps_subsys.qsys
remove_instance f2sdram_adapter
foreach i $hps_gone { remove_interface $i }
load_component agilex_hps
set_component_parameter_value f2s_data_width 0
set_component_parameter_value f2sdram_data_width 0
save_component
reload_component_footprint agilex_hps
validate_system
save_system ;# no path: a relative one resolves from the loaded system's folder

load_system qsys_top.qsys
foreach i {subsys_debug altera_ace5lite_cache_coherency_translator_0 ext_hps_f2sdram_master} { remove_instance $i }
foreach c [get_connections] {
  foreach end [split $c /] {
    if {[lsearch -exact $hps_gone [lindex [split $end .] 1]] >= 0 && [string match subsys_hps.* $end]} {
      remove_connection $c
    }
  }
}
foreach i [get_interfaces] {
  set of [split [get_interface_property $i EXPORT_OF] .]
  if {[lindex $of 0] == "subsys_hps" && [lsearch -exact $hps_gone [lindex $of 1]] >= 0} { remove_interface $i }
}
load_component ocm
set_component_parameter_value memorySize 4096 ;# 256 KB is 128 M20K; kept as hps2fpga test target
save_component
foreach i [get_instances] {
  if {[get_instance_property $i CLASS_NAME] == "altera_generic_component"} { reload_component_footprint $i }
}
foreach {name base} {lwOlt 0x00100000 lwOnu 0x00101000 lwEth 0x00102000} {
  add_instance $name altera_axi_bridge
  foreach {p v} {AXI_VERSION AXI4 ADDR_WIDTH 12 DATA_WIDTH 32 S0_ID_WIDTH 4 M0_ID_WIDTH 4} {
    set_instance_parameter_value $name $p $v
  }
  add_connection clk_100.out_clk $name.clk
  add_connection rst_in.out_reset $name.clk_reset
  add_connection subsys_hps.lwhps2fpga $name.s0
  set_connection_parameter_value subsys_hps.lwhps2fpga/$name.s0 baseAddress $base
  add_interface $name axi4 master
  set_interface_property $name EXPORT_OF $name.m0
}
validate_system
save_system
