// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.lib._
import java.io.File

/** taxi 1G MAC + 1000BASE-X/SGMII PCS (common/hw/ext/taxi, submodule) through common/hw/sv/eth_mac_1g_basex.sv. */
case class EthMac1gBasex(txCd: ClockDomain, rxCd: ClockDomain) extends BlackBox {
  setDefinitionName("eth_mac_1g_basex")
  val rx_clk, rx_rst, tx_clk, tx_rst = in Bool ()
  val tx_tdata = in Bits (8 bits)
  val tx_tvalid, tx_tlast, tx_tuser = in Bool ()
  val tx_tready = out Bool ()
  val rx_tdata = out Bits (8 bits)
  val rx_tvalid, rx_tlast, rx_tuser = out Bool ()
  val serdes_tx_data = out Bits (8 bits)
  val serdes_tx_data_k = out Bool ()
  val serdes_rx_data = in Bits (8 bits)
  val serdes_rx_data_k = in Bool ()
  val an_restart, an_speedup, an_sgmii_en = in Bool ()
  val an_complete, an_sgmii_mode, rx_status = out Bool ()
  val an_lp_adv_ability = out Bits (16 bits)
  val stat_rx_pkt_good, stat_rx_pkt_bad, stat_tx_pkt_good = out Bool ()
  mapClockDomain(txCd, tx_clk, tx_rst)
  mapClockDomain(rxCd, rx_clk, rx_rst)
  noIoPrefix()
  for (f <- EthMac1gBasex.sources) addRTLPath(f)
}

object EthMac1gBasex {
  val taxi = "common/hw/ext/taxi/src"
  /** The wrapper and taxi's file list of taxi_eth_mac_phy_1g_basex, resolved from the submodule. */
  def sources: Seq[String] = {
    def expand(list: File): Seq[File] = scala.io.Source.fromFile(list).getLines().map(_.trim).filter(_.nonEmpty).toSeq.flatMap { l =>
      val f = new File(list.getParentFile, l)
      if (l.endsWith(".f")) expand(f) else Seq(f)
    }
    val list = new File(s"$taxi/eth/rtl/taxi_eth_mac_phy_1g_basex.f")
    if (!list.exists) throw new Exception(s"$list missing: git submodule update --init")
    (Seq(new File("common/hw/sv/eth_mac_1g_basex.sv")) ++ expand(list)).map(_.getCanonicalPath).distinct
  }
}

/** Ethernet port on a raw 10-bit SERDES lane (1.25 Gb/s, bit 0 first): comma alignment and 8b/10b here, taxi's PCS
  * (clause 36, clause 37 / SGMII autonegotiation) and MAC. Frames as bytes, FCS added and checked by the MAC. */
case class EthPort(txCd: ClockDomain, rxCd: ClockDomain) extends Component {
  val io = new Bundle {
    val rawRx = in Bits (10 bits)  // rxCd
    val rawTx = out Bits (10 bits) // txCd
    val tx = slave Stream (Fragment(Bits(8 bits)))  // txCd
    val rx = master Flow (Fragment(Bits(8 bits)))   // rxCd
    val rxBad = out Bool ()                         // with rx.last: FCS or code error
    val sgmii = in Bool ()
    val anSpeedup = in Bool ()
    val anComplete, synced = out Bool ()
  }
  val mac = EthMac1gBasex(txCd, rxCd)
  val rxArea = new ClockingArea(rxCd) {
    val aligner = CommaAligner()
    aligner.io.raw := io.rawRx
    val decoder = Decoder8b10b()
    decoder.io.code := aligner.io.code
    mac.serdes_rx_data := decoder.io.output.data
    mac.serdes_rx_data_k := decoder.io.output.k
  }
  val txArea = new ClockingArea(txCd) {
    val encoder = Encoder8b10b()
    encoder.io.input.data := mac.serdes_tx_data
    encoder.io.input.k := mac.serdes_tx_data_k
    io.rawTx := encoder.io.code
  }
  mac.tx_tdata := io.tx.fragment
  mac.tx_tvalid := io.tx.valid
  mac.tx_tlast := io.tx.last
  mac.tx_tuser := False
  io.tx.ready := mac.tx_tready
  io.rx.valid := mac.rx_tvalid
  io.rx.fragment := mac.rx_tdata
  io.rx.last := mac.rx_tlast
  io.rxBad := mac.rx_tuser
  mac.an_restart := False
  mac.an_speedup := io.anSpeedup
  mac.an_sgmii_en := io.sgmii
  io.anComplete := mac.an_complete
  io.synced := mac.rx_status
}
