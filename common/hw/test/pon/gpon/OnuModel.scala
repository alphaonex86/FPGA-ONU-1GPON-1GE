// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import GponCodes._
import GtcDsTest.{bigToBytes, bytesToBig}

/** Upstream line: laser off (0), each burst ORed in at its bit. Test only. */
class UsLine(totalBytes: Int) {
  val bits = new Array[Boolean](totalBytes * 8)
  def byte(t: Int): Int = (0 until 8).foldLeft(0)((a, k) => (a << 1) | (if (bits(t * 8 + k)) 1 else 0))
  def put(bit: Int, bytes: Seq[Int]): Unit =
    for ((b, i) <- bytes.zipWithIndex; k <- 0 until 8) bits(bit + 8 * i + k) |= ((b >> (7 - k)) & 1) == 1
}

case class Alloc(allocId: Int, flags: Int, start: Int, stop: Int)

/** ONU activation (G.984.3 states O2-O5, O7): answers SN and ranging requests, then sends its PLOAMu on each grant.
  * `rtdBits` is its whole round trip, response time included. Test only. */
class OnuModel(line: UsLine, val name: String, val sn: BigInt, val rtdBits: Int, seed: Int, mute: Boolean = false,
               gasp: Boolean = false) {
  var state = 2 // O2 standby: downstream in sync
  var onuId = 0xFF
  var preBits = 0
  var eqd = 0L
  var bip = 0
  var gaspPending = gasp
  var deactivated = 0
  val rnd = new scala.util.Random(seed)

  /** One downstream frame starting at upstream byte time `t`: its BWmap and PLOAM; `now` is the line time. */
  def frame(t: Int, bwmap: Seq[Alloc], ploam: Option[Seq[Int]], now: Int): Unit = {
    for (a <- bwmap) {
      val snMsg = (r: Int) => Seq(onuId, 0x01) ++ bigToBytes(sn, 8) ++ Seq(r >> 4, ((r & 0xF) << 4) | 0x04)
      if (state == 3 && a.allocId == 254) {
        val r = rnd.nextInt(234)
        burst(t, a, preBits + r * 256, snMsg(r), now)
      } else if (state == 4 && a.allocId == onuId && !mute) burst(t, a, preBits, snMsg(0), now)
      else if (state == 5 && a.allocId == onuId) {
        val msg = Seq(onuId, if (gaspPending) 0x03 else 0x04) ++ Seq.fill(10)(0)
        gaspPending = false
        burst(t, a, eqd.toInt, msg, now)
      }
    }
    for (m <- ploam) (m(1), m(0)) match {
      case (0x01, 0xFF) if state == 2 =>
        preBits = if ((m(9) >> 5 & 1) == 1) ((m(10) << 8) | m(11)) * 256 else 0
        state = 3
      case (0x03, 0xFF) if state == 3 && bytesToBig(m.slice(3, 11)) == sn => onuId = m(2); state = 4
      case (0x04, id) if id == onuId && state >= 4 => eqd = bytesToBig(m.slice(3, 7)).toLong; state = 5
      case (0x05, id) if (id == onuId || id == 0xFF) && state != 7 => onuId = 0xFF; state = 2; deactivated += 1
      case (0x06, 0xFF) if m(2) == 0xFF && bytesToBig(m.slice(3, 11)) == sn => state = 7 // emergency stop
      case _ =>
    }
  }

  /** PLOu, PLOAMu, then idle GEM frames to the end of the allocation, `delay` bits after its nominal place. */
  def burst(t: Int, a: Alloc, delay: Int, ploam: Seq[Int], now: Int): Unit = {
    val content = ploam :+ Model.crc8(ploam)
    val fill = (a.stop - a.start + 1 - content.length) / 5
    val clear = Seq(bip, onuId, 0) ++ content ++ Seq.fill(fill)(bigToBytes(gemMask, 5)).flatten
    val bipFirst = (t + a.start - 3) * 8 + rtdBits + delay
    if (bipFirst - 11 * 8 <= now * 8) throw new Exception(s"FAIL: $name: burst placed in the past")
    line.put(bipFirst - 11 * 8, Seq.fill(8)(0xAA))
    line.put(bipFirst - 3 * 8, bigToBytes(usDelimiter, 3))
    line.put(bipFirst, clear.zip(Model.keystream(clear.length)).map { case (c, k) => c ^ k })
    bip = clear.drop(1).reduce(_ ^ _)
  }
}
