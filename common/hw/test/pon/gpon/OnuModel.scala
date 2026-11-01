// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import scala.collection.mutable
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
  * Once active, `queues` (T-CONT Alloc-ID -> bytes waiting) are served on their grants with mode 0 DBRu reports
  * (taken before the grant is served); contiguous allocations form one burst. `rtdBits` is its whole round trip,
  * response time included. Test only. */
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
  val queues = mutable.Map[Int, Int]()
  val sent = mutable.Map[Int, Long]().withDefaultValue(0L)

  /** One downstream frame starting at upstream byte time `t`: its BWmap and PLOAM; `now` is the line time. */
  def frame(t: Int, bwmap: Seq[Alloc], ploam: Option[Seq[Int]], now: Int): Unit = {
    val snMsg = (r: Int) => Seq(onuId, 0x01) ++ bigToBytes(sn, 8) ++ Seq(r >> 4, ((r & 0xF) << 4) | 0x04)
    for (a <- bwmap) {
      if (state == 3 && a.allocId == 254) {
        val r = rnd.nextInt(234)
        burst(t, Seq(a), preBits + r * 256, _ => withCrc(snMsg(r)), now)
      } else if (state == 4 && a.allocId == onuId && !mute) burst(t, Seq(a), preBits, _ => withCrc(snMsg(0)), now)
    }
    if (state == 5) {
      val mine = bwmap.filter(a => a.allocId == onuId || queues.contains(a.allocId))
      val runs = mine.foldLeft(List[List[Alloc]]()) {
        case (run :: done, a) if a.start == run.head.stop + 1 => (a :: run) :: done
        case (done, a) => List(a) :: done
      }
      for (run <- runs.reverse) burst(t, run.reverse, eqd.toInt, serve, now)
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

  def withCrc(field: Seq[Int]): Seq[Int] = field :+ Model.crc8(field)

  /** An active ONU's allocation: PLOAMu, DBRu, then its queue's bytes as payload (idle GEM frames here). */
  def serve(a: Alloc): Seq[Int] = {
    val ploamU = if ((a.flags & 0x400) == 0) Nil else {
      val msg = Seq(onuId, if (gaspPending) 0x03 else 0x04) ++ Seq.fill(10)(0)
      gaspPending = false
      withCrc(msg)
    }
    val queue = queues.getOrElse(a.allocId, 0)
    val dbru = if ((a.flags & 0x180) == 0x080) withCrc(Seq(OnuModel.dbruCode((queue + 47) / 48))) else Nil
    val room = a.stop - a.start + 1 - ploamU.length - dbru.length
    if (queues.contains(a.allocId)) {
      val n = queue min room
      queues(a.allocId) = queue - n
      sent(a.allocId) += n
    }
    ploamU ++ dbru ++ Seq.fill(room / 5)(bigToBytes(gemMask, 5)).flatten ++ Seq.fill(room % 5)(0)
  }

  /** PLOu then the allocations' contents, `delay` bits after the nominal place of the first. */
  def burst(t: Int, allocs: Seq[Alloc], delay: Int, content: Alloc => Seq[Int], now: Int): Unit = {
    val clear = Seq(bip, onuId, 0) ++ allocs.flatMap(content)
    val bipFirst = (t + allocs.head.start - 3) * 8 + rtdBits + delay
    if (bipFirst - 11 * 8 <= now * 8) throw new Exception(s"FAIL: $name: burst placed in the past")
    line.put(bipFirst - 11 * 8, Seq.fill(8)(0xAA))
    line.put(bipFirst - 3 * 8, bigToBytes(usDelimiter, 3))
    line.put(bipFirst, clear.zip(Model.keystream(clear.length)).map { case (c, k) => c ^ k })
    bip = clear.drop(1).reduce(_ ^ _)
  }
}

object OnuModel {
  /** Reporting blocks to a mode 0 DBRu code (G.984.3 Table 8-1, ONU side). */
  def dbruCode(blocks: Int): Int =
    if (blocks < 128) blocks
    else if (blocks > 8191) 0xFE
    else {
      val n = 32 - Integer.numberOfLeadingZeros(blocks) - 7 // leading ones of the code
      ((0xFF << (8 - n)) & 0xFF) | ((blocks >> (2 * n - 1)) & ((1 << (7 - n)) - 1))
    }

  /** Code to blocks as the OLT reads it (Table 8-1, OLT side). */
  def dbruBlocks(code: Int): Int = {
    val n = (7 to 0 by -1).takeWhile(k => (code >> k & 1) == 1).length
    if (n == 0) code else ((1 << (6 + n)) | ((code & ((1 << (7 - n)) - 1)) << (2 * n - 1)) | ((1 << (2 * n - 1)) - 1))
  }
}
