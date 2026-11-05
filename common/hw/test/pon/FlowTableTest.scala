// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.core.sim._
import scala.collection.mutable
import pon.gpon.GtcDsTest.check

/** FlowTable against a model: the CPU writes random flows in their hashed bucket (both ways, overwrites, an entry
  * invalidated), lookups of written and unknown keys, one at a time, answer hit and action 6 clocks later; IPv4 and IPv6
  * keys. */
object FlowTableTest extends App {
  case class Key(dport: Int, sport: Int, dst: BigInt, src: BigInt, udp: Boolean, session: Int, pppoe: Boolean, vid: Int,
                 tagged: Boolean, addressBits: Int = 32) {
    def bits: BigInt = Seq[(BigInt, Int)]((dport, 16), (sport, 16), (dst, addressBits), (src, addressBits),
      (if (udp) 1 else 0, 1), (session, 16), (if (pppoe) 1 else 0, 1), (vid, 12), (if (tagged) 1 else 0, 1))
      .foldRight(BigInt(0)) { case ((v, w), acc) => (acc << w) | v }
    def bucket(bucketBits: Int): Int = FlowTable.model(bits, 63 + 2 * addressBits) & ((1 << bucketBits) - 1)
  }
  case class Action(natSrc: Boolean, ip: Long, port: Int, egress: Int)
  val rnd = new scala.util.Random(5)
  def key(addressBits: Int): Key = {
    val pppoe = rnd.nextBoolean(); val tagged = rnd.nextBoolean()
    Key(rnd.nextInt(65536), rnd.nextInt(65536), BigInt(addressBits, rnd), BigInt(addressBits, rnd), rnd.nextBoolean(),
      if (pppoe) rnd.nextInt(65536) else 0, pppoe, if (tagged) rnd.nextInt(4096) else 0, tagged, addressBits)
  }
  def action() = Action(rnd.nextBoolean(), rnd.nextLong() & 0xFFFFFFFFL, rnd.nextInt(65536), rnd.nextInt(64))
  def drive(k: FlowKey, v: Key): Unit = {
    k.dport #= v.dport; k.sport #= v.sport; k.dst #= v.dst; k.src #= v.src; k.udp #= v.udp
    k.session #= v.session; k.pppoe #= v.pppoe; k.vid #= v.vid; k.tagged #= v.tagged
  }
  val bucketBits = 6 // small: many collisions
  def bucketOf(k: Key) = k.bucket(bucketBits)

  for (addressBits <- Seq(32, 128)) SimConfig.compile(FlowTable(bucketBits, addressBits)).doSim { dut =>
    def key(): Key = FlowTableTest.key(addressBits)
    val io = dut.io
    val cd = dut.clockDomain
    cd.forkStimulus(10)
    io.lookup.valid #= false
    io.write.valid #= false
    cd.waitSampling(3)
    val table = mutable.Map[(Int, Int), (Key, Action)]() // (bucket, way)
    def write(bucket: Int, way: Int, k: Key, a: Action, valid: Boolean): Unit = {
      val w = io.write.payload
      w.bucket #= bucket; w.way #= way; w.entry.valid #= valid; drive(w.entry.key, k)
      w.entry.action.natSrc #= a.natSrc; w.entry.action.ip #= a.ip; w.entry.action.port #= a.port
      w.entry.action.egress #= a.egress
      io.write.valid #= true
      cd.waitSampling()
      io.write.valid #= false
      if (valid) table((bucket, way)) = (k, a) else table.remove((bucket, way))
    }
    val keys = Seq.fill(200)(key())
    for (k <- keys) write(bucketOf(k), rnd.nextInt(2), k, action(), valid = true) // collisions overwrite
    for (_ <- 0 until 10) { val (b, w) = table.keys.toSeq(rnd.nextInt(table.size)); write(b, w, key(), action(), false) }

    val expected = mutable.Queue[Option[Action]]()
    var wrong = mutable.ArrayBuffer[String]()
    var hits = 0
    cd.onSamplings {
      if (io.result.valid.toBoolean) {
        val e = expected.dequeue()
        val r = io.result.payload
        val got = if (r.hit.toBoolean) Some(Action(r.action.natSrc.toBoolean, r.action.ip.toLong, r.action.port.toInt,
          r.action.egress.toInt)) else None
        if (got.isDefined) hits += 1
        if (got != e) wrong += s"expected $e got $got"
      }
    }
    val probes = Seq.fill(600)(if (rnd.nextInt(4) == 0) key() else keys(rnd.nextInt(keys.length)))
    for (k <- probes) {
      drive(io.lookup.payload, k)
      io.lookup.valid #= rnd.nextInt(4) != 0
      cd.waitSampling()
      if (io.lookup.valid.toBoolean) {
        expected += (0 until 2).flatMap(w => table.get((bucketOf(k), w))).collectFirst { case (kk, a) if kk == k => a }
        io.lookup.valid #= false
        cd.waitSampling(5) // one lookup at a time
      }
    }
    io.lookup.valid #= false
    cd.waitSampling(8)
    check(expected.isEmpty && wrong.isEmpty && hits > 100, s"IPv${if (addressBits == 32) 4 else 6} lookups: $hits hits, " +
      s"misses and actions as the model ${wrong.take(2)}")
  }
  println("PASS FlowTableTest")
}
