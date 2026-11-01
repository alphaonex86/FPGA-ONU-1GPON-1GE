// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon

import spinal.core._
import spinal.core.sim._
import scala.collection.mutable
import pon.gpon.GtcDsTest.check

/** Aes128 against FIPS-197 appendix C.1 and the JVM's AES on random keys and blocks, fed as fast as it takes them. */
object Aes128Test extends App {
  def model(key: BigInt, block: BigInt): BigInt = {
    def bytes(v: BigInt) = (15 to 0 by -1).map(i => ((v >> (8 * i)) & 0xFF).toByte).toArray
    val cipher = javax.crypto.Cipher.getInstance("AES/ECB/NoPadding")
    cipher.init(javax.crypto.Cipher.ENCRYPT_MODE, new javax.crypto.spec.SecretKeySpec(bytes(key), "AES"))
    BigInt(1, cipher.doFinal(bytes(block)))
  }
  val fipsKey = BigInt("000102030405060708090a0b0c0d0e0f", 16)
  val fipsPlain = BigInt("00112233445566778899aabbccddeeff", 16)
  check(model(fipsKey, fipsPlain) == BigInt("69c4e0d86a7b0430d8cdb78070b4c55a", 16), "JVM AES model on FIPS-197 C.1")

  SimConfig.compile(Aes128()).doSim(seed = 3) { dut =>
    val cd = dut.clockDomain
    val rng = new scala.util.Random(5)
    val inputs = Seq((fipsKey, fipsPlain)) ++ Seq.fill(200)((BigInt(128, rng), BigInt(128, rng)))
    val expected = mutable.Queue(inputs.map { case (k, b) => model(k, b) }: _*)
    var index, results, wrong, first, lastOut, firstIn = 0
    var clocks = 0
    dut.io.input.valid #= false
    cd.onSamplings {
      clocks += 1
      if (dut.io.input.valid.toBoolean && dut.io.input.ready.toBoolean) { if (index == 0) firstIn = clocks; index += 1 }
      if (dut.io.output.valid.toBoolean) {
        if (dut.io.output.payload.toBigInt != expected.dequeue()) wrong += 1
        if (results == 0) first = clocks
        results += 1
        lastOut = clocks
      }
      dut.io.input.valid #= index < inputs.length
      if (index < inputs.length) { dut.io.input.key #= inputs(index)._1; dut.io.input.block #= inputs(index)._2 }
    }
    cd.forkStimulus(10)
    cd.waitSamplingWhere(results == inputs.length || clocks > 10000)
    check(results == inputs.length && wrong == 0, s"all ${inputs.length} blocks right, in order")
    check(first - firstIn == Aes128.latency, s"latency ${first - firstIn} clocks")
    val perBlock = (lastOut - first).toDouble / (results - 1)
    check(perBlock <= 10, f"throughput: one block per $perBlock%.1f clocks (two in flight)")
  }
  println("PASS Aes128Test")
}
