// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.core.sim._
import GtcDsTest.check

/** DBRu reports against the G.984.3 model (blocks rounded up, Table 8-1 octet) for random backlogs and block sizes,
  * and the octet read back by our OLT (OltScheduler.dbruBlocks) within its code's range. */
object DbruReporterTest extends App {
  def decoded(code: Int): Int = { // Table 8-1, OLT side: the dropped bits read as ones
    val n = Integer.numberOfLeadingZeros(~code << 24) // leading ones
    if (code == 0xFF) 0 else if (n == 0) code else (1 << (6 + n)) | ((code & ((1 << (7 - n)) - 1)) << (2 * n - 1)) | ((1 << (2 * n - 1)) - 1)
  }
  val rnd = new scala.util.Random(5)
  SimConfig.compile(DbruReporter(2)).doSim { dut =>
    val io = dut.io
    val cd = dut.clockDomain
    cd.forkStimulus(10)
    var wrong = Seq[(Int, Int, Int)]()
    val cases = Seq((0, 48), (1, 48), (48, 48), (49, 48), (6095, 48), (6096, 48), (65535, 1), (65535, 48), (300, 32)) ++
      Seq.fill(200)((rnd.nextInt(65536), 1 + rnd.nextInt(if (rnd.nextBoolean()) 128 else 65535)))
    for (((bytes, block), k) <- cases.zipWithIndex) {
      val other = rnd.nextInt(65536)
      io.blockBytes #= block
      io.bytes(k % 2) #= bytes
      io.bytes(1 - k % 2) #= other
      cd.waitSampling(3 * 19 + 2) // a division begun before the change, then a whole round
      for ((b, s) <- Seq(bytes -> k % 2, other -> (1 - k % 2))) {
        val expected = DbruReporter.model((b + block - 1) / block)
        if (io.codes(s).toInt != expected) wrong :+= ((b, block, io.codes(s).toInt))
      }
    }
    check(wrong.isEmpty, s"${cases.length} backlogs x 2 streams: ceil(bytes / block) as the Table 8-1 octet ${wrong.take(4)}")
    val roundTrip = (0 to 9000).filterNot { b => val d = decoded(DbruReporter.model(b)); b <= d && (b > 8191 || d < 2 * b + 2) }
    check(roundTrip.isEmpty, s"OLT decoding of every code: at least the blocks reported, within the code's step ${roundTrip.take(4)}")
  }
  println("PASS DbruReporterTest")
}
