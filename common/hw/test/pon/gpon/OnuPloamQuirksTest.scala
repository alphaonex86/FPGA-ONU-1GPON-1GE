// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.core.sim._
import scala.collection.mutable
import GponCodes._
import GtcDsTest.{bigToBytes, bytesToBig, check}

/** OnuPloam against the downstream PLOAM sequences of real OLTs, as recorded by stock ONUs (ZTE at two ISPs) and read
  * out of an OLT firmware (Broadcom Maple, HSGQ G008): copies, order, foreign ONUs' messages, EqD drift updates,
  * deactivation churn with a new ONU-ID, messages left to the CPU. The ONU must answer exactly like the stock ONU
  * (every copy acknowledged, Password 3 times, nothing else upstream) and keep its bursts where the OLT expects them:
  * an OLT re-ranges past 4 bits of drift and deactivates past 8 (G.984.3 Table 10-2). One frame per call, compressed in
  * time (the gaps between messages carry no state but TO1/TO2). */
object OnuPloamQuirksTest extends App {
  val sn = BigInt("414C434C12345678", 16)
  val other = BigInt("48575443000000B2", 16)
  val password = BigInt("31323334353637383930", 16)
  val responseBits = 43546
  val ploamU = 1 << 10
  def ploam(bytes: Int*): BigInt = bytesToBig(bytes.padTo(12, 0))
  def withSn(m: BigInt, s: BigInt): BigInt = m | (s << 8) // SN in bytes 4-11
  def ack(id: Int, m: BigInt): BigInt = { val d = bigToBytes(m, 12); ploam(Seq(id, 0x09, d(1)) ++ d.slice(0, 9): _*) }
  def pw(id: Int): BigInt = (BigInt(id) << 88) | (BigInt(2) << 80) | password

  SimConfig.compile(OnuPloam()).doSim { dut =>
    val io = dut.io
    val cd = dut.clockDomain
    io.config.enable #= true
    io.config.sn #= sn
    io.config.password #= password
    io.config.responseBits #= responseBits
    io.config.to1Frames #= 1 << 19
    io.config.to2Frames #= 4000
    io.synced #= true
    io.frameStart #= false
    io.frameBit #= 0
    io.ploam.valid #= false
    io.alloc.valid #= false
    io.cpuPloam.valid #= false
    io.job.ready #= true
    io.usPloam.ready #= false
    cd.forkStimulus(10)
    cd.waitSampling(4)

    val upstream = mutable.ArrayBuffer[BigInt]()
    val toCpu = mutable.ArrayBuffer[BigInt]()
    val jobs = mutable.ArrayBuffer[(Int, Int, Long, Boolean)]() // frame, allocId, bit, sn
    var frame = 0
    var lost = false
    val keySwitches = mutable.ArrayBuffer[Long]()
    var keyRequests = 0
    cd.onSamplings {
      if (io.cpuRx.valid.toBoolean) toCpu += io.cpuRx.payload.toBigInt
      if (io.keySwitch.valid.toBoolean) keySwitches += io.keySwitch.payload.toLong
      if (io.keyRequest.toBoolean) keyRequests += 1
      if (io.job.valid.toBoolean) jobs += ((frame, io.job.payload.alloc.allocId.toInt, io.job.payload.bit.toLong, io.job.payload.sn.toBoolean))
      if (io.hwLost.toBoolean || io.jobLost.toBoolean) lost = true
    }
    def state = io.state.toEnum.position + 1
    def frameBit(n: Int): Long = (n.toLong * usFrameBytes * 8) & ((1L << 27) - 1)

    /** One downstream frame: frame start, its PLOAMd (good CRC unless `bad`), its allocations; one PLOAMu slot. */
    def send(m: Option[BigInt], allocs: Seq[(Int, Int, Int, Int)] = Nil, bad: Boolean = false): Unit = {
      io.frameBit #= frameBit(frame)
      io.frameStart #= true
      cd.waitSampling()
      io.frameStart #= false
      for (x <- m) { io.ploam.valid #= true; io.ploam.value #= x; io.ploam.crcOk #= !bad; cd.waitSampling(); io.ploam.valid #= false }
      for ((id, flags, start, stop) <- allocs) {
        io.alloc.valid #= true
        io.alloc.value.allocId #= id
        io.alloc.value.flags #= flags
        io.alloc.value.start #= start
        io.alloc.value.stop #= stop
        io.alloc.crcOk #= true
        cd.waitSampling()
      }
      io.alloc.valid #= false
      cd.waitSampling(4)
      if (io.usPloam.valid.toBoolean) { // the grant's PLOAMu: popped at the next edge
        upstream += io.usPloam.payload.toBigInt
        io.usPloam.ready #= true
        cd.waitSampling()
        io.usPloam.ready #= false
      }
      cd.waitSampling(4)
      frame += 1
    }
    def copies(m: BigInt, n: Int = 3): Unit = for (_ <- 0 until n) send(Some(m))
    def idle(n: Int): Unit = for (_ <- 0 until n) send(None)
    /** Sends, then the upstream PLOAMs it caused (drained in idle frames). */
    def answers(body: => Unit): Seq[BigInt] = { val u = upstream.length; body; idle(6); upstream.drop(u).toSeq }
    def jobsOf(f: Int) = jobs.filter(_._1 == f)

    /** Parameter learning, SN acquisition and ranging, as each OLT runs it. */
    def activate(id: Int, eqd: Long, maple: Boolean): Unit = {
      idle(2)
      check(state == 2, s"synchronized: O2 (O$state)")
      val uo = ploam(0xFF, 0x01, 32, 0, 0, 0xAA, 0xAB, 0x59, 0x83, 0x20, 0, 5) // guard 32, pre-assigned delay 5 x 32 bytes
      copies(uo)
      if (maple) copies(ploam(0xFF, 0x14, 120, 70)) // Extended_Burst_Length: type 3 preamble 120 / 70 bytes
      check(state == 3, "Upstream_Overhead: O3")
      if (maple) check(io.overhead.bytes.toInt == (32 + 24) / 8 + 120, s"EBL: overhead ${io.overhead.bytes.toInt} bytes before ranging")
      if (maple) copies(withSn(ploam(0xFF, 0x06, 0x00), other)) // Maple enables each discovered SN before assigning it
      copies(withSn(ploam(0xFF, 0x03, id - 1), other))
      check(state == 3 && io.onuId.toInt == 0xFF, "Assign_ONU-ID of another SN: ignored")
      send(None, Seq((OltScheduler.snAllocId, ploamU, 100, 112)))
      idle(1)
      val snJob = jobs.last
      check(snJob._4 && snJob._2 == OltScheduler.snAllocId, "SN request answered")
      if (maple) copies(withSn(ploam(0xFF, 0x06, 0x00), sn))
      check(state == 3, "Disable_Serial_Number 0x00 (enable) of our SN in O3: no change")
      val before = upstream.length
      copies(withSn(ploam(0xFF, 0x03, id), sn))
      check(state == 4 && io.onuId.toInt == id, s"Assign_ONU-ID $id x3: O4")
      send(None, Seq((id, ploamU, 200, 212)))
      idle(1)
      check(jobs.last._2 == id && jobs.last._4, "ranging grant answered with the serial number")
      copies(ploam(id, 0x04, 0, (eqd >> 24).toInt & 0xFF, (eqd >> 16).toInt & 0xFF, (eqd >> 8).toInt & 0xFF, eqd.toInt & 0xFF))
      check(state == 5 && io.eqd.toLong == eqd, s"Ranging_Time x3: O5, EqD $eqd")
      if (maple) check(io.overhead.bytes.toInt == (32 + 24) / 8 + 70, s"EBL: overhead ${io.overhead.bytes.toInt} bytes once ranged")
      idle(4)
      check(upstream.length == before, "no upstream PLOAM during ranging (the SN goes in the burst)")
    }

    def acked(id: Int, msgs: Seq[BigInt]): Unit = {
      val got = answers(msgs.foreach(copies(_)))
      check(got == msgs.flatMap(m => Seq.fill(3)(ack(id, m))),
        f"each copy acknowledged in order: ${msgs.map(m => f"0x${bigToBytes(m, 12)(1)}%02X").mkString(" ")}")
      check(msgs.forall(m => toCpu.count(_ == m) >= 3), "and each copy given to the CPU")
    }

    // ZTE C300/C600 (Entel field, AXS DC3): activation, provisioning, EqD drift, deactivation every ~10 s with a new ID
    val zteId = 83
    val zteEqd = 0x34799L
    activate(zteId, zteEqd, maple = false)
    check(answers(send(Some(ploam(zteId, 0x09)))) == Seq.fill(3)(pw(zteId)), "Request_Password x1: Password x3")
    val reqKey = ploam(zteId, 0x0D)
    check(answers(send(Some(reqKey))).isEmpty && toCpu.contains(reqKey) && keyRequests == 1,
      "Request_Key x1: to the CPU only (it sends the key), a pending key switch void")
    acked(zteId, Seq(ploam(zteId, 0x13, 0x2F, 0x24, 0x0B, 0x1D),            // Key_Switching_Time
      ploam(zteId, 0x0E, 0x01, 0x05, 0x30),                                // Configure_Port-ID: OMCI 0x053
      ploam(zteId, 0x08, 0x02, 0x05, 0x30),                                // Encrypted_Port-ID
      ploam(zteId, 0x12, 0x00, 0x00, 0x9C, 0x40),                          // BER_Interval 40000 frames
      ploam(zteId, 0x0A, 0x05, 0x30, 0x01)) ++                             // Assign_Alloc-ID: the default one again
      (0x164 to 0x161 by -1).map(p => ploam(zteId, 0x08, 0x02, p >> 4, (p & 0xF) << 4)) ++
      (0x1E3 to 0x1E0 by -1).map(a => ploam(zteId, 0x0A, a >> 4, (a & 0xF) << 4, 0x01)))
    check(io.omciValid.toBoolean && io.omciPort.toInt == 0x053, "OMCI Port-ID 0x053")
    check(keySwitches == Seq.fill(3)(0x2F240B1DL), "Key_Switching_Time x3: superframe 0x2F240B1D to the decryption")
    check(io.encryptedUsed.toInt == 0, "Encrypted_Port-ID with a = 0, as ZTE sends them: no port encrypted")
    def encryptedPorts = io.encryptedPorts.zipWithIndex.filter(p => (io.encryptedUsed.toInt >> p._2 & 1) == 1).map(_._1.toInt)
    acked(zteId, Seq(ploam(zteId, 0x08, 0x03, 0x16, 0x40), ploam(zteId, 0x08, 0x01, 0x05, 0x30)))
    check(encryptedPorts == Seq(0x164), "Encrypted_Port-ID a = 1: 0x164 encrypted, once for 3 copies; b = 0: ignored")
    acked(zteId, Seq(ploam(zteId, 0x08, 0x02, 0x16, 0x40), ploam(zteId, 0x08, 0x03, 0x05, 0x30)))
    check(encryptedPorts == Seq(0x053), "a = 0: 0x164 clear again")

    val grants = Seq((zteId, 0, 100, 140)) ++ (0x1E0 to 0x1E3).zipWithIndex.map { case (a, k) => (a, 0, 150 + 50 * k, 190 + 50 * k) }
    send(None, grants)
    idle(1)
    val f0 = frame - 2
    def base(f: Int, eqd: Long) = (frameBit(f) + responseBits + eqd) & ((1L << 27) - 1)
    check(jobsOf(f0).map(j => (j._2, j._3)) == grants.map(g => (g._1, base(f0, zteEqd) + g._3 * 8)),
      s"the default and the four T-CONTs served at frame start + response + EqD + StartTime (${jobsOf(f0)})")
    // OLT drift monitor: a new Ranging_Time in O5, 5 bits shorter (seen every 10 s to 10 min on Entel)
    val drifted = zteEqd - 5
    check(answers(copies(ploam(zteId, 0x04, 0, 0, 0x03, 0x47, 0x94))).isEmpty && state == 5, "drift Ranging_Time: O5, no answer")
    send(None, grants)
    idle(1)
    check(jobsOf(frame - 2).map(_._3) == grants.map(g => base(frame - 2, drifted) + g._3 * 8),
      "bursts move by the EqD change exactly, from the next frame")
    check(answers(copies(ploam(zteId, 0x04, 1, 0, 0, 0, 7))).isEmpty && io.eqd.toLong == drifted, "secondary path Ranging_Time: ignored")
    send(Some(ploam(zteId, 0x05)), bad = true)
    check(state == 5, "Deactivate_ONU-ID with a bad CRC: ignored")

    // ZTE sends Deactivate once, a few ms after the last Acknowledge; then a new activation with the next ONU-ID
    check(answers(send(Some(ploam(zteId, 0x05)))).isEmpty && state == 2 && io.onuId.toInt == 0xFF && !io.omciValid.toBoolean &&
      io.encryptedUsed.toInt == 0, "Deactivate_ONU-ID x1: O2, ONU-ID, OMCI and encrypted ports gone, no answer")
    send(None, grants)
    idle(1)
    check(jobsOf(frame - 2).isEmpty, "deactivated: no burst for the old allocations")
    activate(zteId + 1, zteEqd, maple = false)
    send(None, grants :+ ((zteId + 1, 0, 400, 440)))
    idle(1)
    check(jobsOf(frame - 2).map(_._2) == Seq(zteId + 1), "re-activated with ONU-ID 84: only its default allocation, old T-CONTs dropped")

    // Broadcom Maple (HSGQ G008 lab): EBL, Disable_SN enable before each Assign_ONU-ID, Deactivate x3
    copies(ploam(zteId + 1, 0x05))
    val mapleId = 4
    val mapleEqd = 0xA7BE6L
    activate(mapleId, mapleEqd, maple = true)
    check(answers(send(Some(ploam(mapleId, 0x09)))) == Seq.fill(3)(pw(mapleId)), "Request_Password x1: Password x3")
    acked(mapleId, Seq(ploam(mapleId, 0x0E, 0x01, 0x00, 0x40),             // Configure_Port-ID: OMCI 0x004
      ploam(mapleId, 0x12, 0x00, 0x01, 0x38, 0x80),                        // BER_Interval 80000 frames
      ploam(mapleId, 0x08, 0x02, 0x00, 0x40),
      ploam(mapleId, 0x08, 0x02, 0x13, 0x90),
      ploam(mapleId, 0x0A, 0x10, 0x40, 0x01)))
    // other ONUs join while we run
    val foreign = Seq(withSn(ploam(0xFF, 0x06, 0x00), other), withSn(ploam(0xFF, 0x03, 2), other))
    check(answers(foreign.foreach(copies(_))).isEmpty && state == 5 && io.onuId.toInt == mapleId, "another ONU's enable and ONU-ID: no effect")
    // messages the hardware leaves to the CPU (PEE, Change_Power_Level, PST, Swift_POPUP, Ranging_Adjustment, Sleep_Allow)
    val cpuOnly = Seq(0x0F, 0x10, 0x11, 0x16, 0x17, 0x18).map(k => ploam(mapleId, k, 0x01))
    check(answers(cpuOnly.foreach(m => send(Some(m)))).isEmpty && state == 5 && cpuOnly.forall(toCpu.contains),
      "PLOAMs without a fixed answer: to the CPU, O5 kept, nothing from the hardware")
    check(answers(copies(ploam(mapleId, 0x05))).isEmpty && state == 2, "Deactivate_ONU-ID x3: O2 once, the copies ignored")
    activate(mapleId, mapleEqd, maple = true)
    copies(withSn(ploam(0xFF, 0x06, 0xFF), sn))
    check(state == 7, "Disable_Serial_Number of our SN: O7")
    send(None, Seq((mapleId, ploamU, 100, 140)))
    idle(1)
    check(jobsOf(frame - 2).isEmpty, "O7: no burst")
    copies(withSn(ploam(0xFF, 0x06, 0x00), other))
    check(state == 7, "enable of another SN: still O7")
    copies(withSn(ploam(0xFF, 0x06, 0x00), sn))
    check(state == 2, "enable of our SN: O2")
    check(!lost, "no upstream PLOAM or job lost")
  }
  println("PASS OnuPloamQuirksTest")
}
