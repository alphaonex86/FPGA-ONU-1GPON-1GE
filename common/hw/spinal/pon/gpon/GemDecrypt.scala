// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.lib._
import pon.Aes128

/** Downstream AES-128 counter-mode decryption of the encrypted Port-IDs (G.984.3 clause 12.2), downstream domain.
  * Each fragment's header starts its keystream (counter: the crypto-counter at the header, +1 per block) while its
  * bytes wait `delay` clocks in a RAM. The key in use becomes the shadow key at the first fragment of the frame set by
  * Key_Switching_Time; Request_Key voids that switch. A fragment whose keystream is late is dropped as cut, `late`
  * pulses, and its blocks are discarded when they come. */
case class GemDecrypt(maxPorts: Int = 16, delay: Int = 28, keystreamBlocks: Int = 4) extends Component {
  val io = new Bundle {
    val header = slave Flow (GemHeaderEvent()) // one clock before the fragment's first byte
    val input = slave Flow (Fragment(GemRxByte()))
    val inputCut = in Bool ()
    val output = master Flow (Fragment(GemRxByte()))
    val outputCut = out Bool ()
    val ports = in Vec (UInt(12 bits), maxPorts)
    val portsUsed = in Bits (maxPorts bits)
    val shadowKey = in Bits (128 bits)
    val keySwitch = slave Flow (UInt(30 bits))
    val keyRequest = in Bool ()
    val late = out Bool ()
  }

  case class Job() extends Bundle {
    val counter = UInt(46 bits)
    val blocks = UInt(9 bits)
    val switchKey = Bool() // first job of the switch frame or later: the shadow key from it on
  }
  val jobs = StreamFifo(Job(), 4)
  val encrypted = io.ports.zip(io.portsUsed.asBools).map { case (p, u) => u && p === io.header.portId }.orR
  val headerSeen = RegNext(io.header.valid) init (False)
  jobs.io.push.valid := RegNext(io.header.valid && encrypted) init (False)
  jobs.io.push.counter := RegNext(io.header.counter)
  jobs.io.push.blocks := RegNext((io.header.pli +^ 15) >> 4).resized
  val switchFrame = Reg(UInt(30 bits))
  val armed = RegInit(False)
  jobs.io.push.switchKey := RegNext(armed && !(io.header.counter(45 downto 16) - switchFrame).msb)
  when(jobs.io.push.fire && jobs.io.push.switchKey)(armed := False)
  when(io.keySwitch.valid) { switchFrame := io.keySwitch.payload; armed := True }
  when(io.keyRequest)(armed := False)
  // the fragment's tags: decrypt (its job queued), lost (encrypted, job queue full)
  val decrypt, lost = RegInit(False)
  when(headerSeen) { decrypt := jobs.io.push.fire; lost := jobs.io.push.valid && !jobs.io.push.ready }

  case class Delayed() extends Bundle {
    val valid, cut, decrypt, lost = Bool()
    val gem = Fragment(GemRxByte())
  }
  val line = Delayed()
  line.valid := RegNext(io.input.valid) init (False)
  line.gem := RegNext(io.input.payload)
  line.cut := RegNext(io.inputCut) init (False)
  line.decrypt := decrypt
  line.lost := lost
  val ram = Mem(Delayed(), 32)
  val writeAt = Reg(UInt(5 bits)) init (0)
  writeAt := writeAt + 1
  ram.write(writeAt, line)
  val delayed = ram.readSync(writeAt - (delay - 1))
  val filled = RegInit(False) // every RAM word written
  when(writeAt === 31)(filled := True)
  val ready = RegNext(filled) init (False)

  // keystream: jobs in order
  val aes = Aes128()
  val key = Reg(Bits(128 bits)) init (0)
  val counter = Reg(UInt(46 bits))
  val blocksLeft = Reg(UInt(9 bits)) init (0)
  val job = jobs.io.pop.m2sPipe()
  job.ready := blocksLeft === 0
  when(job.fire) {
    counter := job.counter
    blocksLeft := job.blocks
    when(job.switchKey)(key := io.shadowKey)
  }
  val credits = Reg(UInt(log2Up(keystreamBlocks + 1) bits)) init (keystreamBlocks)
  aes.io.input.valid := blocksLeft =/= 0 && credits =/= 0
  aes.io.input.block := (counter @@ counter @@ counter).resize(128).asBits
  aes.io.input.key := key
  when(aes.io.input.fire) { counter := counter + 1; blocksLeft := blocksLeft - 1 }
  val keystream = StreamFifo(Bits(129 bits), keystreamBlocks) // its job's last block, the block
  keystream.io.push.valid := aes.io.output.valid
  keystream.io.push.payload := Delay(aes.io.input.fire && blocksLeft === 1, Aes128.latency, init = False) ## aes.io.output.payload
  val head = keystream.io.pop
  when(aes.io.input.fire && !head.fire)(credits := credits - 1)
  when(head.fire && !aes.io.input.fire)(credits := credits + 1)

  // a decrypted fragment ending without its last block used (late, cut) owes its leftover blocks: discarded
  val index = Reg(UInt(4 bits)) init (0)
  val dropping = RegInit(False)
  val owed = Reg(UInt(4 bits)) init (0)
  val byteValid = ready && delayed.valid && !dropping
  val cut = ready && delayed.cut
  val late = byteValid && (delayed.lost || delayed.decrypt && (owed =/= 0 || !head.valid))
  val use = byteValid && delayed.decrypt && !late
  val discard = owed =/= 0 && head.valid
  head.ready := discard || use && (index === 15 || delayed.gem.last)
  val owes = late && delayed.decrypt || cut && delayed.decrypt && !dropping
  owed := owed + owes.asUInt - (discard && head.payload.msb).asUInt
  when(use)(index := index + 1)
  when(late)(dropping := True)
  when(ready && delayed.valid && delayed.gem.last || cut) { index := 0; dropping := False }

  val keyByte = Vec(head.payload(127 downto 0).subdivideIn(8 bits).reverse)(index)
  io.output.valid := RegNext(byteValid && !late) init (False)
  io.output.last := RegNext(delayed.gem.last)
  io.output.portId := RegNext(delayed.gem.portId)
  io.output.pti := RegNext(delayed.gem.pti)
  io.output.data := RegNext(use ? (delayed.gem.data ^ keyByte) | delayed.gem.data)
  io.outputCut := RegNext(cut || late) init (False)
  io.late := RegNext(late) init (False)
}
