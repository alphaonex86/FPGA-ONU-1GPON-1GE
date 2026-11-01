// SPDX-License-Identifier: CERN-OHL-S-2.0
package pon.gpon

import spinal.core._
import spinal.lib._
import GponCodes._

/** GF(256) of the G.984 FEC: x^8+x^4+x^3+x^2+1, alpha = 2. */
object Gf {
  def mulConst(x: Bits, c: Int): Bits =
    Vec((0 until 8).map(bit => (0 until 8).filter(k => (Model.gfMul(1 << k, c) >> bit & 1) == 1).map(x(_)).foldLeft(False)(_ ^ _))).asBits
  def mul(a: Bits, b: Bits): Bits = (0 until 8).map(k => mulConst(a, 1 << k) & B(8 bits, default -> b(k))).reduce(_ ^ _)
  def inverse(x: Bits): Bits = Vec((0 until 256).map(v => B(Model.gfInv(v), 8 bits)))(x.asUInt)
  def alpha(i: Int): Int = Model.alphaPow(((i % 255) + 255) % 255)
}

/** RS(255,239) decoder of the downstream FEC (G.984.3 13.2: t = 8, roots alpha^0-15), one byte per clock, the bytes and
  * their tag out `latency` clocks later. Syndromes per codeword; after its end, inversionless Berlekamp-Massey and the
  * error evaluator, 3 clocks a step; as the bytes leave the delay RAM, Chien search and Forney correct them. A codeword
  * with fewer locator roots than its degree is uncorrectable: counted, its bytes corrected so far stay so. Codewords:
  * 255 bytes or the short last one (`shortLength`, zero pad implied); `enable` with `end`: decode it, else pass. */
case class FecDecoder[T <: Data](tagType: HardType[T], shortLength: Int = frameBytes % 255, delay: Int = 340) extends Component {
  val io = new Bundle {
    val data = in Bits (8 bits)
    val start, end = in Bool () // the codeword's first, last byte
    val enable = in Bool ()
    val tag = in(tagType())
    val outData = out Bits (8 bits)
    val outTag = out(tagType())
    val codewords, correctedBytes, correctedCodewords, uncorrectable = out UInt (32 bits)
  }
  val t = 8
  def zero = B(0, 8 bits)

  val syndromes = Vec(Reg(Bits(8 bits)), 2 * t)
  for ((s, i) <- syndromes.zipWithIndex) s := (io.start ? zero | Gf.mulConst(s, Gf.alpha(i))) ^ io.data
  val length = Reg(UInt(8 bits))
  length := io.start ? U(1, 8 bits) | length + 1
  val go = RegNext(io.end) init (False) // syndromes final
  val goShort = RegNext(length =/= 255 - 1)
  val goEnabled = RegNext(io.enable)

  // Berlekamp-Massey: delta = sum lambda_i S_(r-i) (window(i) = S_(r-i)), the same sums give the evaluator after
  val lambda, b = Vec(Reg(Bits(8 bits)), t + 1)
  val omega = Vec(Reg(Bits(8 bits)), t)
  val gamma = Reg(Bits(8 bits))
  val k = Reg(SInt(6 bits))
  val window = Vec(Reg(Bits(8 bits)), t + 1)
  val queue = Vec(Reg(Bits(8 bits)), 2 * t) // the syndromes to shift in, rotated
  val products = Vec(Reg(Bits(8 bits)), t + 1)
  val delta = Reg(Bits(8 bits))
  val step = Reg(UInt(5 bits)) init (0) // 0-15 locator, 16-23 evaluator
  val phase = Reg(UInt(2 bits)) init (0)
  val busy = RegInit(False)
  val short, decoding = Reg(Bool())
  val finished = RegInit(False)
  finished := False
  for (i <- 0 to t) products(i) := Gf.mul(lambda(i), window(i))
  delta := products.reduce(_ ^ _)
  def restartWindow(): Unit = { window(0) := queue(0); for (i <- 1 to t) window(i) := zero } // S_0, for the evaluator
  when(busy) {
    phase := (phase === 2) ? U(0, 2 bits) | phase + 1
    when(phase === 2) {
      step := step + 1
      when(step < 2 * t) {
        for (i <- 0 to t) lambda(i) := Gf.mul(gamma, lambda(i)) ^ (if (i == 0) zero else Gf.mul(delta, b(i - 1)))
        when(delta =/= 0 && !k.msb) {
          b := lambda
          k := -k - 1
          gamma := delta
        } otherwise {
          for (i <- 0 to t) b(i) := (if (i == 0) zero else b(i - 1))
          k := k + 1
        }
      } otherwise (omega((step - 2 * t).resize(3)) := delta)
      window(0) := queue(0)
      for (i <- 1 to t) window(i) := window(i - 1)
      for (i <- 0 until 2 * t) queue(i) := queue((i + 1) % (2 * t))
      when(step === 2 * t - 1)(restartWindow())
      when(step === 3 * t - 1) { busy := False; finished := True }
    }
  }
  when(go) {
    for (i <- 0 until 2 * t) queue(i) := syndromes((i + 1) % (2 * t))
    window(0) := syndromes(0)
    for (i <- 1 to t) window(i) := zero
    for (i <- 0 to t) { lambda(i) := (if (i == 0) B(1, 8 bits) else zero); b(i) := (if (i == 0) B(1, 8 bits) else zero) }
    gamma := 1
    k := 0
    step := 0
    phase := 0
    busy := True
    short := goShort
    decoding := goEnabled
  }
  // results, held until the codeword leaves the delay RAM
  val located = Vec(Reg(Bits(8 bits)), t + 1)
  val evaluator = Vec(Reg(Bits(8 bits)), t)
  val degree = Reg(UInt(4 bits))
  val locatedShort, locatedDecoding = Reg(Bool())
  when(finished) {
    for (i <- 0 to t) located(i) := decoding ? lambda(i) | (if (i == 0) B(1, 8 bits) else zero)
    for (i <- 0 until t) evaluator(i) := decoding ? omega(i) | zero
    degree := decoding ? (t to 0 by -1).foldLeft(U(0, 4 bits))((d, i) => (lambda(i) =/= 0 && d === 0) ? U(i, 4 bits) | d) | U(0, 4 bits)
    locatedShort := short
    locatedDecoding := decoding
  }

  // delay RAM
  case class Delayed() extends Bundle {
    val data = Bits(8 bits)
    val start, end = Bool()
    val tag = tagType()
  }
  val written = Delayed()
  written.data := io.data
  written.start := io.start
  written.end := io.end
  written.tag := io.tag
  val ram = Mem(Delayed(), 512)
  val writeAt = Reg(UInt(9 bits)) init (0)
  writeAt := writeAt + 1
  ram.write(writeAt, written)
  val leaving = ram.readSync(writeAt - (delay - 1))
  val ready = RegInit(False) // the RAM words read from here on were written
  when(writeAt === delay - 1)(ready := True)

  // Chien search and Forney: for byte j of n, X^-1 = alpha^-(n-1-j); terms start at lambda_i alpha^-i(n-1), times alpha^i a byte
  val chien = Vec(Reg(Bits(8 bits)), t + 1)
  val chienOmega = Vec(Reg(Bits(8 bits)), t)
  val codewordDecoding = RegInit(False)
  val codewordDegreeValue = Reg(UInt(4 bits))
  def startTerm(x: Bits, i: Int): Bits = locatedShort ? Gf.mulConst(x, Gf.alpha(-i * (shortLength - 1))) | Gf.mulConst(x, Gf.alpha(-i * 254))
  when(ready && leaving.start) {
    for (i <- 0 to t) chien(i) := startTerm(located(i), i)
    for (i <- 0 until t) chienOmega(i) := startTerm(evaluator(i), i)
    codewordDecoding := locatedDecoding
    codewordDegreeValue := degree
  } otherwise {
    for (i <- 0 to t) chien(i) := Gf.mulConst(chien(i), Gf.alpha(i))
    for (i <- 0 until t) chienOmega(i) := Gf.mulConst(chienOmega(i), Gf.alpha(i))
  }
  val s1 = RegNext(leaving)
  val s2 = RegNext(s1)
  val root = RegNext(chien.reduce(_ ^ _) === 0) init (False)
  val odd = RegNext((1 to t by 2).map(chien(_)).reduce(_ ^ _))
  val omegaSum = RegNext(chienOmega.reduce(_ ^ _))
  val s3 = RegNext(s2)
  val rootS3 = RegNext(root) init (False)
  val oddInverse = RegNext(Gf.inverse(odd))
  val omegaS3 = RegNext(omegaSum)
  io.outData := RegNext(s3.data ^ (rootS3 ? Gf.mul(omegaS3, oddInverse) | zero))
  io.outTag := RegNext(s3.tag)

  // statistics at the codeword's last byte (s2: the root of that byte known)
  val roots = Reg(UInt(4 bits))
  val rootsNow = (s2.start ? U(0, 4 bits) | roots) + root.asUInt
  roots := rootsNow
  val codewords, correctedBytes, correctedCodewords, uncorrectable = Reg(UInt(32 bits)) init (0)
  val decodingS2 = RegNext(codewordDecoding) // the next codeword's is set with s1's start
  val degreeS2 = RegNext(codewordDegreeValue)
  when(RegNext(RegNext(ready)) && s2.end && decodingS2) {
    codewords := codewords + 1
    when(rootsNow === degreeS2) {
      correctedBytes := correctedBytes + degreeS2
      when(degreeS2 =/= 0)(correctedCodewords := correctedCodewords + 1)
    } otherwise (uncorrectable := uncorrectable + 1)
  }
  io.codewords := codewords
  io.correctedBytes := correctedBytes
  io.correctedCodewords := correctedCodewords
  io.uncorrectable := uncorrectable
}
