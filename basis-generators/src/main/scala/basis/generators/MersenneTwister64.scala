/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.generators

import basis.util._

/** A 64-bit pseudorandom number generator. Implements Makoto Matsumoto and
  * Takuji Nishimura's MT19937-64 algorithm.
  * 
  * @see  [[http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt64.html MT19937-64]]
  */
final class MersenneTwister64 private (
    private[this] val state: Array[Long],
    private[this] var index: Int)
  extends Randomness with Arbitrary[Long] {
  
  def this(seed: Long) = {
    this(new Array[Long](312), 0)
    state(0) = seed
    var i = 1
    while (i < 312) {
      state(i) = 6364136223846793005L * (state(i - 1) ^ (state(i - 1) >>> 62)) + i
      i += 1
    }
  }
  
  def this(key: Array[Long]) = {
    this(19650218L)
    var i = 0
    var j = 0
    var k = 312 max key.length
    while (k != 0) {
      state(i) = (state(i) ^ ((state(i - 1) ^ (state(i - 1) >>> 62)) * 3935559000370003845L)) + key(j) + j
      i += 1
      j += 1
      if (i > 311) {
        state(0) = state(311)
        i = 1
      }
      if (j >= key.length) j = 0
      k -= 1
    }
    k = 311
    while (k != 0) {
      state(i) = (state(i) ^ ((state(i - 1) ^ (state(i - 1) >>> 62)) * 2862933555777941757L)) - i
      i += 1
      if (i > 311) {
        state(0) = state(311)
        i = 1
      }
      k -= 1
    }
    state(0) = 1L << 63
  }
  
  def this() = this(java.lang.System.currentTimeMillis)
  
  private[this] def generate() {
    val state = this.state
    var x = 0L
    var i = 0
    while (i < 156) {
      x = (state(i) & 0xFFFFFFFF80000000L) | (state(i + 1) & 0x000000007FFFFFFFL)
      state(i) = state(i + 156) ^ (x >>> 1) ^ (if ((x & 1L) == 0L) 0L else 0xB5026F5AA96619E9L)
      i += 1
    }
    while (i < 311) {
      x = (state(i) & 0xFFFFFFFF80000000L) | (state(i + 1) & 0x000000007FFFFFFFL)
      state(i) = state(i - 156) ^ (x >>> 1) ^ (if ((x & 1L) == 0L) 0L else 0xB5026F5AA96619E9L)
      i += 1
    }
    x = (state(311) & 0xFFFFFFFF80000000L) | (state(0) & 0x000000007FFFFFFFL)
    state(311) = state(155) ^ (x >>> 1) ^ (if ((x & 1L) == 0L) 0L else 0xB5026F5AA96619E9L)
  }
  
  override def apply(): Long = {
    if (index == 0) generate()
    
    var x = state(index)
    x ^= (x >>> 29) & 0x5555555555555555L
    x ^= (x <<  17) & 0x71D67FFFEDA60000L
    x ^= (x <<  37) & 0xFFF7EEE000000000L
    x ^= (x >>> 43)
    
    index = if (index < 311) index + 1 else 0
    x
  }
  
  override def nextByte(): Byte = apply().toByte
  
  override def nextShort(): Short = apply().toShort
  
  override def nextInt(): Int = apply().toInt
  
  override def nextLong(): Long = apply()
  
  override def nextFloat(): Float = (nextInt() >>> 8).toFloat / (1 << 24).toFloat
  
  override def nextDouble(): Double = (nextLong() >>> 11).toDouble / (1L << 53).toDouble
  
  override def nextBoolean(): Boolean = (apply() >>> 63) != 0
  
  override def toString: String = "MersenneTwister64"
}
