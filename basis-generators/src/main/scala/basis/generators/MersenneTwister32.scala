/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.generators

import basis.util._

/** A 32-bit pseudorandom number generator. Implements Makoto Matsumoto and
  * Takuji Nishimura's MT19937 algorithm.
  * 
  * @see  [[http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/MT2002/emt19937ar.html MT19937]]
  */
final class MersenneTwister32 private (
    private[this] val state: Array[Int],
    private[this] var index: Int)
  extends Randomness with Arbitrary[Int] {
  
  def this(seed: Int) = {
    this(new Array[Int](624), 0)
    state(0) = seed
    var i = 1
    while (i < 624) {
      state(i) = 1812433253 * (state(i - 1) ^ (state(i - 1) >>> 30)) + i
      i += 1
    }
  }
  
  def this(key: Array[Int]) = {
    this(19650218)
    var i = 0
    var j = 0
    var k = 624 max key.length
    while (k != 0) {
      state(i) = (state(i) ^ ((state(i - 1) ^ (state(i - 1) >>> 30)) * 1664525)) + key(j) + j
      i += 1
      j += 1
      if (i > 623) {
        state(0) = state(623)
        i = 1
      }
      if (j >= key.length) j = 0
      k -= 1
    }
    k = 623
    while (k != 0) {
      state(i) = (state(i) ^ ((state(i - 1) ^ (state(i - 1) >>> 30)) * 1566083941)) - i
      i += 1
      if (i > 623) {
        state(0) = state(623)
        i = 1
      }
      k -= 1
    }
    state(0) = 1 << 31
  }
  
  def this() = this(java.lang.System.currentTimeMillis.toInt)
  
  private[this] def generate() {
    val state = this.state
    var x = 0
    var i = 0
    while (i < 227) {
      x = (state(i) & 0x80000000) | (state(i + 1) & 0x7FFFFFFF)
      state(i) = state(i + 397) ^ (x >>> 1) ^ (if ((x & 1) == 0) 0 else 0x9908B0DF)
      i += 1
    }
    while (i < 623) {
      x = (state(i) & 0x80000000) | (state(i + 1) & 0x7FFFFFFF)
      state(i) = state(i - 227) ^ (x >>> 1) ^ (if ((x & 1) == 0) 0 else 0x9908B0DF)
      i += 1
    }
    x = (state(623) & 0x80000000) | (state(0) & 0x7FFFFFFF)
    state(623) = state(396) ^ (x >>> 1) ^ (if ((x & 1) == 0) 0 else 0x9908B0DF)
  }
  
  override def apply(): Int = {
    if (index == 0) generate()
    
    var x = state(index)
    x ^= (x >>> 11)
    x ^= (x <<   7) & 0x9D2C5680
    x ^= (x <<  15) & 0xEFC60000
    x ^= (x >>> 18)
    
    index = if (index < 623) index + 1 else 0
    x
  }
  
  override def asInt: Arbitrary[Int] = this
  
  override def nextByte(): Byte = apply().toByte
  
  override def nextShort(): Short = apply().toShort
  
  override def nextInt(): Int = apply()
  
  override def nextLong(): Long = (apply().toLong << 32) | apply().toLong
  
  override def nextFloat(): Float = (nextInt() >>> 8).toFloat / (1 << 24).toFloat
  
  override def nextDouble(): Double = (nextLong() >>> 11).toDouble / (1L << 53).toDouble
  
  override def nextBoolean(): Boolean = (apply() >>> 31) != 0
  
  override def toString: String = "MersenneTwister32"
}
