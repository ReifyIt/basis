/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.util

import java.lang.Double.doubleToLongBits
import java.lang.Float.floatToIntBits
import java.lang.Integer.{ rotateLeft => rotl }

/** Contains a straightforward implementation of Austin Appleby's MurmurHash 3 algorithm.
  * Implements MurmurHash3_x86_32 revision 136.
  * 
  * The name '''murmur''' comes from '''mu'''ltiply '''r'''otate '''mu'''ltiply
  * '''r'''otate–the simplest sequence of operations that will thoroughly mix
  * the bits of a value. `MurmurHash` actually multiples, shifts, and xors, but
  * Austin Appleby, the author of the algorithm, chose the more phonetic name murmor.
  * 
  * Example usage: `mash(mix(mix(mix(seed, x), y), z))`
  * 
  * @author Chris Sachs
  * 
  * @see    [[http://code.google.com/p/smhasher/]]
  */
object MurmurHash {
  /** Hashes a `Byte` value. */
  @inline def hash(value: Byte): Int = value.toInt
  
  /** Hashes a `Short` value. */
  @inline def hash(value: Short): Int = value.toInt
  
  /** Hashes an `Int` value. */
  @inline def hash(value: Int): Int = value
  
  /** Hashes a `Long` value. */
  @inline def hash(value: Long): Int = (value ^ (value >>> 32)).toInt
  
  /** Hashes a `Char` value. */
  @inline def hash(value: Char): Int = value.toInt
  
  /** Hashes a `Float` value. */
  @inline def hash(value: Float): Int = if (value == 0.0F) 0 else hash(floatToIntBits(value))
  
  /** Hashes a `Double` value. */
  @inline def hash(value: Double): Int = if (value == 0.0) 0 else hash(doubleToLongBits(value))
  
  /** Hashes a `Boolean` value. */
  @inline def hash(value: Boolean): Int = if (value) 1231 else 1237
  
  /** Hashes `()`. */
  @inline def hash(value: Unit): Int = 0
  
  /** Hashes `Any` value. */
  @inline def hash(value: Any): Int = value.hashCode
  
  /** Mixes a `Byte` value into the hash code. */
  @inline def mix(code: Int, value: Byte): Int = mix(code, hash(value))
  
  /** Mixes a `Short` value into the hash code. */
  @inline def mix(code: Int, value: Short): Int = mix(code, hash(value))
  
  /** Mixes an `Int` value into the hash code. */
  def mix(code: Int, value: Int): Int = {
    var h = code
    var k = value
    
    k *= 0xcc9e2d51
    k = rotl(k, 15)
    k *= 0x1b873593
    
    h ^= k
    
    h = rotl(h, 13)
    h = h * 5 + 0xe6546b64
    
    h
  }
  
  /** Mixes a `Long` value into the hash code. */
  @inline def mix(code: Int, value: Long): Int = mix(code, hash(value))
  
  /** Mixes a `Char` value into the hash code. */
  @inline def mix(code: Int, value: Char): Int = mix(code, hash(value))
  
  /** Mixes a `Float` value into the hash code. */
  @inline def mix(code: Int, value: Float): Int = mix(code, hash(value))
  
  /** Mixes a `Double` value into the hash code. */
  @inline def mix(code: Int, value: Double): Int = mix(code, hash(value))
  
  /** Mixes a `Boolean` value into the hash code. */
  @inline def mix(code: Int, value: Boolean): Int = mix(code, hash(value))
  
  /** Mixes `()` into the hash code. */
  @inline def mix(code: Int, value: Unit): Int = mix(code, hash(value))
  
  /** Mixes `Any` value into the hash code. */
  @inline def mix(code: Int, value: Any): Int = mix(code, hash(value))
  
  /** Finalizes the hash code. */
  def mash(code: Int): Int = {
    var h = code
    
    h ^= h >>> 16
    h *= 0x85ebca6b
    h ^= h >>> 13
    h *= 0xc2b2ae35
    h ^= h >>> 16
    
    h
  }
}
