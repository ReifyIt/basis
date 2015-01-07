//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.stat

import basis.util._
import scala.annotation._

@implicitNotFound("No implicit source of randomness available.")
abstract class Random {
  def randomByte(): Byte

  def randomShort(): Short

  def randomInt(): Int

  def randomLong(): Long

  def randomFloat(): Float

  def randomDouble(): Double

  def randomBoolean(): Boolean

  def randomByteBelow(upper: Byte): Byte = {
    if (upper < 0) throw new IllegalArgumentException("Invalid upper bound"+" "+"("+ upper +")"+".")
    (randomInt() % upper).abs.toByte
  }

  def randomShortBelow(upper: Short): Short = {
    if (upper < 0) throw new IllegalArgumentException("Invalid upper bound"+" "+"("+ upper +")"+".")
    (randomInt() % upper).abs.toShort
  }

  def randomIntBelow(upper: Int): Int = {
    if (upper < 0) throw new IllegalArgumentException("Invalid upper bound"+" "+"("+ upper +")"+".")
    (randomInt() % upper).abs
  }

  def randomLongBelow(upper: Long): Long = {
    if (upper < 0L) throw new IllegalArgumentException("Invalid upper bound"+" "+"("+ upper +")"+".")
    (randomLong() % upper).abs
  }

  def randomFloatBelow(upper: Float): Float = {
    if (upper.isNaN || upper.isInfinite || upper < 0.0F)
      throw new IllegalArgumentException("Invalid upper bound"+" "+"("+ upper +")"+".")
    randomFloat() * upper
  }

  def randomDoubleBelow(upper: Double): Double = {
    if (upper.isNaN || upper.isInfinite || upper < 0.0)
      throw new IllegalArgumentException("Invalid upper bound"+" "+"("+ upper +")"+".")
    randomDouble() * upper
  }

  def randomByteBetween(lower: Byte, upper: Byte): Byte = {
    if (upper < lower) throw new IllegalArgumentException("Invalid bounds"+" "+"["+ lower +", "+ upper+"]"+".")
    val size = (upper - lower) + 1
    (lower + (randomInt() % size).abs).toByte
  }

  def randomShortBetween(lower: Short, upper: Short): Short = {
    if (upper < lower) throw new IllegalArgumentException("Invalid bounds"+" "+"["+ lower +", "+ upper+"]"+".")
    val size = (upper - lower) + 1
    (lower + (randomInt() % size).abs).toShort
  }

  def randomIntBetween(lower: Int, upper: Int): Int = {
    val size = (upper - lower) + 1
    if (upper < lower || size < 0)
      throw new IllegalArgumentException("Invalid bounds"+" "+"["+ lower +", "+ upper+"]"+".")
    lower + (randomInt() % size).abs
  }

  def randomLongBetween(lower: Long, upper: Long): Long = {
    val size = (upper - lower) + 1L
    if (upper < lower || size < 0L)
      throw new IllegalArgumentException("Invalid bounds"+" "+"["+ lower +", "+ upper+"]"+".")
    lower + (randomLong() % size).abs
  }

  def randomFloatBetween(lower: Float, upper: Float): Float = {
    val size = upper - lower
    if (size.isNaN || size.isInfinite || size < 0.0F)
      throw new IllegalArgumentException("Invalid bounds"+" "+"["+ lower +", "+ upper+")"+".")
    lower + randomFloat() * size
  }

  def randomDoubleBetween(lower: Double, upper: Double): Double = {
    val size = upper - lower
    if (size.isNaN || size.isInfinite || size < 0.0)
      throw new IllegalArgumentException("Invalid bounds"+" "+"["+ lower +", "+ upper+")"+".")
    lower + randomDouble() * size
  }
}
