/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.random

import basis.util._

import scala.annotation.implicitNotFound

/** A source of pseudorandom primitive values.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  */
@implicitNotFound("No source of entropy available.")
abstract class Entropy {
  def nextByte(): Byte
  
  def nextShort(): Short
  
  def nextInt(): Int
  
  def nextLong(): Long
  
  def nextFloat(): Float
  
  def nextDouble(): Double
  
  def nextBoolean(): Boolean
  
  def nextBelow(upper: Byte): Byte = {
    if (upper < 0) throw new IllegalArgumentException("Invalid upper bound"+" "+"("+ upper +")"+".")
    (nextInt() % upper).abs.toByte
  }
  
  def nextBelow(upper: Short): Short = {
    if (upper < 0) throw new IllegalArgumentException("Invalid upper bound"+" "+"("+ upper +")"+".")
    (nextInt() % upper).abs.toShort
  }
  
  def nextBelow(upper: Int): Int = {
    if (upper < 0) throw new IllegalArgumentException("Invalid upper bound"+" "+"("+ upper +")"+".")
    (nextInt() % upper).abs
  }
  
  def nextBelow(upper: Long): Long = {
    if (upper < 0L) throw new IllegalArgumentException("Invalid upper bound"+" "+"("+ upper +")"+".")
    (nextLong() % upper).abs
  }
  
  def nextBelow(upper: Float): Float = {
    if (upper.isNaN || upper.isInfinite || upper < 0.0F)
      throw new IllegalArgumentException("Invalid upper bound"+" "+"("+ upper +")"+".")
    nextFloat() * upper
  }
  
  def nextBelow(upper: Double): Double = {
    if (upper.isNaN || upper.isInfinite || upper < 0.0)
      throw new IllegalArgumentException("Invalid upper bound"+" "+"("+ upper +")"+".")
    nextDouble() * upper
  }
  
  def nextBetween(lower: Byte, upper: Byte): Byte = {
    if (upper < lower) throw new IllegalArgumentException("Invalid bounds"+" "+"["+ lower +", "+ upper+"]"+".")
    val size = (upper - lower) + 1
    (lower + (nextInt() % size).abs).toByte
  }
  
  def nextBetween(lower: Short, upper: Short): Short = {
    if (upper < lower) throw new IllegalArgumentException("Invalid bounds"+" "+"["+ lower +", "+ upper+"]"+".")
    val size = (upper - lower) + 1
    (lower + (nextInt() % size).abs).toShort
  }
  
  def nextBetween(lower: Int, upper: Int): Int = {
    val size = (upper - lower) + 1
    if (upper < lower || size < 0)
      throw new IllegalArgumentException("Invalid bounds"+" "+"["+ lower +", "+ upper+"]"+".")
    lower + (nextInt() % size).abs
  }
  
  def nextBetween(lower: Long, upper: Long): Long = {
    val size = (upper - lower) + 1L
    if (upper < lower || size < 0L)
      throw new IllegalArgumentException("Invalid bounds"+" "+"["+ lower +", "+ upper+"]"+".")
    lower + (nextLong() % size).abs
  }
  
  def nextBetween(lower: Float, upper: Float): Float = {
    val size = upper - lower
    if (size.isNaN || size.isInfinite || size < 0.0F)
      throw new IllegalArgumentException("Invalid bounds"+" "+"["+ lower +", "+ upper+")"+".")
    lower + nextFloat() * size
  }
  
  def nextBetween(lower: Double, upper: Double): Double = {
    val size = upper - lower
    if (size.isNaN || size.isInfinite || size < 0.0)
      throw new IllegalArgumentException("Invalid bounds"+" "+"["+ lower +", "+ upper+")"+".")
    lower + nextDouble() * size
  }
}
