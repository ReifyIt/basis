/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.generators

import basis.util._

import scala.annotation.implicitNotFound

@implicitNotFound("No available source of randomness.")
abstract class Randomness {
  def nextByte(): Byte
  
  def nextShort(): Short
  
  def nextInt(): Int
  
  def nextLong(): Long
  
  def nextFloat(): Float
  
  def nextDouble(): Double
  
  def nextBoolean(): Boolean
  
  def nextBetween(lower: Byte, upper: Byte): Byte = {
    if (upper < lower) throw new IllegalArgumentException(s"Invalid bounds [$lower, $upper].")
    val size = (upper - lower) + 1
    (lower + (nextInt() % size).abs).toByte
  }
  
  def nextBelow(upper: Byte): Byte = {
    if (upper < 0) throw new IllegalArgumentException(s"Invalid upper bound ($upper).")
    (nextInt() % upper).abs.toByte
  }
  
  def nextBetween(lower: Short, upper: Short): Short = {
    if (upper < lower) throw new IllegalArgumentException(s"Invalid bounds [$lower, $upper].")
    val size = (upper - lower) + 1
    (lower + (nextInt() % size).abs).toShort
  }
  
  def nextBelow(upper: Short): Short = {
    if (upper < 0) throw new IllegalArgumentException(s"Invalid upper bound ($upper).")
    (nextInt() % upper).abs.toShort
  }
  
  def nextBetween(lower: Int, upper: Int): Int = {
    val size = (upper - lower) + 1
    if (upper < lower || size < 0) throw new IllegalArgumentException(s"Invalid bounds [$lower, $upper].")
    lower + (nextInt() % size).abs
  }
  
  def nextBelow(upper: Int): Int = {
    if (upper < 0) throw new IllegalArgumentException(s"Invalid upper bound ($upper).")
    (nextInt() % upper).abs
  }
  
  def nextBetween(lower: Long, upper: Long): Long = {
    val size = (upper - lower) + 1L
    if (upper < lower || size < 0L) throw new IllegalArgumentException(s"Invalid bounds [$lower, $upper].")
    lower + (nextLong() % size).abs
  }
  
  def nextBelow(upper: Long): Long = {
    if (upper < 0L) throw new IllegalArgumentException(s"Invalid upper bound ($upper).")
    (nextLong() % upper).abs
  }
  
  def nextBetween(lower: Float, upper: Float): Float = {
    val size = upper - lower
    if (size.isNaN || size.isInfinite || size < 0.0F)
      throw new IllegalArgumentException(s"Invalid bounds [$lower, $upper].")
    lower + nextFloat() * size
  }
  
  def nextBelow(upper: Float): Float = {
    if (upper.isNaN || upper.isInfinite || upper < 0.0F)
      throw new IllegalArgumentException(s"Invalid upper bound ($upper).")
    nextFloat() * upper
  }
  
  def nextBetween(lower: Double, upper: Double): Double = {
    val size = upper - lower
    if (size.isNaN || size.isInfinite || size < 0.0)
      throw new IllegalArgumentException(s"Invalid bounds [$lower, $upper].")
    lower + nextDouble() * size
  }
  
  def nextBelow(upper: Double): Double = {
    if (upper.isNaN || upper.isInfinite || upper < 0.0)
      throw new IllegalArgumentException(s"Invalid upper bound ($upper).")
    nextDouble() * upper
  }
  
  def asByte: Arbitrary[Byte] = new ByteView
  
  def asPositiveByte: Arbitrary[Byte] = new PositiveByteView
  
  def asShort: Arbitrary[Short] = new ShortView
  
  def asPositiveShort: Arbitrary[Short] = new PositiveShortView
  
  def asInt: Arbitrary[Int] = new IntView
  
  def asPositiveInt: Arbitrary[Int] = new PositiveIntView
  
  def asLong: Arbitrary[Long] = new LongView
  
  def asPositiveLong: Arbitrary[Long] = new PositiveLongView
  
  def asFloat: Arbitrary[Float] = new FloatView
  
  def asDouble: Arbitrary[Double] = new DoubleView
  
  def asBoolean: Arbitrary[Boolean] = new BooleanView
  
  def between(lower: Byte, upper: Byte): Arbitrary[Byte] = new BoundedByteView(lower, upper)
  
  def between(lower: Short, upper: Short): Arbitrary[Short] = new BoundedShortView(lower, upper)
  
  def between(lower: Int, upper: Int): Arbitrary[Int] = new BoundedIntView(lower, upper)
  
  def between(lower: Long, upper: Long): Arbitrary[Long] = new BoundedLongView(lower, upper)
  
  def between(lower: Float, upper: Float): Arbitrary[Float] = new BoundedFloatView(lower, upper)
  
  def between(lower: Double, upper: Double): Arbitrary[Double] = new BoundedDoubleView(lower, upper)
  
  def below(upper: Byte): Arbitrary[Byte] = new LimitedByteView(upper)
  
  def below(upper: Short): Arbitrary[Short] = new LimitedShortView(upper)
  
  def below(upper: Int): Arbitrary[Int] = new LimitedIntView(upper)
  
  def below(upper: Long): Arbitrary[Long] = new LimitedLongView(upper)
  
  def below(upper: Float): Arbitrary[Float] = new LimitedFloatView(upper)
  
  def below(upper: Double): Arbitrary[Double] = new LimitedDoubleView(upper)
  
  private final class ByteView extends Arbitrary[Byte] {
    override def apply(): Byte = nextByte()
    override def toString: String = Randomness.this.toString +"."+"asByte"
  }
  
  private final class BoundedByteView(lower: Byte, upper: Byte) extends Arbitrary[Byte] {
    if (upper < lower) throw new IllegalArgumentException(s"Invalid bounds [$lower, $upper].")
    private[this] val size: Int = (upper - lower) + 1
    override def apply(): Byte = (lower + (nextInt() % size).abs).toByte
    override def toString: String = Randomness.this.toString +"."+"asByte"+"("+ lower +", "+ upper +")"
  }
  
  private final class LimitedByteView(upper: Byte) extends Arbitrary[Byte] {
    if (upper < 0) throw new IllegalArgumentException(s"Invalid upper bound ($upper).")
    override def apply(): Byte = (nextInt() % upper).abs.toByte
    override def toString: String = Randomness.this.toString +"."+"asByte"+"("+ upper +")"
  }
  
  private final class PositiveByteView extends Arbitrary[Byte] {
    override def apply(): Byte = (nextByte() >>> 1).toByte
    override def toString: String = Randomness.this.toString +"."+"asPositiveByte"
  }
  
  private final class ShortView extends Arbitrary[Short] {
    override def apply(): Short = nextShort()
    override def toString: String = Randomness.this.toString +"."+"asShort"
  }
  
  private final class BoundedShortView(lower: Short, upper: Short) extends Arbitrary[Short] {
    if (upper < lower) throw new IllegalArgumentException(s"Invalid bounds [$lower, $upper].")
    private[this] val size: Int = (upper - lower) + 1
    override def apply(): Short = (lower + (nextInt() % size).abs).toShort
    override def toString: String = Randomness.this.toString +"."+"asShort"+"("+ lower +", "+ upper +")"
  }
  
  private final class LimitedShortView(upper: Short) extends Arbitrary[Short] {
    if (upper < 0) throw new IllegalArgumentException(s"Invalid upper bound ($upper).")
    override def apply(): Short = (nextInt() % upper).abs.toShort
    override def toString: String = Randomness.this.toString +"."+"asShort"+"("+ upper +")"
  }
  
  private final class PositiveShortView extends Arbitrary[Short] {
    override def apply(): Short = (nextShort() >>> 1).toShort
    override def toString: String = Randomness.this.toString +"."+"asPositiveShort"
  }
  
  private final class IntView extends Arbitrary[Int] {
    override def apply(): Int = nextInt()
    override def toString: String = Randomness.this.toString +"."+"asInt"
  }
  
  private final class BoundedIntView(lower: Int, upper: Int) extends Arbitrary[Int] {
    private[this] val size: Int = (upper - lower) + 1
    if (upper < lower || size < 0) throw new IllegalArgumentException(s"Invalid bounds [$lower, $upper].")
    override def apply(): Int = lower + (nextInt() % size).abs
    override def toString: String = Randomness.this.toString +"."+"asInt"+"("+ lower +", "+ upper +")"
  }
  
  private final class LimitedIntView(upper: Int) extends Arbitrary[Int] {
    if (upper < 0) throw new IllegalArgumentException(s"Invalid upper bound ($upper).")
    override def apply(): Int = (nextInt() % upper).abs
    override def toString: String = Randomness.this.toString +"."+"asInt"+"("+ upper +")"
  }
  
  private final class PositiveIntView extends Arbitrary[Int] {
    override def apply(): Int = nextInt() >>> 1
    override def toString: String = Randomness.this.toString +"."+"asPositiveInt"
  }
  
  private final class LongView extends Arbitrary[Long] {
    override def apply(): Long = nextLong()
    override def toString: String = Randomness.this.toString +"."+"asLong"
  }
  
  private final class BoundedLongView(lower: Long, upper: Long) extends Arbitrary[Long] {
    private[this] val size: Long = (upper - lower) + 1L
    if (upper < lower || size < 0L) throw new IllegalArgumentException(s"Invalid bounds [$lower, $upper].")
    override def apply(): Long = lower + (nextLong() % size).abs
    override def toString: String = Randomness.this.toString +"."+"asLong"+"("+ lower +", "+ upper +")"
  }
  
  private final class LimitedLongView(upper: Long) extends Arbitrary[Long] {
    if (upper < 0L) throw new IllegalArgumentException(s"Invalid upper bound ($upper).")
    override def apply(): Long = (nextLong() % upper).abs
    override def toString: String = Randomness.this.toString +"."+"asLong"+"("+ upper +")"
  }
  
  private final class PositiveLongView extends Arbitrary[Long] {
    override def apply(): Long = nextLong() >>> 1
    override def toString: String = Randomness.this.toString +"."+"asPositiveLong"
  }
  
  private final class FloatView extends Arbitrary[Float] {
    override def apply(): Float = nextFloat()
    override def toString: String = Randomness.this.toString +"."+"asFloat"
  }
  
  private final class BoundedFloatView(lower: Float, upper: Float) extends Arbitrary[Float] {
    private[this] val size: Float = upper - lower
    if (size.isNaN || size.isInfinite || size < 0.0F)
      throw new IllegalArgumentException(s"Invalid bounds [$lower, $upper].")
    override def apply(): Float = lower + nextFloat() * size
    override def toString: String = Randomness.this.toString +"."+"asFloat"+"("+ lower +", "+ upper +")"
  }
  
  private final class LimitedFloatView(upper: Float) extends Arbitrary[Float] {
    if (upper.isNaN || upper.isInfinite || upper < 0.0F)
      throw new IllegalArgumentException(s"Invalid upper bound ($upper).")
    override def apply(): Float = nextFloat() * upper
    override def toString: String = Randomness.this.toString +"."+"asFloat"+"("+ upper +")"
  }
  
  private final class DoubleView extends Arbitrary[Double] {
    override def apply(): Double = nextDouble()
    override def toString: String = Randomness.this.toString +"."+"asDouble"
  }
  
  private final class BoundedDoubleView(lower: Double, upper: Double) extends Arbitrary[Double] {
    private[this] val size: Double = upper - lower
    if (size.isNaN || size.isInfinite || size < 0.0)
      throw new IllegalArgumentException(s"Invalid bounds [$lower, $upper].")
    override def apply(): Double = lower + nextDouble() * size
    override def toString: String = Randomness.this.toString +"."+"asDouble"+"("+ lower +", "+ upper +")"
  }
  
  private final class LimitedDoubleView(upper: Double) extends Arbitrary[Double] {
    if (upper.isNaN || upper.isInfinite || upper < 0.0)
      throw new IllegalArgumentException(s"Invalid upper bound ($upper).")
    override def apply(): Double = nextDouble() * upper
    override def toString: String = Randomness.this.toString +"."+"asDouble"+"("+ upper +")"
  }
  
  private final class BooleanView extends Arbitrary[Boolean] {
    override def apply(): Boolean = nextBoolean()
    override def toString: String = Randomness.this.toString +"."+"asBoolean"
  }
}
