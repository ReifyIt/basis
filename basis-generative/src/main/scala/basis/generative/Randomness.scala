/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.generative

import basis.util._

abstract class Randomness {
  def asByte: Arbitrary[Byte] = new ByteView
  
  def asByte(lower: Byte, upper: Byte): Arbitrary[Byte] = new ByteRangeView(lower, upper)
  
  def asPositiveByte: Arbitrary[Byte] = new PositiveByteView
  
  def asShort: Arbitrary[Short] = new ShortView
  
  def asShort(lower: Short, upper: Short): Arbitrary[Short] = new ShortRangeView(lower, upper)
  
  def asPositiveShort: Arbitrary[Short] = new PositiveShortView
  
  def asInt: Arbitrary[Int] = new IntView
  
  def asInt(lower: Int, upper: Int): Arbitrary[Int] = new IntRangeView(lower, upper)
  
  def asPositiveInt: Arbitrary[Int] = new PositiveIntView
  
  def asLong: Arbitrary[Long] = new LongView
  
  def asLong(lower: Long, upper: Long): Arbitrary[Long] = new LongRangeView(lower, upper)
  
  def asPositiveLong: Arbitrary[Long] = new PositiveLongView
  
  def asFloat: Arbitrary[Float] = new FloatView
  
  def asFloat(lower: Float, upper: Float): Arbitrary[Float] = new FloatRangeView(lower, upper)
  
  def asDouble: Arbitrary[Double] = new DoubleView
  
  def asDouble(lower: Double, upper: Double): Arbitrary[Double] = new DoubleRangeView(lower, upper)
  
  def asBoolean: Arbitrary[Boolean] = new BooleanView
  
  def choose(lower: Byte, upper: Byte): Byte = {
    if (upper < lower) throw new IllegalArgumentException(s"Invalid range ($lower, $upper).")
    val size = (upper - lower) + 1
    (lower + (toInt() % size).abs).toByte
  }
  
  def choose(lower: Short, upper: Short): Short = {
    if (upper < lower) throw new IllegalArgumentException(s"Invalid range ($lower, $upper).")
    val size = (upper - lower) + 1
    (lower + (toInt() % size).abs).toShort
  }
  
  def choose(lower: Int, upper: Int): Int = {
    val size = (upper - lower) + 1
    if (upper < lower || size < 0) throw new IllegalArgumentException(s"Invalid range ($lower, $upper).")
    lower + (toInt() % size).abs
  }
  
  def choose(lower: Long, upper: Long): Long = {
    val size = (upper - lower) + 1L
    if (upper < lower || size < 0L) throw new IllegalArgumentException(s"Invalid range ($lower, $upper).")
    lower + (toLong() % size).abs
  }
  
  def choose(lower: Float, upper: Float): Float = {
    val size = upper - lower
    if (size.isNaN || size.isInfinite || size < 0.0F)
      throw new IllegalArgumentException(s"Invalid range ($lower, $upper).")
    lower + toFloat() * size
  }
  
  def choose(lower: Double, upper: Double): Double = {
    val size = upper - lower
    if (size.isNaN || size.isInfinite || size < 0.0)
      throw new IllegalArgumentException(s"Invalid range ($lower, $upper).")
    lower + toDouble() * size
  }
  
  def toByte(): Byte
  
  def toShort(): Short
  
  def toInt(): Int
  
  def toLong(): Long
  
  def toFloat(): Float
  
  def toDouble(): Double
  
  def toBoolean(): Boolean
  
  private final class ByteView extends Arbitrary[Byte] {
    override def apply(): Byte = toByte()
    override def toString: String = Randomness.this.toString +"."+"asByte"
  }
  
  private final class ByteRangeView(lower: Byte, upper: Byte) extends Arbitrary[Byte] {
    if (upper < lower) throw new IllegalArgumentException(s"Invalid range ($lower, $upper).")
    private[this] val size: Int = (upper - lower) + 1
    override def apply(): Byte = (lower + (toInt() % size).abs).toByte
    override def toString: String = Randomness.this.toString +"."+"asByte"+"("+ lower +", "+ upper +")"
  }
  
  private final class PositiveByteView extends Arbitrary[Byte] {
    override def apply(): Byte = (toByte() >>> 1).toByte
    override def toString: String = Randomness.this.toString +"."+"asPositiveByte"
  }
  
  private final class ShortView extends Arbitrary[Short] {
    override def apply(): Short = toShort()
    override def toString: String = Randomness.this.toString +"."+"asShort"
  }
  
  private final class ShortRangeView(lower: Short, upper: Short) extends Arbitrary[Short] {
    if (upper < lower) throw new IllegalArgumentException(s"Invalid range ($lower, $upper).")
    private[this] val size: Int = (upper - lower) + 1
    override def apply(): Short = (lower + (toInt() % size).abs).toShort
    override def toString: String = Randomness.this.toString +"."+"asShort"+"("+ lower +", "+ upper +")"
  }
  
  private final class PositiveShortView extends Arbitrary[Short] {
    override def apply(): Short = (toShort >>> 1).toShort
    override def toString: String = Randomness.this.toString +"."+"asPositiveShort"
  }
  
  private final class IntView extends Arbitrary[Int] {
    override def apply(): Int = toInt()
    override def toString: String = Randomness.this.toString +"."+"asInt"
  }
  
  private final class IntRangeView(lower: Int, upper: Int) extends Arbitrary[Int] {
    private[this] val size: Int = (upper - lower) + 1
    if (upper < lower || size < 0) throw new IllegalArgumentException(s"Invalid range ($lower, $upper).")
    override def apply(): Int = lower + (toInt() % size).abs
    override def toString: String = Randomness.this.toString +"."+"asInt"+"("+ lower +", "+ upper +")"
  }
  
  private final class PositiveIntView extends Arbitrary[Int] {
    override def apply(): Int = toInt() >>> 1
    override def toString: String = Randomness.this.toString +"."+"asPositiveInt"
  }
  
  private final class LongView extends Arbitrary[Long] {
    override def apply(): Long = toLong()
    override def toString: String = Randomness.this.toString +"."+"asLong"
  }
  
  private final class LongRangeView(lower: Long, upper: Long) extends Arbitrary[Long] {
    private[this] val size: Long = (upper - lower) + 1L
    if (upper < lower || size < 0L) throw new IllegalArgumentException(s"Invalid range ($lower, $upper).")
    override def apply(): Long = lower + (toLong() % size).abs
    override def toString: String = Randomness.this.toString +"."+"asLong"+"("+ lower +", "+ upper +")"
  }
  
  private final class PositiveLongView extends Arbitrary[Long] {
    override def apply(): Long = toLong() >>> 1
    override def toString: String = Randomness.this.toString +"."+"asPositiveLong"
  }
  
  private final class FloatView extends Arbitrary[Float] {
    override def apply(): Float = toFloat()
    override def toString: String = Randomness.this.toString +"."+"asFloat"
  }
  
  private final class FloatRangeView(lower: Float, upper: Float) extends Arbitrary[Float] {
    private[this] val size: Float = upper - lower
    if (size.isNaN || size.isInfinite || size < 0.0F)
      throw new IllegalArgumentException(s"Invalid range ($lower, $upper).")
    override def apply(): Float = lower + toFloat() * size
    override def toString: String = Randomness.this.toString +"."+"asFloat"+"("+ lower +", "+ upper +")"
  }
  
  private final class DoubleView extends Arbitrary[Double] {
    override def apply(): Double = toDouble()
    override def toString: String = Randomness.this.toString +"."+"asDouble"
  }
  
  private final class DoubleRangeView(lower: Double, upper: Double) extends Arbitrary[Double] {
    private[this] val size: Double = upper - lower
    if (size.isNaN || size.isInfinite || size < 0.0)
      throw new IllegalArgumentException(s"Invalid range ($lower, $upper).")
    override def apply(): Double = lower + toDouble() * size
    override def toString: String = Randomness.this.toString +"."+"asDouble"+"("+ lower +", "+ upper +")"
  }
  
  private final class BooleanView extends Arbitrary[Boolean] {
    override def apply(): Boolean = toBoolean()
    override def toString: String = Randomness.this.toString +"."+"asBoolean"
  }
}
