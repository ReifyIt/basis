/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.generative

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
  
  private final class ByteRangeView(l: Byte, u: Byte) extends Arbitrary[Byte] {
    if (u < l) throw new IllegalArgumentException(s"Invalid range ($l, $u).")
    private[this] val d: Int = (u - l) + 1
    override def apply(): Byte = (l + java.lang.Math.abs(toInt() % d)).toByte
    override def toString: String = Randomness.this.toString +"."+"asByte"+"("+ l +", "+ u +")"
  }
  
  private final class PositiveByteView extends Arbitrary[Byte] {
    override def apply(): Byte = (toByte() >>> 1).toByte
    override def toString: String = Randomness.this.toString +"."+"asPositiveByte"
  }
  
  private final class ShortView extends Arbitrary[Short] {
    override def apply(): Short = toShort()
    override def toString: String = Randomness.this.toString +"."+"asShort"
  }
  
  private final class ShortRangeView(l: Short, u: Short) extends Arbitrary[Short] {
    if (u < l) throw new IllegalArgumentException(s"Invalid range ($l, $u).")
    private[this] val d: Int = (u - l) + 1
    override def apply(): Short = (l + java.lang.Math.abs(toInt() % d)).toShort
    override def toString: String = Randomness.this.toString +"."+"asShort"+"("+ l +", "+ u +")"
  }
  
  private final class PositiveShortView extends Arbitrary[Short] {
    override def apply(): Short = (toShort >>> 1).toShort
    override def toString: String = Randomness.this.toString +"."+"asPositiveShort"
  }
  
  private final class IntView extends Arbitrary[Int] {
    override def apply(): Int = toInt()
    override def toString: String = Randomness.this.toString +"."+"asInt"
  }
  
  private final class IntRangeView(l: Int, u: Int) extends Arbitrary[Int] {
    private[this] val d: Int = (u - l) + 1
    if (u < l || d < 0) throw new IllegalArgumentException(s"Invalid range ($l, $u).")
    override def apply(): Int = l + java.lang.Math.abs(toInt() % d)
    override def toString: String = Randomness.this.toString +"."+"asInt"+"("+ l +", "+ u +")"
  }
  
  private final class PositiveIntView extends Arbitrary[Int] {
    override def apply(): Int = toInt() >>> 1
    override def toString: String = Randomness.this.toString +"."+"asPositiveInt"
  }
  
  private final class LongView extends Arbitrary[Long] {
    override def apply(): Long = toLong()
    override def toString: String = Randomness.this.toString +"."+"asLong"
  }
  
  private final class LongRangeView(l: Long, u: Long) extends Arbitrary[Long] {
    private[this] val d: Long = (u - l) + 1L
    if (u < l || d < 0L) throw new IllegalArgumentException(s"Invalid range ($l, $u).")
    override def apply(): Long = l + java.lang.Math.abs(toLong() % d)
    override def toString: String = Randomness.this.toString +"."+"asLong"+"("+ l +", "+ u +")"
  }
  
  private final class PositiveLongView extends Arbitrary[Long] {
    override def apply(): Long = toLong() >>> 1
    override def toString: String = Randomness.this.toString +"."+"asPositiveLong"
  }
  
  private final class FloatView extends Arbitrary[Float] {
    override def apply(): Float = toFloat()
    override def toString: String = Randomness.this.toString +"."+"asFloat"
  }
  
  private final class FloatRangeView(l: Float, u: Float) extends Arbitrary[Float] {
    private[this] val d: Float = u - l
    if (java.lang.Float.isNaN(d) || java.lang.Float.isInfinite(d) || d < 0.0F)
      throw new IllegalArgumentException(s"Invalid range ($l, $u).")
    override def apply(): Float = l + toFloat() * d
    override def toString: String = Randomness.this.toString +"."+"asFloat"+"("+ l +", "+ u +")"
  }
  
  private final class DoubleView extends Arbitrary[Double] {
    override def apply(): Double = toDouble()
    override def toString: String = Randomness.this.toString +"."+"asDouble"
  }
  
  private final class DoubleRangeView(l: Double, u: Double) extends Arbitrary[Double] {
    private[this] val d: Double = u - l
    if (java.lang.Double.isNaN(d) || java.lang.Double.isInfinite(d) || d < 0.0)
      throw new IllegalArgumentException(s"Invalid range ($l, $u).")
    override def apply(): Double = l + toDouble() * d
    override def toString: String = Randomness.this.toString +"."+"asDouble"+"("+ l +", "+ u +")"
  }
  
  private final class BooleanView extends Arbitrary[Boolean] {
    override def apply(): Boolean = toBoolean()
    override def toString: String = Randomness.this.toString +"."+"asBoolean"
  }
}
