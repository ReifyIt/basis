//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.math
package binary64

/** A 64-bit two's complement integer value.
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    Integral
  *
  * @define element   value
  * @define point     value
  * @define vector    value
  * @define scalar    value
  */
final class Integer(val value: Long)
  extends AnyVal
  with Integer.OrderedRingElement
  with Integer.PointElement
  with Integer.VectorZN {

  override def dim: Int = 1

  override def apply(i: Int): Integer = {
    if (i != 0) throw new IndexOutOfBoundsException(i.toString)
    this
  }

  override def + (that: Integer): Integer =
    new Integer(value + that.value)

  override def unary_- : Integer =
    new Integer(-value)

  override def - (that: Integer): Integer =
    new Integer(value - that.value)

  override def * (that: Integer): Integer =
    new Integer(value * that.value)

  override def :* (that: Integer): Integer = this * that

  override def *: (that: Integer): Integer = that * this

  override def ∘ (that: Integer): Integer = this * that

  override def ⋅ (that: Integer): Integer = this * that

  override def abs: Integer =
    new Integer(java.lang.Math.abs(value))

  override def min(that: Integer): Integer =
    new Integer(java.lang.Math.min(value, that.value))

  override def max(that: Integer): Integer =
    new Integer(java.lang.Math.max(value, that.value))

  override def < (that: Integer): Boolean = value < that.value

  override def <= (that: Integer): Boolean = value <= that.value

  override def > (that: Integer): Boolean = value > that.value

  override def >= (that: Integer): Boolean = value >= that.value

  def gcd(that: Integer): Integer = {
    var a = java.lang.Math.abs(value)
    var b = java.lang.Math.abs(that.value)
    while (b != 0L) {
      val t = b
      b = a % b
      a = t
    }
    new Integer(a)
  }

  def toInt: Int = value.toInt

  def toLong: Long = value

  def toFloat: Float = value.toFloat

  def toDouble: Double = value.toDouble

  override def toString: String = java.lang.Long.toString(value)
}

/** An ordered ring of 64-bit two's complement integer values.
  * @group Integral */
object Integer extends OrderedRing with AffineSpace with ZN {
  override type Element = Integer

  override type Point = Integer

  override type Vector = Integer

  override val Vector: Integer.type = Integer

  override type Scalar = Integer

  override val Scalar: Integer.type = Integer

  override def dim: Int = 1

  override def origin: Integer = zero

  override val zero: Integer = new Integer(0L)

  override val unit: Integer = new Integer(1L)

  implicit def apply(value: Long): Integer = new Integer(value)

  override def apply(coords: Array[Long]): Integer = {
    if (coords.length != 1) throw new DimensionException
    new Integer(coords(0))
  }

  override def apply(coords: Array[Integer]): Integer = {
    if (coords.length != 1) throw new DimensionException
    coords(0)
  }

  override def toString: String = "Integer"
}
