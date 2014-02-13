//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.math
package binary64

/** A double-precision floating-point value.
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    Real
  *
  * @define element   value
  * @define point     value
  * @define vector    value
  * @define matrix    value
  * @define scalar    value
  */
final class Real(val value: Double)
  extends AnyVal
  with Real.RealFieldElement
  with Real.PointElement
  with Real.VectorRN
  with Real.MatrixRMxN {

  override def Row: Real.type = Real
  override def Col: Real.type = Real

  override def dim: Int = 1

  override def apply(i: Int): Real = {
    if (i != 0) throw new IndexOutOfBoundsException(i.toString)
    this
  }

  override def apply(i: Int, j: Int): Real = {
    if (i != 0 || j != 0)
      throw new IndexOutOfBoundsException("row "+ i +", "+"col "+ j)
    this
  }

  override def row(i: Int): Real = {
    if (i != 0) throw new IndexOutOfBoundsException("row "+ i)
    this
  }

  override def col(j: Int): Real = {
    if (j != 0) throw new IndexOutOfBoundsException("col "+ j)
    this
  }

  override def + (that: Real): Real =
    new Real(value + that.value)

  override def unary_- : Real =
    new Real(-value)

  override def - (that: Real): Real =
    new Real(value - that.value)

  override def * (that: Real): Real =
    new Real(value * that.value)

  override def :* (that: Real): Real = this * that

  override def *: (that: Real): Real = that * this

  override def ∘ (that: Real): Real = this * that

  override def :⋅ (that: Real): Real = this * that

  override def ⋅: (that: Real): Real = that * this

  override def inverse: Real =
    new Real(1.0 / value)

  override def / (that: Real): Real =
    new Real(value / that.value)

  override def ⋅ (that: Real): Real = this * that

  override def transpose: Real = this

  override def det: Real = this

  override def trace: Real = this

  override def norm: Real = this

  override def normalized: Real = Real.unit

  override def pow(that: Real): Real =
    new Real(java.lang.Math.pow(value, that.value))

  override def sqrt: Real =
    new Real(java.lang.Math.sqrt(value))

  override def abs: Real =
    new Real(java.lang.Math.abs(value))

  override def min(that: Real): Real =
    new Real(java.lang.Math.min(value, that.value))

  override def max(that: Real): Real =
    new Real(java.lang.Math.max(value, that.value))

  override def < (that: Real): Boolean = value < that.value

  override def <= (that: Real): Boolean = value <= that.value

  override def > (that: Real): Boolean = value > that.value

  override def >= (that: Real): Boolean = value >= that.value

  def ceil: Real =
    new Real(java.lang.Math.ceil(value))

  def floor: Real =
    new Real(java.lang.Math.floor(value))

  def toInt: Int = value.toInt

  def toLong: Long = value.toLong

  def toFloat: Float = value.toFloat

  def toDouble: Double = value

  override def toString: String = java.lang.Double.toString(value)
}

/** A real field of double-precision floating-point values.
  * @group Real */
object Real extends RealField with AffineSpace with RN with RMxN {
  override type Element = Real

  override type Point = Real

  override type Vector = Real

  override val Vector: Real.type = Real

  override type Matrix = Real

  override type Transpose = Real

  override val Transpose: Real.type = Real

  override type Row = Real

  override val Row: Real.type = Real

  override type Col = Real

  override val Col: Real.type = Real

  override type Scalar = Real

  override val Scalar: Real.type = Real

  implicit override def ScalarTag: scala.reflect.ClassTag[Real] =
    scala.reflect.ClassTag(Predef.classOf[Real])

  override def dim: Int = 1

  override def origin: Real = zero

  override val zero: Real = new Real(0.0)

  override val unit: Real = new Real(1.0)

  implicit def coerce(that: Integer): Real = new Real(that.value.toDouble)

  implicit def apply(value: Double): Real = new Real(value)

  override def apply(coords: Array[Double]): Real = {
    if (coords.length != 1) throw new DimensionException
    new Real(coords(0))
  }

  override def apply(coords: Array[Real]): Real = {
    if (coords.length != 1) throw new DimensionException
    coords(0)
  }

  override def rows(rows: Real*): Real = {
    if (rows.length != 1) throw new DimensionException
    rows(0)
  }

  override def cols(cols: Real*): Real = {
    if (cols.length != 1) throw new DimensionException
    cols(0)
  }

  override def toString: String = "Real"
}
