/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

import language.implicitConversions

final class Real(val value: Double) extends AnyVal with Equals with RealField with RealVector {
  override type Vector = Real
  override type Scalar = Real
  
  @inline override def Vector = Real
  
  @inline override def N: Int = 1
  
  @inline override def apply(i: Int): Real =
    if (i == 0) this else throw new IndexOutOfBoundsException(i.toString)
  
  @inline override def + (that: Real): Real = new Real(value + that.value)
  
  @inline override def unary_- : Real = new Real(-value)
  
  @inline override def - (that: Real): Real = new Real(value - that.value)
  
  @inline override def * (that: Real): Real = new Real(value * that.value)
  
  @inline override def :* (that: Real): Real = new Real(value * that.value)
  
  @inline override def *: (that: Real): Real = new Real(that.value * value)
  
  @inline override def ⋅ (that: Real): Real = new Real(value * that.value)
  
  @inline override def inverse: Real = new Real(1.0 / value)
  
  @inline override def / (that: Real): Real = new Real(value / that.value)
  
  @inline override def pow(that: Real): Real = new Real(java.lang.Math.pow(value, that.value))
  
  @inline override def sqrt: Real = new Real(java.lang.Math.sqrt(value))
  
  @inline override def abs: Real = new Real(java.lang.Math.abs(value))
  
  @inline override def min(that: Real): Real = new Real(java.lang.Math.min(value, that.value))
  
  @inline override def max(that: Real): Real = new Real(java.lang.Math.max(value, that.value))
  
  @inline override def < (that: Real): Boolean = value < that.value
  
  @inline override def <= (that: Real): Boolean = value <= that.value
  
  @inline override def > (that: Real): Boolean = value > that.value
  
  @inline override def >= (that: Real): Boolean = value >= that.value
  
  @inline def toInt: Int = value.toInt
  
  @inline def toLong: Long = value.toLong
  
  @inline def toFloat: Float = value.toFloat
  
  @inline def toDouble: Double = value
  
  @inline override def equals(other: Any): Boolean = other match {
    case that: Real => value == that.value
    case _ => false
  }
  
  @inline override def hashCode: Int = basis.util.MurmurHash.hash(value)
  
  @inline override def toString: String = java.lang.Double.toString(value)
}

object Real extends RealField.Scalar with RealField.Space with RealVector.Space {
  override type Vector = Real
  override type Scalar = Real
  
  @inline override def zero: Real = new Real(0.0)
  
  @inline override def unit: Real = new Real(1.0)
  
  @inline override def N: Int = 1
  
  override def apply(coords: TraversableOnce[Real]): Real = {
    val xs = coords.toSeq
    if (xs.length == 1) xs.head else throw new DimensionException
  }
  
  override def apply(coords: Array[Double]): Real =
    if (coords.length == 1) coords(0) else throw new DimensionException
  
  @inline def apply(value: Double): Real = new Real(value)
  
  @inline implicit def box(value: Double): Real = new Real(value)
  
  @inline implicit def unbox(real: Real): Double = real.value
  
  override def toString: String = "Real"
}
