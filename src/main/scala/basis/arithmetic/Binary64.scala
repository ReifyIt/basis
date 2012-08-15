/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.arithmetic

import basis.algebra._

/** A double-precision floating-point value.
  * 
  * @author Chris Sachs
  * 
  * @define value   real
  */
@inline final class Binary64(val value: Double) extends AnyVal with Binary64.Element {
  @inline override def + (that: Binary64): Binary64 = new Binary64(value + that.value)
  
  @inline override def unary_- : Binary64 = new Binary64(-value)
  
  @inline override def - (that: Binary64): Binary64 = new Binary64(value - that.value)
  
  @inline override def * (that: Binary64): Binary64 = new Binary64(value * that.value)
  
  @inline override def inverse: Binary64 = new Binary64(1.0 / value)
  
  @inline override def / (that: Binary64): Binary64 = new Binary64(value / that.value)
  
  @inline override def pow(that: Binary64): Binary64 = new Binary64(java.lang.Math.pow(value, that.value))
  
  @inline override def sqrt: Binary64 = new Binary64(java.lang.Math.sqrt(value))
  
  @inline override def abs: Binary64 = new Binary64(java.lang.Math.abs(value))
  
  @inline override def min(that: Binary64): Binary64 = new Binary64(java.lang.Math.min(value, that.value))
  
  @inline override def max(that: Binary64): Binary64 = new Binary64(java.lang.Math.max(value, that.value))
  
  @inline override def < (that: Binary64): Boolean = value < that.value
  
  @inline override def <= (that: Binary64): Boolean = value <= that.value
  
  @inline override def > (that: Binary64): Boolean = value > that.value
  
  @inline override def >= (that: Binary64): Boolean = value >= that.value
  
  @inline def toInt: Int = value.toInt
  
  @inline def toLong: Long = value.toLong
  
  @inline def toFloat: Float = value.toFloat
  
  @inline def toDouble: Double = value
  
  @inline override def equals(other: Any): Boolean = other match {
    case that: Binary64 => value == that.value
    case _ => false
  }
  
  @inline override def hashCode: Int = value.##
  
  @inline override def toString: String = java.lang.Double.toString(value)
}

/** A real field of double-precision floating-point values. */
object Binary64 extends RealField {
  import scala.language.implicitConversions
  
  override type Value = Binary64
  
  @inline override def zero: Binary64 = new Binary64(0.0)
  
  @inline override def unit: Binary64 = new Binary64(1.0)
  
  @inline def apply(value: Double): Binary64 = new Binary64(value)
  
  @inline implicit def box(value: Double): Binary64 = new Binary64(value)
  
  @inline implicit def unbox(real: Binary64): Double = real.value
  
  override def toString: String = "Binary64"
}
