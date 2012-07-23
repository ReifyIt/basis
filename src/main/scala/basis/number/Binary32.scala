/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.number

import basis.algebra._

/** A single-precision floating-point value.
  * 
  * @author Chris Sachs
  * 
  * @define value   real
  */
@inline final class Binary32(val value: Float) extends AnyVal with Binary32.Element {
  @inline override def + (that: Binary32): Binary32 = new Binary32(value + that.value)
  
  @inline override def unary_- : Binary32 = new Binary32(-value)
  
  @inline override def - (that: Binary32): Binary32 = new Binary32(value - that.value)
  
  @inline override def * (that: Binary32): Binary32 = new Binary32(value * that.value)
  
  @inline override def inverse: Binary32 = new Binary32(1.0F / value)
  
  @inline override def / (that: Binary32): Binary32 = new Binary32(value / that.value)
  
  @inline override def pow(that: Binary32): Binary32 = new Binary32(java.lang.Math.pow(value, that.value).toFloat)
  
  @inline override def sqrt: Binary32 = new Binary32(java.lang.Math.sqrt(value).toFloat)
  
  @inline override def abs: Binary32 = new Binary32(java.lang.Math.abs(value))
  
  @inline override def min(that: Binary32): Binary32 = new Binary32(java.lang.Math.min(value, that.value))
  
  @inline override def max(that: Binary32): Binary32 = new Binary32(java.lang.Math.max(value, that.value))
  
  @inline override def < (that: Binary32): Boolean = value < that.value
  
  @inline override def <= (that: Binary32): Boolean = value <= that.value
  
  @inline override def > (that: Binary32): Boolean = value > that.value
  
  @inline override def >= (that: Binary32): Boolean = value >= that.value
  
  @inline def toInt: Int = value.toInt
  
  @inline def toLong: Long = value.toLong
  
  @inline def toFloat: Float = value
  
  @inline def toDouble: Double = value.toDouble
  
  @inline override def equals(other: Any): Boolean = other match {
    case that: Binary32 => value == that.value
    case _ => false
  }
  
  @inline override def hashCode: Int = value.##
  
  @inline override def toString: String = java.lang.Float.toString(value)
}

/** A real field of single-precision floating-point values. */
object Binary32 extends RealField {
  import scala.language.implicitConversions
  
  override type Value = Binary32
  
  @inline override def zero: Binary32 = new Binary32(0.0F)
  
  @inline override def unit: Binary32 = new Binary32(1.0F)
  
  @inline def apply(value: Float): Binary32 = new Binary32(value)
  
  @inline implicit def box(value: Float): Binary32 = new Binary32(value)
  
  @inline implicit def unbox(real: Binary32): Float = real.value
  
  override def toString: String = "Binary32"
}
