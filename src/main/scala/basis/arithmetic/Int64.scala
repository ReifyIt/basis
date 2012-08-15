/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.arithmetic

import basis.algebra._

/** A 64-bit two's complement integer value.
  * 
  * @author Chris Sachs
  * 
  * @define value   integer
  */
@inline final class Int64(val value: Long) extends AnyVal with Int64.Element {
  @inline override def + (that: Int64): Int64 = new Int64(value + that.value)
  
  @inline override def unary_- : Int64 = new Int64(-value)
  
  @inline override def - (that: Int64): Int64 = new Int64(value - that.value)
  
  @inline override def * (that: Int64): Int64 = new Int64(value * that.value)
  
  @inline override def abs: Int64 = new Int64(java.lang.Math.abs(value))
  
  @inline override def min(that: Int64): Int64 = new Int64(java.lang.Math.min(value, that.value))
  
  @inline override def max(that: Int64): Int64 = new Int64(java.lang.Math.max(value, that.value))
  
  @inline override def < (that: Int64): Boolean = value < that.value
  
  @inline override def <= (that: Int64): Boolean = value <= that.value
  
  @inline override def > (that: Int64): Boolean = value > that.value
  
  @inline override def >= (that: Int64): Boolean = value >= that.value
  
  @inline def gcd(that: Int64): Int64 = {
    var a = java.lang.Math.abs(value)
    var b = java.lang.Math.abs(that.value)
    while (b != 0L) {
      val t = b
      b = a % b
      a = t
    }
    new Int64(a)
  }
  
  @inline def toInt: Int = value.toInt
  
  @inline def toLong: Long = value
  
  @inline def toFloat: Float = value.toFloat
  
  @inline def toDouble: Double = value.toDouble
  
  @inline override def equals(other: Any): Boolean = other match {
    case that: Int64 => value == that.value
    case _ => false
  }
  
  @inline override def hashCode: Int = value.##
  
  @inline override def toString: String = java.lang.Long.toString(value)
}

/** A ring of 64-bit two's complement integer values. */
object Int64 extends OrderedRing {
  import scala.language.implicitConversions
  
  override type Value = Int64
  
  @inline override def zero: Int64 = new Int64(0L)
  
  @inline override def unit: Int64 = new Int64(1L)
  
  @inline def apply(value: Long): Int64 = new Int64(value)
  
  @inline implicit def box(value: Long): Int64 = new Int64(value)
  
  @inline implicit def unbox(integer: Int64): Long = integer.value
  
  override def toString: String = "Int64"
}
