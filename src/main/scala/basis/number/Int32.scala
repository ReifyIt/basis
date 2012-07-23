/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.number

import basis.algebra._

/** A 32-bit two's complement integer value.
  * 
  * @author Chris Sachs
  * 
  * @define value   integer
  */
@inline final class Int32(val value: Int) extends AnyVal with Int32.Element {
  @inline override def + (that: Int32): Int32 = new Int32(value + that.value)
  
  @inline override def unary_- : Int32 = new Int32(-value)
  
  @inline override def - (that: Int32): Int32 = new Int32(value - that.value)
  
  @inline override def * (that: Int32): Int32 = new Int32(value * that.value)
  
  @inline override def abs: Int32 = new Int32(java.lang.Math.abs(value))
  
  @inline override def min(that: Int32): Int32 = new Int32(java.lang.Math.min(value, that.value))
  
  @inline override def max(that: Int32): Int32 = new Int32(java.lang.Math.max(value, that.value))
  
  @inline override def < (that: Int32): Boolean = value < that.value
  
  @inline override def <= (that: Int32): Boolean = value <= that.value
  
  @inline override def > (that: Int32): Boolean = value > that.value
  
  @inline override def >= (that: Int32): Boolean = value >= that.value
  
  @inline def gcd(that: Int32): Int32 = {
    var a = java.lang.Math.abs(value)
    var b = java.lang.Math.abs(that.value)
    while (b != 0) {
      val t = b
      b = a % b
      a = t
    }
    new Int32(a)
  }
  
  @inline def toInt: Int = value
  
  @inline def toLong: Long = value.toLong
  
  @inline def toFloat: Float = value.toFloat
  
  @inline def toDouble: Double = value.toDouble
  
  @inline override def equals(other: Any): Boolean = other match {
    case that: Int32 => value == that.value
    case _ => false
  }
  
  @inline override def hashCode: Int = value.##
  
  @inline override def toString: String = java.lang.Integer.toString(value)
}

/** A ring of 32-bit two's complement integer values. */
object Int32 extends OrderedRing {
  import scala.language.implicitConversions
  
  override type Value = Int32
  
  @inline override def zero: Int32 = new Int32(0)
  
  @inline override def unit: Int32 = new Int32(1)
  
  @inline def apply(value: Int): Int32 = new Int32(value)
  
  @inline implicit def box(value: Int): Int32 = new Int32(value)
  
  @inline implicit def unbox(integer: Int32): Int = integer.value
  
  override def toString: String = "Int32"
}
