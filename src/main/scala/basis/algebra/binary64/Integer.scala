/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

import language.implicitConversions

final class Integer(val value: Long) extends AnyVal with Equals with OrderedRing with IntegerVector {
  override type Vector = Integer
  override type Scalar = Integer
  
  @inline override def Vector = Integer
  
  @inline override def N: Int = 1
  
  @inline override def apply(i: Int): Integer =
    if (i == 0) this else throw new IndexOutOfBoundsException(i.toString)
  
  @inline override def + (that: Integer): Integer = new Integer(value + that.value)
  
  @inline override def unary_- : Integer = new Integer(-value)
  
  @inline override def - (that: Integer): Integer = new Integer(value - that.value)
  
  @inline override def * (that: Integer): Integer = new Integer(value * that.value)
  
  @inline override def :* (that: Integer): Integer = new Integer(value * that.value)
  
  @inline override def *: (that: Integer): Integer = new Integer(that.value * value)
  
  @inline override def ⋅ (that: Integer): Integer = new Integer(value * that.value)
  
  @inline def gcd(that: Integer): Integer = {
    var a = java.lang.Math.abs(value)
    var b = java.lang.Math.abs(that.value)
    while (b != 0L) {
      val t = b
      b = a % b
      a = t
    }
    new Integer(a)
  }
  
  @inline override def abs: Integer = new Integer(java.lang.Math.abs(value))
  
  @inline override def min(that: Integer): Integer = new Integer(java.lang.Math.min(value, that.value))
  
  @inline override def max(that: Integer): Integer = new Integer(java.lang.Math.max(value, that.value))
  
  @inline override def < (that: Integer): Boolean = value < that.value
  
  @inline override def <= (that: Integer): Boolean = value <= that.value
  
  @inline override def > (that: Integer): Boolean = value > that.value
  
  @inline override def >= (that: Integer): Boolean = value >= that.value
  
  @inline def toInt: Int = value.toInt
  
  @inline def toLong: Long = value
  
  @inline def toFloat: Float = value.toFloat
  
  @inline def toDouble: Double = value.toDouble
  
  @inline override def equals(other: Any): Boolean = other match {
    case that: Integer => value == that.value
    case _ => false
  }
  
  @inline override def hashCode: Int = basis.util.MurmurHash.hash(value)
  
  @inline override def toString: String = java.lang.Long.toString(value)
}

object Integer extends OrderedRing.Scalar with OrderedRing.Space with IntegerVector.Space {
  override type Vector = Integer
  override type Scalar = Integer
  
  @inline override def zero: Integer = new Integer(0L)
  
  @inline override def unit: Integer = new Integer(1L)
  
  @inline override def N: Int = 1
  
  override def apply(coords: TraversableOnce[Integer]): Integer = {
    val xs = coords.toSeq
    if (xs.length == 1) xs.head else throw new DimensionException
  }
  
  override def apply(coords: Array[Long]): Integer =
    if (coords.length == 1) coords(0) else throw new DimensionException
  
  @inline def apply(value: Long): Integer = new Integer(value)
  
  @inline implicit def box(value: Long): Integer = new Integer(value)
  
  @inline implicit def unbox(integer: Integer): Long = integer.value
  
  override def toString: String = "Integer"
}
