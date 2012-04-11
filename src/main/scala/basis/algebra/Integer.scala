/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.util.MurmurHash._

final class Integer(private val value: Long)
  extends OrderedRingElement[Integer] {
  
  def Space = Integer
  
  def + (that: Integer): Integer = new Integer(value + that.value)
  
  def unary_- : Integer = new Integer(-value)
  
  def - (that: Integer): Integer = new Integer(value - that.value)
  
  def * (that: Integer): Integer = new Integer(value * that.value)
  
  def :* (that: Integer): Integer = new Integer(value * that.value)
  
  def *: (that: Integer): Integer = new Integer(that.value * value)
  
  def pow(n: Long): Integer = new Integer(math.pow(value, n).toLong)
  
  def gcd(that: Integer): Integer = {
    var a = math.abs(value)
    var b = math.abs(that.value)
    while (b != 0L) {
      val t = b
      b = a % b
      a = t
    }
    new Integer(a)
  }
  
  def abs: Integer = if (value >= 0L) this else -this
  
  def min(that: Integer): Integer = if (value <= that.value) this else that
  
  def max(that: Integer): Integer = if (value >= that.value) this else that
  
  def < (that: Integer): Boolean = value < that.value
  
  def <= (that: Integer): Boolean = value <= that.value
  
  def >= (that: Integer): Boolean = value >= that.value
  
  def > (that: Integer): Boolean = value > that.value
  
  override def equals(other: Any): Boolean = other match {
    case that: Integer => value == that.value
    case _ => false
  }
  
  override def hashCode: Int = hash(value)
  
  override def toString: String = value.toString
  
  def toInt: Int = value.toInt
  
  def toLong: Long = value
  
  def toFloat: Float = value.toFloat
  
  def toDouble: Double = value.toDouble
}

object Integer extends OrderedRing {
  type Scalar = Integer
  
  val zero: Integer = new Integer(0L)
  
  val unit: Integer = new Integer(1L)
  
  def apply(value: Long): Integer = new Integer(value)
  
  def unapply(integer: Integer): Some[Long] = Some(integer.value)
  
  implicit def box(value: Long): Integer = new Integer(value)
  
  implicit def unbox(integer: Integer): Long = integer.value
  
  override def toString: String = "Integer"
}
