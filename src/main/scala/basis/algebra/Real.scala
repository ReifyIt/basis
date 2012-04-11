/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.util.MurmurHash._

final class Real(private val value: Double)
  extends OrderedRingElement[Real]
    with CompleteFieldElement[Real] {
  
  def Space = Real
  
  def + (that: Real): Real = new Real(value + that.value)
  
  def unary_- : Real = new Real(-value)
  
  def - (that: Real): Real = new Real(value - that.value)
  
  def * (that: Real): Real = new Real(value * that.value)
  
  def :* (that: Real): Real = new Real(value * that.value)
  
  def *: (that: Real): Real = new Real(that.value * value)
  
  def inverse: Real = new Real(1.0 / value)
  
  def / (that: Real): Real = new Real(value / that.value)
  
  def pow(that: Real): Real = new Real(math.pow(value, that.value))
  
  def pow(n: Int): Real = new Real(math.pow(value, n))
  
  def sqrt: Real = new Real(math.sqrt(value))
  
  def abs: Real = if (value >= 0.0) this else -this
  
  def min(that: Real): Real = if (value <= that.value) this else that
  
  def max(that: Real): Real = if (value >= that.value) this else that
  
  def < (that: Real): Boolean = value < that.value
  
  def <= (that: Real): Boolean = value <= that.value
  
  def >= (that: Real): Boolean = value >= that.value
  
  def > (that: Real): Boolean = value > that.value
  
  override def equals(other: Any): Boolean = other match {
    case that: Real => value == that.value
    case _ => false
  }
  
  override def hashCode: Int = hash(value)
  
  override def toString: String = value.toString
  
  def toInt: Int = value.toInt
  
  def toLong: Long = value.toLong
  
  def toFloat: Float = value.toFloat
  
  def toDouble: Double = value
}

object Real extends OrderedRing with CompleteField {
  type Scalar = Real
  
  val zero: Real = new Real(0.0)
  
  val unit: Real = new Real(1.0)
  
  def apply(value: Double): Real = new Real(value)
  
  def unapply(real: Real): Some[Double] = Some(real.toDouble)
  
  implicit def box(value: Double): Real = new Real(value)
  
  implicit def unbox(real: Real): Double = real.toDouble
  
  override def toString: String = "Real"
}
