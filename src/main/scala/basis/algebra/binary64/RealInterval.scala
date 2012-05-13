/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

import language.implicitConversions

final class RealInterval private (val lower: Real, val upper: Real) extends RealInterval.Element {
  override def isEmpty: Boolean = lower > upper
  
  override def contains(value: Real): Boolean =
    lower <= value && upper >= value
  
  override def + (that: RealInterval): RealInterval = {
    if (isEmpty || that.isEmpty) RealInterval.empty
    else new RealInterval(lower + that.lower, upper + that.upper)
  }
  
  override def unary_- : RealInterval = {
    if (isEmpty) RealInterval.empty
    else new RealInterval(-upper, -lower)
  }
  
  override def - (that: RealInterval): RealInterval = {
    if (isEmpty || that.isEmpty) RealInterval.empty
    else new RealInterval(lower - that.upper, upper - that.lower)
  }
  
  override def * (that: RealInterval): RealInterval = {
    if (isEmpty || that.isEmpty) RealInterval.empty
    else {
      val ll = lower * that.lower
      val lu = lower * that.upper
      val ul = upper * that.lower
      val uu = upper * that.upper
      new RealInterval(ll min lu min ul min uu, ll max lu max ul max uu)
    }
  }
  
  override def inverse: RealInterval = {
    if (isEmpty) RealInterval.empty
    else if (contains(0.0)) RealInterval.infinite
    else new RealInterval(upper.inverse, lower.inverse)
  }
  
  override def / (that: RealInterval): RealInterval = {
    if (isEmpty || that.isEmpty) RealInterval.empty
    else if (that.contains(0.0)) RealInterval.infinite
    else {
      val lowerInverseDivisor = that.upper.inverse
      val upperInverseDivisor = that.lower.inverse
      val ll = lower * lowerInverseDivisor
      val lu = lower * upperInverseDivisor
      val ul = upper * lowerInverseDivisor
      val uu = upper * upperInverseDivisor
      new RealInterval(ll min lu min ul min uu, ll max lu max ul max uu)
    }
  }
  
  override def intersect(that: RealInterval): RealInterval = {
    if (isEmpty || that.isEmpty) RealInterval.empty
    else {
      val greatestLower = lower max that.lower
      val leastUpper = upper min that.upper
      if (greatestLower > leastUpper) RealInterval.empty
      else new RealInterval(greatestLower, leastUpper)
    }
  }
}

object RealInterval extends IntervalField[Real.type] {
  override type Interval = RealInterval
  override type Member = Real
  
  override def Member = Real
  
  override lazy val zero: RealInterval = new RealInterval(0.0, 0.0)
  override lazy val unit: RealInterval = new RealInterval(1.0, 1.0)
  
  override lazy val empty: RealInterval = new RealInterval(Double.PositiveInfinity, Double.NegativeInfinity)
  
  lazy val infinite: RealInterval = new RealInterval(Double.NegativeInfinity, Double.PositiveInfinity)
  
  override def apply(lower: Real, upper: Real): RealInterval = {
    if (lower > upper) throw new IllegalArgumentException("lower > upper")
    else new RealInterval(lower, upper)
  }
  
  implicit override def apply(element: Real): RealInterval =
    new RealInterval(element, element)
  
  override def toString: String = "RealInterval"
}
