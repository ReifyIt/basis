/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

import language.implicitConversions

final class RealInterval private (val lower: Real, val upper: Real)
  extends IntervalField {
  
  override type Interval = RealInterval
  override type Element  = Real
  
  override def Interval = RealInterval
  
  override def isEmpty: Boolean = lower > upper
  
  override def contains(element: Real): Boolean =
    lower <= element && upper >= element
  
  override def + (that: Interval): Interval = {
    if (isEmpty || that.isEmpty) Interval.empty
    else new Interval(lower + that.lower, upper + that.upper)
  }
  
  override def unary_- : Interval = {
    if (isEmpty) Interval.empty
    else new Interval(-upper, -lower)
  }
  
  override def - (that: Interval): Interval = {
    if (isEmpty || that.isEmpty) Interval.empty
    else new Interval(lower - that.upper, upper - that.lower)
  }
  
  override def * (that: Interval): Interval = {
    if (isEmpty || that.isEmpty) Interval.empty
    else {
      val ll = lower * that.lower
      val lu = lower * that.upper
      val ul = upper * that.lower
      val uu = upper * that.upper
      new Interval(ll min lu min ul min uu, ll max lu max ul max uu)
    }
  }
  
  override def inverse: Interval = {
    if (isEmpty) Interval.empty
    else if (contains(0.0)) Interval.infinite
    else new Interval(upper.inverse, lower.inverse)
  }
  
  override def / (that: Interval): Interval = {
    if (isEmpty || that.isEmpty) Interval.empty
    else if (that.contains(0.0)) Interval.infinite
    else {
      val lowerInverseDivisor = that.upper.inverse
      val upperInverseDivisor = that.lower.inverse
      val ll = lower * lowerInverseDivisor
      val lu = lower * upperInverseDivisor
      val ul = upper * lowerInverseDivisor
      val uu = upper * upperInverseDivisor
      new Interval(ll min lu min ul min uu, ll max lu max ul max uu)
    }
  }
  
  override def intersect(that: Interval): Interval = {
    if (isEmpty || that.isEmpty) Interval.empty
    else {
      val greatestLower = lower max that.lower
      val leastUpper = upper min that.upper
      if (greatestLower > leastUpper) Interval.empty
      else new Interval(greatestLower, leastUpper)
    }
  }
}

object RealInterval extends IntervalField.Space {
  override type Interval = RealInterval
  override type Element  = Real
  
  override def Element = Real
  
  override val zero: Interval = new Interval(0.0, 0.0)
  
  override val unit: Interval = new Interval(1.0, 1.0)
  
  override val empty: Interval = new Interval(Double.PositiveInfinity, Double.NegativeInfinity)
  
  val infinite: Interval = new Interval(Double.NegativeInfinity, Double.PositiveInfinity)
  
  override def apply(lower: Real, upper: Real): Interval = {
    if (lower > upper) throw new IllegalArgumentException("lower > upper")
    else new Interval(lower, upper)
  }
  
  implicit override def apply(element: Real): Interval =
    new Interval(element, element)
  
  override def toString: String = "RealInterval"
}
