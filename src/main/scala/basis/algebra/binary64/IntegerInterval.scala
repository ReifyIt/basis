/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

import language.implicitConversions

final class IntegerInterval private (val lower: Integer, val upper: Integer)
  extends IntervalRing {
  
  override type Interval = IntegerInterval
  override type Element  = Integer
  
  override def Interval = IntegerInterval
  
  override def isEmpty: Boolean = lower > upper
  
  override def contains(element: Integer): Boolean =
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

object IntegerInterval extends IntervalRing.Space {
  override type Interval = IntegerInterval
  override type Element  = Integer
  
  override def Element = Integer
  
  override val zero: Interval = new Interval(0L, 0L)
  
  override val unit: Interval = new Interval(1L, 1L)
  
  override val empty: Interval = new Interval(0L, -1L)
  
  override def apply(lower: Integer, upper: Integer): Interval = {
    if (lower > upper) throw new IllegalArgumentException("lower > upper")
    else new Interval(lower, upper)
  }
  
  implicit override def apply(element: Integer): Interval =
    new Interval(element, element)
  
  override def toString: String = "IntegerInterval"
}
