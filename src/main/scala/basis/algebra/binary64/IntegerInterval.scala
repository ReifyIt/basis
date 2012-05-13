/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

import language.implicitConversions

final class IntegerInterval private (val lower: Integer, val upper: Integer) extends IntegerInterval.Element {
  override def isEmpty: Boolean = lower > upper
  
  override def contains(value: Integer): Boolean =
    lower <= value && upper >= value
  
  override def + (that: IntegerInterval): IntegerInterval = {
    if (isEmpty || that.isEmpty) IntegerInterval.empty
    else new IntegerInterval(lower + that.lower, upper + that.upper)
  }
  
  override def unary_- : IntegerInterval = {
    if (isEmpty) IntegerInterval.empty
    else new IntegerInterval(-upper, -lower)
  }
  
  override def - (that: IntegerInterval): IntegerInterval = {
    if (isEmpty || that.isEmpty) IntegerInterval.empty
    else new IntegerInterval(lower - that.upper, upper - that.lower)
  }
  
  override def * (that: IntegerInterval): IntegerInterval = {
    if (isEmpty || that.isEmpty) IntegerInterval.empty
    else {
      val ll = lower * that.lower
      val lu = lower * that.upper
      val ul = upper * that.lower
      val uu = upper * that.upper
      new IntegerInterval(ll min lu min ul min uu, ll max lu max ul max uu)
    }
  }
  
  override def intersect(that: IntegerInterval): IntegerInterval = {
    if (isEmpty || that.isEmpty) IntegerInterval.empty
    else {
      val greatestLower = lower max that.lower
      val leastUpper    = upper min that.upper
      if (greatestLower > leastUpper) IntegerInterval.empty
      else new IntegerInterval(greatestLower, leastUpper)
    }
  }
}

object IntegerInterval extends IntervalRing[Integer.type] {
  override type Interval = IntegerInterval
  override type Member = Integer
  
  override def Member = Integer
  
  override lazy val zero: IntegerInterval = new IntegerInterval(0L, 0L)
  override lazy val unit: IntegerInterval = new IntegerInterval(1L, 1L)
  
  override lazy val empty: IntegerInterval = new IntegerInterval(0L, -1L)
  
  override def apply(lower: Integer, upper: Integer): IntegerInterval = {
    if (lower > upper) throw new IllegalArgumentException("lower > upper")
    else new IntegerInterval(lower, upper)
  }
  
  implicit override def apply(value: Integer): IntegerInterval =
    new IntegerInterval(value, value)
  
  override def toString: String = "IntegerInterval"
}
