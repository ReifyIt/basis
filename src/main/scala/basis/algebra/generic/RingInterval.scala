/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

class RingInterval[S <: OrderedRing with Singleton](val Member: S) extends IntervalRing[S] {
  final class Element private[RingInterval] (val lower: Member, val upper: Member) extends super.Element
  
  override type Interval = Element
  
  override lazy val zero: Interval = super.zero
  override lazy val unit: Interval = super.unit
  
  override lazy val empty: Interval = new Interval(Member.unit, Member.zero)
  
  override def apply(lower: Member, upper: Member): Interval = {
    if (lower > upper) throw new IllegalArgumentException("lower > upper")
    else new Interval(lower, upper)
  }
  
  override def toString: String = "RingInterval"+"("+ Member +")"
}
