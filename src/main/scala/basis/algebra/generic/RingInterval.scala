/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

final class RingInterval[F <: OrderedRing { type Vector = F }] private
    (val Interval: RingInterval.Space[F], val lower: F, val upper: F)
  extends IntervalRing {
  
  override type Interval = RingInterval[F]
  override type Element  = F
}

object RingInterval {
  def apply[F <: OrderedRing { type Vector = F }]
      (Element: OrderedRing.Space { type Vector = F }) =
    new Space[F](Element)
  
  class Space[F <: OrderedRing { type Vector = F }]
      (val Element: OrderedRing.Space { type Vector = F })
    extends IntervalRing.Space {
    
    override type Interval = RingInterval[F]
    override type Element  = F
    
    override lazy val zero: Interval = super.zero
    override lazy val unit: Interval = super.unit
    
    override lazy val empty: Interval =
      new Interval(this, Element.unit, Element.zero)
    
    override def apply(lower: Element, upper: Element): Interval = {
      if (lower > upper) throw new IllegalArgumentException("lower > upper")
      else new Interval(this, lower, upper)
    }
    
    override def toString: String = "RingInterval"+"("+ Element +")"
  }
}
