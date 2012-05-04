/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait IntervalRing extends Any with Equals with Ring { self =>
  override type Vector = Interval
  
  type Interval <: IntervalRing {
    type Interval = self.Interval
    type Element  = self.Element
  }
  
  type Element <: OrderedRing {
    type Vector = self.Element
  }
  
  def Interval: IntervalRing.Space {
    type Interval = self.Interval
    type Element  = self.Element
  }
  
  def lower: Element
  
  def upper: Element
  
  def isEmpty: Boolean = lower > upper
  
  def contains(element: Element): Boolean =
    lower <= element && upper >= element
  
  override def + (that: Interval): Interval = {
    if (isEmpty || that.isEmpty) Interval.empty
    else Interval(lower + that.lower, upper + that.upper)
  }
  
  override def unary_- : Interval = {
    if (isEmpty) Interval.empty
    else Interval(-upper, -lower)
  }
  
  override def - (that: Interval): Interval = {
    if (isEmpty || that.isEmpty) Interval.empty
    else Interval(lower - that.upper, upper - that.lower)
  }
  
  override def * (that: Interval): Interval = {
    if (isEmpty || that.isEmpty) Interval.empty
    else {
      val ll = lower * that.lower
      val lu = lower * that.upper
      val ul = upper * that.lower
      val uu = upper * that.upper
      Interval(ll min lu min ul min uu, ll max lu max ul max uu)
    }
  }
  
  def intersect(that: Interval): Interval = {
    if (isEmpty || that.isEmpty) Interval.empty
    else {
      val greatestLower = lower max that.lower
      val leastUpper = upper min that.upper
      if (greatestLower > leastUpper) Interval.empty
      else Interval(greatestLower, leastUpper)
    }
  }
  
  override def canEqual(other: Any): Boolean = other.isInstanceOf[IntervalRing]
  
  override def equals(other: Any): Boolean = other match {
    case that: IntervalRing =>
      that.canEqual(this) && lower.equals(that.lower) && upper.equals(that.upper)
    case _ => false
  }
  
  override def hashCode: Int = {
    import basis.util.MurmurHash._
    mash(mix(mix(635062501, lower), upper))
  }
  
  override def toString: String = {
    if (isEmpty) "{}"
    else if (lower == upper) "{"+ lower +"}"
    else "["+ lower +", "+ upper +"]"
  }
}

object IntervalRing {
  trait Space extends Ring.Space { self =>
    override type Vector = Interval
    
    type Interval <: IntervalRing {
      type Interval = self.Interval
      type Element  = self.Element
    }
    
    type Element <: OrderedRing {
      type Vector = self.Element
    }
    
    def Element: OrderedRing.Space {
      type Vector = self.Element
    }
    
    override def zero: Interval = apply(Element.zero)
    
    override def unit: Interval = apply(Element.unit)
    
    def empty: Interval
    
    def apply(lower: Element, upper: Element): Interval
    
    def apply(element: Element): Interval = apply(element, element)
    
    def unapply(interval: Interval): Option[(Element, Element)] =
      if (!interval.isEmpty) Some(interval.lower, interval.upper) else None
  }
  
  trait Scalar extends Ring.Scalar { self =>
    override type Scalar <: IntervalRing {
      type Interval = self.Scalar
    }
    
    override def Scalar: IntervalRing.Space {
      type Interval = self.Scalar
    }
  }
}
