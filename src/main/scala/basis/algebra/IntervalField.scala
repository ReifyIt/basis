/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait IntervalField extends Any with IntervalRing with Field { self =>
  override type Interval <: IntervalField {
    type Interval = self.Interval
    type Element  = self.Element
  }
  
  override type Element <: OrderedField {
    type Vector = self.Element
  }
  
  override def Interval: IntervalField.Space {
    type Interval = self.Interval
    type Element  = self.Element
  }
  
  override def lower: Element
  
  override def upper: Element
  
  override def isEmpty: Boolean
  
  override def contains(element: Element): Boolean
  
  override def + (that: Interval): Interval
  
  override def unary_- : Interval
  
  override def - (that: Interval): Interval
  
  override def * (that: Interval): Interval
  
  override def inverse: Interval = {
    if (isEmpty) Interval.empty
    else if (contains(Interval.Element.zero))
      throw new ArithmeticException("inverse of interval containing zero")
    else Interval(upper.inverse, lower.inverse)
  }
  
  override def / (that: Interval): Interval = {
    if (isEmpty || that.isEmpty) Interval.empty
    else if (that.contains(Interval.Element.zero))
      throw new ArithmeticException("division by interval containing zero")
    else {
      val lowerInverseDivisor = that.upper.inverse
      val upperInverseDivisor = that.lower.inverse
      val ll = lower * lowerInverseDivisor
      val lu = lower * upperInverseDivisor
      val ul = upper * lowerInverseDivisor
      val uu = upper * upperInverseDivisor
     Interval(ll min lu min ul min uu, ll max lu max ul max uu)
    }
  }
  
  override def intersect(that: Interval): Interval
}

object IntervalField {
  trait Space extends IntervalRing.Space with Field.Space { self =>
    override type Interval <: IntervalField {
      type Interval = self.Interval
      type Element  = self.Element
    }
    
    override type Element <: OrderedField {
      type Vector = self.Element
    }
    
    override def Element: OrderedField.Space {
      type Vector = self.Element
    }
    
    override def zero: Interval
    
    override def unit: Interval
    
    override def empty: Interval
    
    override def apply(lower: Element, upper: Element): Interval
    
    override def apply(element: Element): Interval
    
    override def unapply(interval: Interval): Option[(Element, Element)]
  }
  
  trait Scalar extends IntervalRing.Scalar { self =>
    override type Scalar <: IntervalField {
      type Interval = self.Scalar
    }
    
    override def Scalar: IntervalField.Space {
      type Interval = self.Scalar
    }
  }
}
