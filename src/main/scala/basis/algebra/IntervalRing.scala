/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait IntervalRing[S <: OrderedRing with Singleton] extends Ring {
  trait Element extends Any with super.Element {
    protected def Interval: IntervalRing.this.type = IntervalRing.this
    
    def lower: Member
    
    def upper: Member
    
    def isEmpty: Boolean = lower > upper
    
    def contains(value: Member): Boolean =
      lower <= value && upper >= value
    
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
        val leastUpper    = upper min that.upper
        if (greatestLower > leastUpper) Interval.empty
        else Interval(greatestLower, leastUpper)
      }
    }
    
    override def equals(other: Any): Boolean = other match {
      case that: Element => lower.equals(that.lower) && upper.equals(that.upper)
      case _ => false
    }
    
    override def hashCode: Int = {
      import scala.util.hashing.MurmurHash3._
      finalizeHash(mixLast(mix(635062501, lower.##), upper.##), 2)
    }
    
    override def toString: String = {
      if (isEmpty) "{}"
      else if (lower == upper) "{"+ lower +"}"
      else "["+ lower +", "+ upper +"]"
    }
  }
  
  override type Value = Interval
  
  type Interval <: Element
  
  type Member = S#Value
  
  def Member: S
  
  override def zero: Interval = apply(Member.zero)
  
  override def unit: Interval = apply(Member.unit)
  
  def empty: Interval
  
  def apply(lower: Member, upper: Member): Interval
  
  def apply(value: Member): Interval = apply(value, value)
  
  def unapply(interval: Interval): Option[(Member, Member)] =
    if (interval.isEmpty) None else Some(interval.lower, interval.upper)
}
