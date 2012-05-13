/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait IntervalField[S <: OrderedField with Singleton] extends IntervalRing[S] with Field {
  trait Element extends Any with super[IntervalRing].Element with super[Field].Element {
    override def inverse: Interval = {
      if (isEmpty) Interval.empty
      else if (contains(Member.zero))
        throw new ArithmeticException("inverse of interval containing zero")
      else Interval(upper.inverse, lower.inverse)
    }
    
    override def / (that: Interval): Interval = {
      if (isEmpty || that.isEmpty) Interval.empty
      else if (that.contains(Member.zero))
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
  }
  
  override type Interval <: Element
}
