/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.math

/** Closed sets of elements between two points of an ordered ring.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Structures
  * 
  * @define space   interval ring
  */
trait IntervalRing extends Ring {
  /** A closed set of elements between two points of an ordered ring.
    * 
    * @define element   $interval
    * @define interval  interval
    */
  trait Value extends Any with super.Value {
    /** Returns the inclusive lower bound of this $interval. */
    def lower: Scalar
    
    /** Returns the inclusive upper bound of this $interval. */
    def upper: Scalar
    
    /** Returns `true` if a value lies on this $interval. */
    def contains(value: Scalar): Boolean =
      lower <= value && value <= upper
    
    override def + (that: Interval): Interval =
      IntervalRing.this.apply(lower + that.lower, upper + that.upper)
    
    override def unary_- : Interval =
      IntervalRing.this.apply(-upper, -lower)
    
    override def - (that: Interval): Interval =
      IntervalRing.this.apply(lower - that.upper, upper - that.lower)
    
    override def * (that: Interval): Interval = {
      val ll = lower * that.lower
      val lu = lower * that.upper
      val ul = upper * that.lower
      val uu = upper * that.upper
      IntervalRing.this.apply(
        ll min lu min ul min uu,
        ll max lu max ul max uu)
    }
    
    override def equals(other: Any): Boolean = other match {
      case that: Value => lower.equals(that.lower) && upper.equals(that.upper)
      case _ => false
    }
    
    override def hashCode: Int = {
      import scala.util.hashing.MurmurHash3._
      finalizeHash(mix(mix(635062501, lower.hashCode), upper.hashCode), 2)
    }
    
    override def toString: String = {
      val s = new java.lang.StringBuilder(IntervalRing.this.toString)
      s.append('(')
      s.append(lower)
      s.append(", ")
      s.append(upper)
      s.append(')')
      s.toString
    }
  }
  
  /** The type of elements in this $space; equivalent to the type of intervals. */
  override type Element = Interval
  
  /** The type of intervals in this $space. */
  type Interval <: Value
  
  /** The type of scalars in this $space. */
  type Scalar = Scalar.Element
  
  /** Returns the scalar set of this $space. */
  val Scalar: OrderedRing
  
  override def zero: Interval = degenerate(Scalar.zero)
  
  override def unit: Interval = degenerate(Scalar.unit)
  
  /** Returns a degenerate interval spanning a single point. */
  implicit def degenerate(value: Scalar): Interval = apply(value, value)
  
  /** Returns a new interval with inclusive lower and upper bounds. */
  def apply(lower: Scalar, upper: Scalar): Interval
  
  /** Extracts the lower and upper bounds from an interval. */
  def unapply(interval: Interval): Option[(Scalar, Scalar)] =
    Some((interval.lower, interval.upper))
}
