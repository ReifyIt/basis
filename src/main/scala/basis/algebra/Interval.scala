/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import scala.math.{min, max}

import basis.memory._
import basis.util.MurmurHash._

/** A discrete closed interval implementing arithmetic operations.
  * 
  * @author Chris Sachs
  * 
  * @constructor Constructs an interval with a lower and upper bound.
  * @param  lower   The lower bound of this $interval.
  * @param  upper   The upper bound of this $interval.
  * 
  * @define Element   $Interval
  * @define element   $interval
  * @define Interval  Interval
  * @define interval  discrete interval
  */
final class Interval private (val lower: Long, val upper: Long) extends Ring[Interval] {
  /** Returns `true` if this is '''N'''ot '''a'''n '''I'''nterval. */
  def isNaI: Boolean = lower > upper && lower > 0L
  
  /** Returns `true` if this $interval contains no values. */
  def isEmpty: Boolean = lower > upper
  
  /** Returns the number of values enclosed by this $interval. */
  def size: Long = upper - lower + 1L
  
  /** Returns `true` if this $interval encloses the given value. */
  def contains(i: Long): Boolean = lower <= i && i <= upper
  
  def + (that: Interval): Interval = {
    if (that.isEmpty) { if (isNaI) this else that } else if (isEmpty) this
    else new Interval(lower + that.lower, upper + that.upper)
  }
  
  def + (n: Long): Interval = {
    if (isEmpty) this
    else new Interval(lower + n, upper + n)
  }
  
  def unary_- : Interval = {
    if (isEmpty) this
    else new Interval(-upper, -lower)
  }
  
  def - (that: Interval): Interval = {
    if (that.isEmpty) { if (isNaI) this else that } else if (isEmpty) this
    else new Interval(lower - that.upper, upper - that.lower)
  }
  
  def - (n: Long): Interval = {
    if (isEmpty) this
    else new Interval(lower - n, upper - n)
  }
  
  def * (that: Interval): Interval = {
    if (that.isEmpty) { if (isNaI) this else that } else if (isEmpty) this
    else {
      val ll = lower * that.lower
      val lu = lower * that.upper
      val ul = upper * that.lower
      val uu = upper * that.upper
      new Interval(min(min(min(ll, lu), ul), uu), max(max(max(ll, lu), ul), uu))
    }
  }
  
  def * (n: Long): Interval = {
    if (isEmpty) this
    else if (n >= 0) new Interval(lower * n, upper * n)
    else new Interval(upper * n, lower * n)
  }
  
  def pow(n: Long): Interval = {
    require(n >= 0L, "negative exponent")
    if (isEmpty) this
    else {
      val lowerN = math.pow(lower, n).toLong
      val upperN = math.pow(upper, n).toLong
      if ((n & 1L) != 0L || lower >= 0L) // odd exponent or positive interval
        new Interval(lowerN, upperN)
      else if (upper <  0L) // negative interval
        new Interval(upperN, lowerN)
      else // interval spans zero
        new Interval(0L, max(lowerN, upperN))
    }
  }
  
  /** Returns the intersection of this $interval and another $interval.
    * 
    * @param  that  the $interval to intersect with.
    * @return the intersecting interval or else the empty interval.
    */
  def intersect(that: Interval): Interval = {
    if (that.isEmpty) { if (isNaI) this else that } else if (isEmpty) this
    else {
      val greatestLower = max(lower, that.lower)
      val leastUpper = min(upper, that.upper)
      if (greatestLower <= leastUpper) new Interval(greatestLower, leastUpper)
      else Interval.empty
    }
  }
  
  /** Applies a function to each value contained by this $interval. */
  @inline final def foreach[@specialized(Unit) U](f: Long => U) {
    var i = lower
    val upper = this.upper
    while (i <= upper) {
      f(i)
      i += 1L
    }
  }
  
  override def equals(other: Any): Boolean = other match {
    case that: Interval => lower == that.lower && upper == that.upper
    case _ => false
  }
  
  override def hashCode: Int =
    mash(mix(mix(1851844763, lower), upper))
  
  override def toString: String =
    if (isNaI) "[NaI]" else if (isEmpty) "[]" else "["+ lower +", "+ upper +"]"
}

/** Contains factory methods and implicit conversions for discrete intervals. */
object Interval {
  /** The '''N'''ot '''a'''n '''I'''nterval value. */
  val NaI: Interval = new Interval(1L, 0L)
  
  /** The empty interval. */
  val empty: Interval = new Interval(0L, -1L)
  
  /** Creates a new interval with a lower and upper bound.
    * 
    * @param  lower   the lower bound of the interval.
    * @param  upper   the upper bound of the interval.
    * @return the interval [`lower`, `upper`] or `NaI` if `lower` > `upper`.
    */
  def apply(lower: Long, upper: Long): Interval =
    if (lower <= upper) new Interval(lower, upper) else NaI
  
  def unapply(interval: Interval): Option[(Long, Long)] =
    if (interval.isEmpty) None else Some(interval.lower, interval.upper)
  
  /** Returns an improper interval containing just the given value. */
  implicit def improper(value: Long): Interval = new Interval(value, value)
  
  /** The discrete interval additive identity. */
  implicit val additiveIdentity = new AdditiveIdentity(improper(0L))
  
  /** The discrete interval multiplicative identity. */
  implicit val multiplicativeIdentity = new MultiplicativeIdentity(improper(1L))
  
  /** The default struct for discrete intervals. */
  implicit lazy val struct = new StructInterval
  
  /** A struct for discrete intervals. */
  class StructInterval(frameOffset: Long, frameSize: Long, frameAlignment: Long)
    extends Struct2[Long, Long, Interval](frameOffset, frameSize, frameAlignment) {
    
    def this() = this(0L, 0L, 0L)
    
    /** The `lower` bound field projection of this struct. */
    def lower: Struct[Long] = field1
    
    /** The `upper` bound field projection of this struct. */
    def upper: Struct[Long] = field2
    
    def load(data: Data, address: Long): Interval = {
      val lower = data.loadLong(address + offset1)
      val upper = data.loadLong(address + offset2)
      new Interval(lower, upper)
    }
    
    def store(data: Data, address: Long, interval: Interval) {
      data.storeLong(address + offset1, interval.lower)
      data.storeLong(address + offset2, interval.upper)
    }
    
    override def project(offset: Long, size: Long, alignment: Long): StructInterval =
      new StructInterval(offset1 + offset, size, alignment)
    
    override def toString: String = "StructInterval"
  }
}
