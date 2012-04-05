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
  * @param  lower   The lower bound of this $element.
  * @param  upper   The upper bound of this $element.
  * 
  * @define Element   Interval
  * @define element   discrete interval
  */
final class IntervalZ1 private (val lower: Long, val upper: Long) extends Ring[IntervalZ1] {
  /** Returns `true` if this is '''N'''ot '''a'''n '''I'''nterval. */
  def isNaI: Boolean = lower > upper && lower > 0L
  
  /** Returns `true` if this $element contains no values. */
  def isEmpty: Boolean = lower > upper
  
  /** Returns the number of values enclosed by this $element. */
  def size: Long = upper - lower + 1L
  
  /** Returns `true` if this $element encloses the given value. */
  def contains(i: Long): Boolean = lower <= i && i <= upper
  
  def + (that: IntervalZ1): IntervalZ1 = {
    if (that.isEmpty) { if (isNaI) this else that } else if (isEmpty) this
    else new IntervalZ1(lower + that.lower, upper + that.upper)
  }
  
  def + (n: Long): IntervalZ1 = {
    if (isEmpty) this
    else new IntervalZ1(lower + n, upper + n)
  }
  
  def unary_- : IntervalZ1 = {
    if (isEmpty) this
    else new IntervalZ1(-upper, -lower)
  }
  
  def - (that: IntervalZ1): IntervalZ1 = {
    if (that.isEmpty) { if (isNaI) this else that } else if (isEmpty) this
    else new IntervalZ1(lower - that.upper, upper - that.lower)
  }
  
  def - (n: Long): IntervalZ1 = {
    if (isEmpty) this
    else new IntervalZ1(lower - n, upper - n)
  }
  
  def * (that: IntervalZ1): IntervalZ1 = {
    if (that.isEmpty) { if (isNaI) this else that } else if (isEmpty) this
    else {
      val ll = lower * that.lower
      val lu = lower * that.upper
      val ul = upper * that.lower
      val uu = upper * that.upper
      new IntervalZ1(min(min(min(ll, lu), ul), uu), max(max(max(ll, lu), ul), uu))
    }
  }
  
  def * (n: Long): IntervalZ1 = {
    if (isEmpty) this
    else if (n >= 0) new IntervalZ1(lower * n, upper * n)
    else new IntervalZ1(upper * n, lower * n)
  }
  
  def pow(n: Long): IntervalZ1 = {
    require(n >= 0L, "negative exponent")
    if (isEmpty) this
    else {
      val lowerN = math.pow(lower, n).toLong
      val upperN = math.pow(upper, n).toLong
      if ((n & 1L) != 0L || lower >= 0L) // odd exponent or positive interval
        new IntervalZ1(lowerN, upperN)
      else if (upper <  0L) // negative interval
        new IntervalZ1(upperN, lowerN)
      else // interval spans zero
        new IntervalZ1(0L, max(lowerN, upperN))
    }
  }
  
  /** Returns the intersection of this $element and another $element.
    * 
    * @param  that  the $element to intersect with.
    * @return the intersecting interval or else the empty interval.
    */
  def intersect(that: IntervalZ1): IntervalZ1 = {
    if (that.isEmpty) { if (isNaI) this else that } else if (isEmpty) this
    else {
      val greatestLower = max(lower, that.lower)
      val leastUpper = min(upper, that.upper)
      if (greatestLower <= leastUpper) new IntervalZ1(greatestLower, leastUpper)
      else IntervalZ1.empty
    }
  }
  
  /** Applies a function to each value contained by this $element. */
  @inline final def foreach[@specialized(Unit) U](f: Long => U) {
    var i = lower
    val upper = this.upper
    while (i <= upper) {
      f(i)
      i += 1L
    }
  }
  
  override def equals(other: Any): Boolean = other match {
    case that: IntervalZ1 => lower == that.lower && upper == that.upper
    case _ => false
  }
  
  override def hashCode: Int =
    mash(mix(mix(1851844763, lower), upper))
  
  override def toString: String =
    if (isNaI) "[NaI]" else if (isEmpty) "[]" else "["+ lower +", "+ upper +"]"
}

/** Contains factory methods and implicit conversions for discrete intervals. */
object IntervalZ1 {
  /** The degenerate discrete interval containing just the zero value. */
  val zero: IntervalZ1 = new IntervalZ1(0L, 0L)
  
  /** The degenerate discrete interval containing just the unit value. */
  val one: IntervalZ1 = new IntervalZ1(1L, 1L)
  
  /** The empty discrete interval. */
  val empty: IntervalZ1 = new IntervalZ1(0L, -1L)
  
  /** The '''N'''ot '''a'''n '''I'''nterval value. */
  val NaI: IntervalZ1 = new IntervalZ1(1L, 0L)
  
  /** Returns a new discrete interval with a lower and upper bound.
    * 
    * @param  lower   the lower bound of the discrete interval.
    * @param  upper   the upper bound of the discrete interval.
    * @return the interval [`lower`, `upper`] or `NaI` if `lower` > `upper`.
    */
  def apply(lower: Long, upper: Long): IntervalZ1 =
    if (lower <= upper) new IntervalZ1(lower, upper) else NaI
  
  def unapply(interval: IntervalZ1): Option[(Long, Long)] =
    if (interval.isEmpty) None else Some(interval.lower, interval.upper)
  
  /** Returns an interval containing just the given value. */
  implicit def degenerate(value: Long): IntervalZ1 = new IntervalZ1(value, value)
  
  /** The discrete interval additive identity typeclass. */
  implicit val additiveIdentity = new Zero(zero)
  
  /** The discrete interval multiplicative identity typeclass. */
  implicit val multiplicativeIdentity = new One(one)
  
  /** The default struct for discrete intervals. */
  implicit lazy val struct = new StructIntervalZ1
  
  /** A struct for discrete intervals. */
  class StructIntervalZ1(frameOffset: Long, frameSize: Long, frameAlignment: Long)
    extends Struct2[Long, Long, IntervalZ1](frameOffset, frameSize, frameAlignment) {
    
    def this() = this(0L, 0L, 0L)
    
    /** The `lower` bound field projection of this struct. */
    def lower: Struct[Long] = field1
    
    /** The `upper` bound field projection of this struct. */
    def upper: Struct[Long] = field2
    
    def load(data: Data, address: Long): IntervalZ1 = {
      val lower = data.loadLong(address + offset1)
      val upper = data.loadLong(address + offset2)
      new IntervalZ1(lower, upper)
    }
    
    def store(data: Data, address: Long, interval: IntervalZ1) {
      data.storeLong(address + offset1, interval.lower)
      data.storeLong(address + offset2, interval.upper)
    }
    
    override def project(offset: Long, size: Long, alignment: Long): StructIntervalZ1 =
      new StructIntervalZ1(offset1 + offset, size, alignment)
    
    override def toString: String = "StructIntervalZ1"
  }
}
