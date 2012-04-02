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
  * @define Element   discrete interval
  * @define element   discrete interval
  */
final class IntervalZ1(val lower: Long, val upper: Long) extends Ring[IntervalZ1] {
  checkBounds()
  private def checkBounds() {
    require(lower <= upper, "lower bound greater than upper bound")
  }
  
  /** Returns the number of values enclosed by this $element. */
  def size: Long = upper - lower + 1L
  
  /** Returns `true` if this $element encloses the given value. */
  def contains(i: Long): Boolean = lower <= i && i <= upper
  
  def + (that: IntervalZ1): IntervalZ1 =
    new IntervalZ1(lower + that.lower, upper + that.upper)
  
  def + (n: Int): IntervalZ1 =
    new IntervalZ1(lower + n, upper + n)
  
  def unary_- : IntervalZ1 =
    new IntervalZ1(-upper, -lower)
  
  def - (that: IntervalZ1): IntervalZ1 =
    new IntervalZ1(lower - that.upper, upper - that.lower)
  
  def - (n: Int): IntervalZ1 =
    new IntervalZ1(lower - n, upper - n)
  
  def * (that: IntervalZ1): IntervalZ1 = {
    val ll = lower * that.lower
    val lu = lower * that.upper
    val ul = upper * that.lower
    val uu = upper * that.upper
    new IntervalZ1(min(min(min(ll, lu), ul), uu), max(max(max(ll, lu), ul), uu))
  }
  
  def * (n: Int): IntervalZ1 =
    if (n >= 0) new IntervalZ1(lower * n, upper * n)
    else new IntervalZ1(upper * n, lower * n)
  
  def pow(n: Int): IntervalZ1 = {
    require(n >= 0, "negative exponent")
    val lowerN = math.pow(lower, n).toLong
    val upperN = math.pow(upper, n).toLong
    if ((n & 1) != 0 || lower >= 0L) // odd exponent or positive interval
      new IntervalZ1(lowerN, upperN)
    else if (upper <  0L) // negative interval
      new IntervalZ1(upperN, lowerN)
    else // interval spans zero
      new IntervalZ1(0L, max(lowerN, upperN))
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
    mash(mix(mix(1506359538, lower), upper))
  
  override def toString: String =
    "["+ lower +", "+ upper +"]"
}

/** Contains factory methods for discrete intervals. */
object IntervalZ1 {
  def apply(lower: Long, upper: Long): IntervalZ1 =
    new IntervalZ1(lower, upper)
  
  def unapply(interval: IntervalZ1): Some[(Long, Long)] =
    Some(interval.lower, interval.upper)
  
  /** Returns a degenerate interval containing just the given value. */
  implicit def degenerate(value: Long): IntervalZ1 = new IntervalZ1(value, value)
  
  /** The discrete interval additive identity. */
  implicit val additiveIdentity = new AdditiveIdentity(degenerate(0L))
  
  /** The discrete interval multiplicative identity. */
  implicit val multiplicativeIdentity = new MultiplicativeIdentity(degenerate(1L))
  
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
