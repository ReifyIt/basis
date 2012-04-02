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

/** A 2-dimensional vector of discrete closed intervals.
  * 
  * @author Chris Sachs
  * 
  * @constructor Constructs an interval vector with bounds for each axis.
  * @param  lowerX  The lower bound on the ''x''-axis.
  * @param  upperX  The upper bound on the ''x''-axis.
  * @param  lowerY  The lower bound on the ''y''-axis.
  * @param  upperY  The upper bound on the ''y''-axis.
  * 
  * @define vector  discrete 2D interval
  * @define scalar  discrete interval
  */
final class IntervalZ2(
    val lowerX: Long, val upperX: Long,
    val lowerY: Long, val upperY: Long)
  extends Vector[IntervalZ2, IntervalZ1] {
  
  checkBounds()
  private def checkBounds() {
    require(lowerX <= upperX, "lower x-bound greater than upper x-bound")
    require(lowerY <= upperY, "lower y-bound greater than upper y-bound")
  }
  
  def this(x: IntervalZ1, y: IntervalZ1) =
    this(x.lower, x.upper, y.lower, y.upper)
  
  /** The ''x''-axis bounds of this $vector. */
  def x: IntervalZ1 = new IntervalZ1(lowerX, upperX)
  
  /** The ''y''-axis bounds of this $vector. */
  def y: IntervalZ1 = new IntervalZ1(lowerY, upperY)
  
  private def x_size: Long = upperX - lowerX + 1L
  
  private def y_size: Long = upperY - lowerY + 1L
  
  /** Returns the number of values enclosed by this $vector. */
  def size: Long = x_size * y_size
  
  /** Returns `true` if this $vector encloses the given coordinates.
    * 
    * @param  i   the ''x''-coordinate; tested against the ''x''-axis bounds.
    * @param  j   the ''y''-coordinate; tested against the ''y''-axis bounds.
    * @return `true` if both coordinates are bounded.
    */
  def contains(i: Long, j: Long): Boolean =
    lowerX <= i && i <= upperX && lowerY <= j && j <= upperY
  
  def + (that: IntervalZ2): IntervalZ2 =
    new IntervalZ2(
      lowerX + that.lowerX, upperX + that.upperX,
      lowerY + that.lowerY, upperY + that.upperY)
  
  def unary_- : IntervalZ2 =
    new IntervalZ2(-upperX, -lowerX, -upperY, -lowerY)
  
  def - (that: IntervalZ2): IntervalZ2 =
    new IntervalZ2(
      lowerX - that.upperX, upperX - that.lowerX,
      lowerY - that.upperY, upperY - that.lowerY)
  
  def :* (scalar: IntervalZ1): IntervalZ2 = {
    val llx = lowerX * scalar.lower
    val lux = lowerX * scalar.upper
    val ulx = upperX * scalar.lower
    val uux = upperX * scalar.upper
    val lly = lowerY * scalar.lower
    val luy = lowerY * scalar.upper
    val uly = upperY * scalar.lower
    val uuy = upperY * scalar.upper
    new IntervalZ2(
      min(min(min(llx, lux), ulx), uux),
      max(max(max(llx, lux), ulx), uux),
      min(min(min(lly, luy), uly), uuy),
      max(max(max(lly, luy), uly), uuy))
  }
  
  def *: (scalar: IntervalZ1): IntervalZ2 = this :* scalar
  
  /** Applies a function to each coordinate pair contained by this $vector. */
  @inline final def foreach[@specialized(Unit) U](f: (Long, Long) => U) {
    var i = 0L // reserve jvm local variable
    var j = 0L // reserve jvm local variable
    val upperX = this.upperX
    val lowerX = this.lowerX
    val upperY = this.upperY
    j = lowerY
    while (j <= upperY) {
      i = lowerX
      while (i <= upperX) {
        f(i, j)
        i += 1L
      }
      j += 1L
    }
  }
  
  override def equals(other: Any): Boolean = other match {
    case that: IntervalZ2 =>
      lowerX == that.lowerX && upperX == that.upperX &&
      lowerY == that.lowerY && upperY == that.upperY
    case _ => false
  }
  
  override def hashCode: Int =
    mash(mix(mix(mix(mix(1506359539, lowerX), upperX), lowerY), upperY))
  
  override def toString: String =
    "IntervalZ2"+"("+ x +", "+ y +")"
}

/** Contains factory methods for discrete 2D intervals. */
object IntervalZ2 {
  def apply(lowerX: Long, upperX: Long, lowerY: Long, upperY: Long): IntervalZ2 =
    new IntervalZ2(lowerX, upperX, lowerY, upperY)
  
  def apply(x: IntervalZ1, y: IntervalZ1): IntervalZ2 =
    new IntervalZ2(x, y)
  
  def unapply(interval: IntervalZ2): Some[(IntervalZ1, IntervalZ1)] =
    Some(interval.x, interval.y)
  
  /** Returns a degenerate discrete 2D interval containing just the given coordinates. */
  def degenerate(x: Long, y: Long): IntervalZ2 = new IntervalZ2(x, x, y, y)
  
  /** The discrete 2D interval additive identity. */
  implicit val additiveIdentity = new AdditiveIdentity(degenerate(0L, 0L))
  
  /** The default struct for discrete 2D intervals. */
  implicit lazy val struct = new StructIntervalZ2
  
  /** A struct for discrete 2D intervals. */
  class StructIntervalZ2(frameOffset: Long, frameSize: Long, frameAlignment: Long)
    extends Struct2[IntervalZ1, IntervalZ1, IntervalZ2](frameOffset, frameSize, frameAlignment) {
    
    import IntervalZ1.StructIntervalZ1
    
    def this() = this(0L, 0L, 0L)
    
    protected override val field1 = new StructIntervalZ1(offset1, size, alignment)
    
    protected override val field2 = new StructIntervalZ1(offset2, size, alignment)
    
    /** The `x` field projection of this struct. */
    def x: StructIntervalZ1 = field1
    
    /** The `y` field projection of this struct. */
    def y: StructIntervalZ1 = field2
    
    /** The `lowerX` field projection of this struct. */
    def lowerX: Struct[Long] = x.lower
    
    /** The `upperX` field projection of this struct. */
    def upperX: Struct[Long] = x.upper
    
    /** The `lowerY` field projection of this struct. */
    def lowerY: Struct[Long] = y.lower
    
    /** The `upperY` field projection of this struct. */
    def upperY: Struct[Long] = y.upper
    
    def load(data: Data, address: Long): IntervalZ2 = {
      val lowerX = field1.lower.load(data, address)
      val upperX = field1.upper.load(data, address)
      val lowerY = field2.lower.load(data, address)
      val upperY = field2.upper.load(data, address)
      new IntervalZ2(lowerX, upperX, lowerY, upperY)
    }
    
    def store(data: Data, address: Long, interval: IntervalZ2) {
      field1.lower.store(data, address, interval.lowerX)
      field1.upper.store(data, address, interval.upperX)
      field2.lower.store(data, address, interval.lowerY)
      field2.upper.store(data, address, interval.upperY)
    }
    
    override def project(offset: Long, size: Long, alignment: Long): StructIntervalZ2 =
      new StructIntervalZ2(offset1 + offset, size, alignment)
    
    override def toString: String = "StructIntervalZ2"
  }
}
