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

/** A 2-dimensional vector of discrete intervals.
  * 
  * @author Chris Sachs
  * 
  * @constructor Constructs a 2D interval with two coordinate bounds.
  * @param  x   The ''x''-axis bounds.
  * @param  y   The ''y''-axis bounds.
  * 
  * @define vector  2D interval vector
  * @define scalar  discrete interval
  */
final class IntervalZ2(val x: Interval, val y: Interval)
  extends Vector[IntervalZ2, Interval] {
  
  /** Returns the number of values enclosed by this $vector. */
  def size: Long = x.size * y.size
  
  /** Returns `true` if this $vector encloses the given coordinates.
    * 
    * @param  i   the ''x''-coordinate; tested against the ''x''-axis bounds.
    * @param  j   the ''y''-coordinate; tested against the ''y''-axis bounds.
    * @return `true` if both coordinates are bounded.
    */
  def contains(i: Long, j: Long): Boolean = x.contains(i) && y.contains(j)
  
  def + (that: IntervalZ2): IntervalZ2 =
    new IntervalZ2(x + that.x, y + that.y)
  
  def unary_- : IntervalZ2 =
    new IntervalZ2(-x, -y)
  
  def - (that: IntervalZ2): IntervalZ2 =
    new IntervalZ2(x - that.x, y - that.y)
  
  def :* (scalar: Interval): IntervalZ2 =
    new IntervalZ2(x * scalar, y * scalar)
  
  def *: (scalar: Interval): IntervalZ2 = this :* scalar
  
  /** Returns the intersection of this $vector and another $vector.
    * 
    * @param  that  the $vector to intersect with.
    * @return the vector intersection of each axis.
    */
  def intersect(that: IntervalZ2): IntervalZ2 =
    new IntervalZ2(x intersect that.x, y intersect that.y)
  
  /** Applies a function to each coordinate pair contained by this $vector. */
  @inline final def foreach[@specialized(Unit) U](f: (Long, Long) => U) {
    var i = 0L // reserve jvm local variable
    var j = 0L // reserve jvm local variable
    val x_upper = x.upper
    val x_lower = x.lower
    val y_upper = y.upper
    j = y.lower
    while (j <= y_upper) {
      i = x_lower
      while (i <= x_upper) {
        f(i, j)
        i += 1L
      }
      j += 1L
    }
  }
  
  override def equals(other: Any): Boolean = other match {
    case that: IntervalZ2 => x.equals(that.x) && y.equals(that.y)
    case _ => false
  }
  
  override def hashCode: Int =
    mash(mix(mix(1506359539, x), y))
  
  override def toString: String =
    "IntervalZ2"+"("+ x +", "+ y +")"
}

/** Contains factory methods and implicit conversions for 2D interval vectors. */
object IntervalZ2 {
  def apply(x: Interval, y: Interval): IntervalZ2 =
    new IntervalZ2(x, y)
  
  def unapply(vector: IntervalZ2): Some[(Interval, Interval)] =
    Some(vector.x, vector.y)
  
  /** Returns a 2D interval containing just the given vector. */
  implicit def degenerate(vector: VectorZ2): IntervalZ2 = new IntervalZ2(vector.x, vector.y)
  
  /** The 2D interval vector additive identity. */
  implicit val additiveIdentity = new AdditiveIdentity(new IntervalZ2(Zero, Zero))
  
  /** The default struct for 2D interval vectors. */
  implicit lazy val struct = new StructIntervalZ2
  
  /** A struct for 2D interval vectors. */
  class StructIntervalZ2(frameOffset: Long, frameSize: Long, frameAlignment: Long)
    extends Struct2[Interval, Interval, IntervalZ2](frameOffset, frameSize, frameAlignment) {
    
    import Interval.StructInterval
    
    def this() = this(0L, 0L, 0L)
    
    protected override val field1 = new StructInterval(offset1, size, alignment)
    
    protected override val field2 = new StructInterval(offset2, size, alignment)
    
    /** The `x`-interval field projection of this struct. */
    def x: StructInterval = field1
    
    /** The `y`-interval field projection of this struct. */
    def y: StructInterval = field2
    
    def load(data: Data, address: Long): IntervalZ2 = {
      val x = field1.load(data, address)
      val y = field2.load(data, address)
      new IntervalZ2(x, y)
    }
    
    def store(data: Data, address: Long, vector: IntervalZ2) {
      field1.store(data, address, vector.x)
      field2.store(data, address, vector.y)
    }
    
    override def project(offset: Long, size: Long, alignment: Long): StructIntervalZ2 =
      new StructIntervalZ2(offset1 + offset, size, alignment)
    
    override def toString: String = "StructIntervalZ2"
  }
}
