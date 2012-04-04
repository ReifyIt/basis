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

/** A 3-dimensional vector of discrete intervals.
  * 
  * @author Chris Sachs
  * 
  * @constructor Constructs a 3D interval with three coordinate bounds.
  * @param  x   The ''x''-axis bounds.
  * @param  y   The ''y''-axis bounds.
  * @param  z   The ''z''-axis bounds.
  * 
  * @define vector  3D interval vector
  * @define scalar  discrete interval
  */
final class IntervalZ3(val x: Interval, val y: Interval, val z: Interval)
  extends Vector[IntervalZ3, Interval] {
  
  /** Returns the number of values enclosed by this $vector. */
  def size: Long = x.size * y.size
  
  /** Returns `true` if this $vector contains the given vector. */
  def contains(vector: VectorZ3): Boolean =
    contains(vector.x, vector.y, vector.z)
  
  /** Returns `true` if this $vector encloses the given coordinates.
    * 
    * @param  i   the ''x''-coordinate; tested against the ''x''-axis bounds.
    * @param  j   the ''y''-coordinate; tested against the ''y''-axis bounds.
    * @param  k   the ''z''-coordinate; tested against the ''z''-axis bounds.
    * @return `true` if both coordinates are bounded.
    */
  def contains(i: Long, j: Long, k: Long): Boolean =
    x.contains(i) && y.contains(j) && z.contains(k)
  
  def + (that: IntervalZ3): IntervalZ3 =
    new IntervalZ3(x + that.x, y + that.y, z + that.z)
  
  def unary_- : IntervalZ3 =
    new IntervalZ3(-x, -y, -z)
  
  def - (that: IntervalZ3): IntervalZ3 =
    new IntervalZ3(x - that.x, y - that.y, z - that.z)
  
  def :* (scalar: Interval): IntervalZ3 =
    new IntervalZ3(x * scalar, y * scalar, z * scalar)
  
  def *: (scalar: Interval): IntervalZ3 = this :* scalar
  
  /** Returns the intersection of this $vector and another $vector.
    * 
    * @param  that  the $vector to intersect with.
    * @return the vector intersection of each axis.
    */
  def intersect(that: IntervalZ3): IntervalZ3 =
    new IntervalZ3(x intersect that.x, y intersect that.y, z intersect that.z)
  
  /** Applies a function to each coordinate triple contained by this $vector. */
  @inline final def foreach[U](f: (Long, Long, Long) => U) {
    var i = 0L // reserve jvm local variable
    var j = 0L // reserve jvm local variable
    var k = 0L // reserve jvm local variable
    val x_upper = x.upper
    val x_lower = x.lower
    val y_upper = y.upper
    val y_lower = y.lower
    val z_upper = z.upper
    k = z.lower
    while (k <= z_upper) {
      j = y_lower
      while (j <= y_upper) {
        i = x_lower
        while (i <= x_upper) {
          f(i, j, k)
          i += 1L
        }
        j += 1L
      }
      k += 1L
    }
  }
  
  override def equals(other: Any): Boolean = other match {
    case that: IntervalZ3 => x.equals(that.x) && y.equals(that.y) && z.equals(that.z)
    case _ => false
  }
  
  override def hashCode: Int =
    mash(mix(mix(1506359540, x), y))
  
  override def toString: String =
    "IntervalZ3"+"("+ x +", "+ y +", "+ z +")"
}

/** Contains factory methods and implicit conversions for 3D interval vectors. */
object IntervalZ3 {
  def apply(x: Interval, y: Interval, z: Interval): IntervalZ3 =
    new IntervalZ3(x, y, z)
  
  def unapply(vector: IntervalZ3): Some[(Interval, Interval, Interval)] =
    Some(vector.x, vector.y, vector.z)
  
  /** Returns a 3D interval containing just the given vector. */
  implicit def degenerate(vector: VectorZ3): IntervalZ3 =
    new IntervalZ3(vector.x, vector.y, vector.z)
  
  /** The 3D interval vector additive identity typeclass. */
  implicit val additiveIdentity = Zero(apply(Zero, Zero, Zero))
  
  /** The default struct for 3D interval vectors. */
  implicit lazy val struct = new StructIntervalZ3
  
  /** A struct for 3D interval vectors. */
  class StructIntervalZ3(frameOffset: Long, frameSize: Long, frameAlignment: Long)
    extends Struct3[Interval, Interval, Interval, IntervalZ3](frameOffset, frameSize, frameAlignment) {
    
    import Interval.StructInterval
    
    def this() = this(0L, 0L, 0L)
    
    protected override val field1 = new StructInterval(offset1, size, alignment)
    
    protected override val field2 = new StructInterval(offset2, size, alignment)
    
    protected override val field3 = new StructInterval(offset3, size, alignment)
    
    /** The `x`-interval field projection of this struct. */
    def x: StructInterval = field1
    
    /** The `y`-interval field projection of this struct. */
    def y: StructInterval = field2
    
    /** The 'z'-interval field projection of this struct. */
    def z: StructInterval = field3
    
    def load(data: Data, address: Long): IntervalZ3 = {
      val x = field1.load(data, address)
      val y = field2.load(data, address)
      val z = field3.load(data, address)
      new IntervalZ3(x, y, z)
    }
    
    def store(data: Data, address: Long, vector: IntervalZ3) {
      field1.store(data, address, vector.x)
      field2.store(data, address, vector.y)
      field3.store(data, address, vector.z)
    }
    
    override def project(offset: Long, size: Long, alignment: Long): StructIntervalZ3 =
      new StructIntervalZ3(offset1 + offset, size, alignment)
    
    override def toString: String = "StructIntervalZ3"
  }
}
