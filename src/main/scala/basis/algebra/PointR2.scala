/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.memory._
import basis.util.MurmurHash._

/** A point in a 2-dimensional `Real` affine space.
  * 
  * @author Chris Sachs
  * 
  * @constructor Constructs a point with two `Double` coordinates.
  * @param  x   The ''x''-coordinate.
  * @param  y   The ''y''-coordinate.
  * 
  * @define scalar  `Real` value
  */
final class PointR2(val x: Double, val y: Double)
  extends AffinePoint[PointR2, VectorR2, Real] {
  
  def :+ (vector: VectorR2): PointR2 =
    new PointR2(x + vector.x, y + vector.y)
  
  def :- (vector: VectorR2): PointR2 =
    new PointR2(x - vector.x, y - vector.y)
  
  def - (that: PointR2): VectorR2 =
    new VectorR2(x - that.x, y - that.y)
  
  override def equals(other: Any): Boolean = other match {
    case that: PointR2 => x == that.x && y == that.y
    case _ => false
  }
  
  override def hashCode: Int =
    mash(mix(mix(-1606195942, x), y))
  
  override def toString: String =
    "PointR2"+"("+ x +", "+ y +")"
}

/** Contains factory methods for points in `R2`. */
object PointR2 {
  def apply(x: Double, y: Double): PointR2 =
    new PointR2(x, y)
  
  def unapply(point: PointR2): Some[(Double, Double)] =
    Some(point.x, point.y)
  
  /** The default struct for points in `R2`. */
  implicit lazy val struct = new StructPointR2
  
  /** A struct for points in `R2`. */
  class StructPointR2(frameOffset: Long, frameSize: Long, frameAlignment: Long)
    extends Struct2[Double, Double, PointR2](frameOffset, frameSize, frameAlignment) {
    
    def this() = this(0L, 0L, 0L)
    
    /** The `x` field projection of this struct. */
    def x: Struct[Double] = field1
    
    /** The `y` field projection of this struct. */
    def y: Struct[Double] = field2
    
    def load(data: Data, address: Long): PointR2 = {
      val x = data.loadDouble(address + offset1)
      val y = data.loadDouble(address + offset2)
      new PointR2(x, y)
    }
    
    def store(data: Data, address: Long, point: PointR2) {
      data.storeDouble(address + offset1, point.x)
      data.storeDouble(address + offset2, point.y)
    }
    
    override def project(offset: Long, size: Long, alignment: Long): StructPointR2 =
      new StructPointR2(offset1 + offset, size, alignment)
    
    override def toString: String = "StructPointR2"
  }
}
