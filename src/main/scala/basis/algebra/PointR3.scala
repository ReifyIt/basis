/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.memory._
import basis.util.MurmurHash._

/** A point in a 3-dimensional `Real` affine space.
  * 
  * @author Chris Sachs
  * 
  * @constructor  Constructs a point with three `Double` coordinates.
  * @param  x   the ''x''-coordinate.
  * @param  y   the ''y''-coordinate.
  * @param  z   the ''z''-coordinate.
  * 
  * @define scalar  `Real` value
  */
final class PointR3(val x: Double, val y: Double, val z: Double)
  extends AffinePoint[PointR3, VectorR3, Real] {
  
  def :+ (vector: VectorR3): PointR3 =
    new PointR3(x + vector.x, y + vector.y, z + vector.z)
  
  def :- (vector: VectorR3): PointR3 =
    new PointR3(x - vector.x, y - vector.y, z - vector.z)
  
  def - (that: PointR3): VectorR3 =
    new VectorR3(x - that.x, y - that.y, z - that.z)
  
  override def equals(other: Any): Boolean = other match {
    case that: PointR3 => x == that.x && y == that.y && z == that.z
    case _ => false
  }
  
  override def hashCode: Int =
    mash(mix(mix(mix(-1606195941, x), y), z))
  
  override def toString: String =
    "PointR3"+"("+ x +", "+ y +", "+ z +")"
}

/** Contains factory methods for points in `R3`. Serves as a struct for points in `R3`. */
object PointR3 extends Struct3[Double, Double, Double, PointR3] {
   /** the origin of `R3`. */
  def Origin: PointR3 = new PointR3(0.0, 0.0, 0.0)
  
  def apply(x: Double, y: Double, z: Double): PointR3 =
    new PointR3(x, y, z)
  
  def unapply(point: PointR3): Some[(Double, Double, Double)] =
    Some(point.x, point.y, point.z)
  
  def load(data: Data, address: Long): PointR3 = {
    val x = data.loadDouble(address + offset1)
    val y = data.loadDouble(address + offset2)
    val z = data.loadDouble(address + offset3)
    new PointR3(x, y, z)
  }
  
  def store(data: Data, address: Long, point: PointR3) {
    data.storeDouble(address + offset1, point.x)
    data.storeDouble(address + offset2, point.y)
    data.storeDouble(address + offset3, point.z)
  }
  
  /** The projection of the `x` field of `PointR3` values. */
  def x: Struct[Double] = field1
  
  /** The projection of the `y` field of `PointR3` values. */
  def y: Struct[Double] = field2
  
  /** The projection of the `z` field of `PointR3` values. */
  def z: Struct[Double] = field3
  
  implicit def struct: this.type = this
  
  override def toString: String = "PointR3"
}
