/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.memory._
import basis.util.MurmurHash._

/** A vector in a 4-dimensional `Real` vector space.
  * 
  * @author Chris Sachs
  * 
  * @constructor  Constructs a vector with four `Double` coordinates.
  * @param  x   the ''x''-coordinate.
  * @param  y   the ''y''-coordinate.
  * @param  z   the ''z''-coordinate.
  * @param  w   the ''w''-coordinate.
  * 
  * @define scalar  `Real` value
  */
final class VectorR4(val x: Double, val y: Double, val z: Double, val w: Double)
  extends EuclideanVector[VectorR4, Real] with RealVector[VectorR4] {
  
  def + (that: VectorR4): VectorR4 =
    new VectorR4(x + that.x, y + that.y, z + that.z, w + that.w)
  
  def unary_- : VectorR4 =
    new VectorR4(-x, -y, -z, -w)
  
  def - (that: VectorR4): VectorR4 =
    new VectorR4(x - that.x, y - that.y, z - that.z, w - that.w)
  
  def :* (scalar: Double): VectorR4 =
    new VectorR4(x * scalar, y * scalar, z * scalar, w * scalar)
  
  def *: (scalar: Double): VectorR4 =
    this :* scalar
  
  def / (scalar: Double): VectorR4 =
    new VectorR4(x / scalar, y / scalar, z / scalar, w / scalar)
  
  def ⋅ (that: VectorR4): Real =
    new Real(x * that.x + y * that.y + z * that.z + w * that.w)
  
  def norm: Real = new Real(length)
  
  /** Returns the length of this $vector. */
  def length: Double = math.sqrt(x * x + y * y + z * z + w * w)
  
  override def equals(other: Any): Boolean = other match {
    case that: VectorR4 => x == that.x && y == that.y && z == that.z && w == that.w
    case _ => false
  }
  
  override def hashCode: Int =
    mash(mix(mix(mix(mix(-1131959685, x), y), z), w))
  
  override def toString: String =
    "VectorR4"+"("+ x +", "+ y +", "+ z +", "+ w +")"
}

/** Contains factory methods for vectors in `R4`. Serves as a struct for vectors in `R4`. */
object VectorR4 extends Struct4[Double, Double, Double, Double, VectorR4] {
  /** The zero vector of `R4`. */
  def Zero: VectorR4 = new VectorR4(0.0, 0.0, 0.0, 0.0)
  
  def apply(x: Double, y: Double, z: Double, w: Double): VectorR4 =
    new VectorR4(x, y, z, w)
  
  def unapply(vector: VectorR4): Some[(Double, Double, Double, Double)] =
    Some(vector.x, vector.y, vector.z, vector.w)
  
  def load(data: Data, address: Long): VectorR4 = {
    val x = data.loadDouble(address + offset1)
    val y = data.loadDouble(address + offset2)
    val z = data.loadDouble(address + offset3)
    val w = data.loadDouble(address + offset4)
    new VectorR4(x, y, z, w)
  }
  
  def store(data: Data, address: Long, vector: VectorR4) {
    data.storeDouble(address + offset1, vector.x)
    data.storeDouble(address + offset2, vector.y)
    data.storeDouble(address + offset3, vector.z)
    data.storeDouble(address + offset4, vector.w)
  }
  
  /** The projection of the `x` field of the `VectorR4` struct. */
  def x: Struct[Double] = field1
  
  /** The projection of the `y` field of the `VectorR4` struct. */
  def y: Struct[Double] = field2
  
  /** The projection of the `z` field of the `VectorR4` struct. */
  def z: Struct[Double] = field3
  
  /** The projection of the `w` field of the `VectorR4` struct. */
  def w: Struct[Double] = field4
  
  implicit def struct: this.type = this
  
  override def toString: String = "VectorR4"
}
