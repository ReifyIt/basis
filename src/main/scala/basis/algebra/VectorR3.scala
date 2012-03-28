/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.memory._
import basis.util.MurmurHash._

/** A vector in a 3-dimensional `Real` vector space.
  * 
  * @author Chris Sachs
  * 
  * @constructor  Constructs a vector with three `Double` coordinates.
  * @param  x   the ''x''-coordinate.
  * @param  y   the ''y''-coordinate.
  * @param  z   the ''z''-coordinate.
  * 
  * @define scalar  `Real` value
  */
final class VectorR3(val x: Double, val y: Double, val z: Double)
  extends EuclideanVector[VectorR3, Real] with RealVector[VectorR3] {
  
  def + (that: VectorR3): VectorR3 =
    new VectorR3(x + that.x, y + that.y, z + that.z)
  
  def unary_- : VectorR3 =
    new VectorR3(-x, -y, -z)
  
  def - (that: VectorR3): VectorR3 =
    new VectorR3(x - that.x, y - that.y, z - that.z)
  
  def :* (scalar: Double): VectorR3 =
    new VectorR3(x * scalar, y * scalar, z * scalar)
  
  def *: (scalar: Double): VectorR3 =
    this :* scalar
  
  def / (scalar: Double): VectorR3 =
    new VectorR3(x / scalar, y / scalar, z / scalar)
  
  /** Returns the cross product of this $vector and another $vector. The name of
    * this method uses the unicode cross product operator U+2A2F.
    * 
    * @param  that  the $vector to take the cross product with.
    * @return the vector cross product of this $vector and the other $vector.
    */
  def ⨯ (that: VectorR3): VectorR3 =
    new VectorR3(y * that.z + z * that.y, z * that.x + x * that.z, x * that.y + y * that.x)
  
  def ⋅ (that: VectorR3): Real =
    new Real(x * that.x + y * that.y + z * that.z)
  
  def norm: Real = new Real(length)
  
  /** Returns the length of this $vector. */
  def length: Double = math.sqrt(x * x + y * y + z * z)
  
  override def equals(other: Any): Boolean = other match {
    case that: VectorR3 => x == that.x && y == that.y && z == that.z
    case _ => false
  }
  
  override def hashCode: Int =
    mash(mix(mix(mix(-1131959686, x), y), z))
  
  override def toString: String =
    "VectorR3"+"("+ x +", "+ y +", "+ z +")"
}

/** Contains factory methods for vectors in `R3`. Serves as a struct for vectors in `R3`. */
object VectorR3 extends Struct3[Double, Double, Double, VectorR3] {
  /** The zero vector of `R3`. */
  def Zero: VectorR3 = new VectorR3(0.0, 0.0, 0.0)
  
  def apply(x: Double, y: Double, z: Double): VectorR3 =
    new VectorR3(x, y, z)
  
  def unapply(vector: VectorR3): Some[(Double, Double, Double)] =
    Some(vector.x, vector.y, vector.z)
  
  def load(data: Data, address: Long): VectorR3 = {
    val x = data.loadDouble(address + offset1)
    val y = data.loadDouble(address + offset2)
    val z = data.loadDouble(address + offset3)
    new VectorR3(x, y, z)
  }
  
  def store(data: Data, address: Long, vector: VectorR3) {
    data.storeDouble(address + offset1, vector.x)
    data.storeDouble(address + offset2, vector.y)
    data.storeDouble(address + offset3, vector.z)
  }
  
  /** The projection of the `x` field of the `VectorR3` struct. */
  def x: Struct[Double] = field1
  
  /** The projection of the `y` field of the `VectorR3` struct. */
  def y: Struct[Double] = field2
  
  /** The projection of the `z` field of the `VectorR3` struct. */
  def z: Struct[Double] = field3
  
  implicit def struct: this.type = this
  
  override def toString: String = "VectorR3"
}
