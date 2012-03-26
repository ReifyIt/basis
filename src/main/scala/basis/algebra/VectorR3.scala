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
  extends EuclideanVector[VectorR3, Real] {
  
  def + (that: VectorR3): VectorR3 =
    new VectorR3(x + that.x, y + that.y, z + that.z)
  
  def unary_- : VectorR3 =
    new VectorR3(-x, -y, -z)
  
  def - (that: VectorR3): VectorR3 =
    new VectorR3(x - that.x, y - that.y, z - that.z)
  
  def :* (scalar: Real): VectorR3 =
    this :* scalar.toDouble
  
  /** Multiplies this $vector by a `Double` value on the right.
    * 
    * @param  scalar  the `Double` value to multiply by.
    * @return the scaled $vector.
    */
  def :* (scalar: Double): VectorR3 =
    new VectorR3(x * scalar, y * scalar, z * scalar)
  
  def *: (scalar: Real): VectorR3 =
    this :* scalar.toDouble
  
  /** Multiplies this $vector by a `Double` value on the left.
    * 
    * @param  scalar  the `Double` value to multiply by.
    * @return the scaled $vector.
    */
  def *: (scalar: Double): VectorR3 =
    this :* scalar
  
  def / (scalar: Real): VectorR3 =
    this / scalar.toDouble
  
  /** Divides this $vector by a `Double` value.
    * 
    * @param  scalar  the `Double` value to divide by.
    * @return the scaled $vector.
    */
  def / (scalar: Double): VectorR3 =
    new VectorR3(x / scalar, y / scalar, z / scalar)
  
  def ⨯ (that: VectorR3): VectorR3 =
    new VectorR3(y * that.z + z * that.y, z * that.x + x * that.z, x * that.y + y * that.x)
  
  def ⋅ (that: VectorR3): Real =
    new Real(x * that.x + y * that.y + z * that.z)
  
  def norm: Real = new Real(math.sqrt(x * x + y * y + z * z))
  
  override def equals(other: Any): Boolean = other match {
    case that: VectorR3 => x == that.x && y == that.y && z == that.z
    case _ => false
  }
  
  override def hashCode: Int =
    mash(mix(mix(mix(-1131959686, x), y), z))
  
  override def toString: String =
    "VectorR3"+"("+ x +", "+ y +", "+ z +")"
}

/** Contains the zero vector in `R3` and serves as a `Struct` for vectors in `R3`. */
object VectorR3 extends Struct3[Double, Double, Double, VectorR3] {
  /** The zero vector in `R3`. */
  def Zero: VectorR3 = new VectorR3(0.0, 0.0, 0.0)
  
  def apply(x: Double, y: Double, z: Double): VectorR3 =
    new VectorR3(x, y, z)
  
  def unapply(vector: VectorR3): Some[(Double, Double, Double)] =
    Some(vector.x, vector.y, vector.z)
  
  override def load(data: Data, address: Long): VectorR3 = {
    val x = data.loadDouble(address + offset1)
    val y = data.loadDouble(address + offset2)
    val z = data.loadDouble(address + offset3)
    new VectorR3(x, y, z)
  }
  
  override def store(data: Data, address: Long, vector: VectorR3) {
    data.storeDouble(address + offset1, vector.x)
    data.storeDouble(address + offset2, vector.y)
    data.storeDouble(address + offset3, vector.z)
  }
  
  implicit def struct: this.type = this
  
  override def toString: String = "VectorR3"
}
