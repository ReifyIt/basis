/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.memory._
import basis.util.MurmurHash._

/** A vector in a 2-dimensional `Real` vector space.
  * 
  * @author Chris Sachs
  * 
  * @constructor  Constructs a vector with two `Double` coordinates.
  * @param  x   the ''x''-coordinate.
  * @param  y   the ''y''-coordinate.
  * 
  * @define scalar  `Real` value
  */
final class VectorR2(val x: Double, val y: Double)
  extends EuclideanVector[VectorR2, Real] with RealVector[VectorR2] {
  
  def + (that: VectorR2): VectorR2 =
    new VectorR2(x + that.x, y + that.y)
  
  def unary_- : VectorR2 =
    new VectorR2(-x, -y)
  
  def - (that: VectorR2): VectorR2 =
    new VectorR2(x - that.x, y - that.y)
  
  def :* (scalar: Double): VectorR2 =
    new VectorR2(x * scalar, y * scalar)
  
  def *: (scalar: Double): VectorR2 =
    this :* scalar
  
  def / (scalar: Double): VectorR2 =
    new VectorR2(x / scalar, y / scalar)
  
  def ⋅ (that: VectorR2): Real =
    new Real(x * that.x + y * that.y)
  
  def norm: Real = new Real(length)
  
  /** Returns the length of this $vector. */
  def length: Double = math.sqrt(x * x + y * y)
  
  override def equals(other: Any): Boolean = other match {
    case that: VectorR2 => x == that.x && y == that.y
    case _ => false
  }
  
  override def hashCode: Int =
    mash(mix(mix(-1131959687, x), y))
  
  override def toString: String =
    "VectorR2"+"("+ x +", "+ y +")"
}

/** Contains the zero vector of `R2` and serves as a struct for vectors in `R2`. */
object VectorR2 extends Struct2[Double, Double, VectorR2] {
  /** The zero vector of `R2`. */
  def Zero: VectorR2 = new VectorR2(0.0, 0.0)
  
  def apply(x: Double, y: Double): VectorR2 =
    new VectorR2(x, y)
  
  def unapply(vector: VectorR2): Some[(Double, Double)] =
    Some(vector.x, vector.y)
  
  def load(data: Data, address: Long): VectorR2 = {
    val x = data.loadDouble(address + offset1)
    val y = data.loadDouble(address + offset2)
    new VectorR2(x, y)
  }
  
  def store(data: Data, address: Long, vector: VectorR2) {
    data.storeDouble(address + offset1, vector.x)
    data.storeDouble(address + offset2, vector.y)
  }
  
  implicit def struct: this.type = this
  
  override def toString: String = "VectorR2"
}
