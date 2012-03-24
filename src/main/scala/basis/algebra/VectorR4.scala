/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.memory._
import basis.util.MurmurHash._

final class VectorR4(val x: Double, val y: Double, val z: Double, val w: Double)
  extends EuclideanVector[VectorR4, Real] {
  
  def + (that: VectorR4): VectorR4 =
    new VectorR4(x + that.x, y + that.y, z + that.z, w + that.w)
  
  def unary_- : VectorR4 =
    new VectorR4(-x, -y, -z, -w)
  
  def - (that: VectorR4): VectorR4 =
    new VectorR4(x - that.x, y - that.y, z - that.z, w - that.w)
  
  def :* (scalar: Real): VectorR4 =
    this :* scalar.toDouble
  
  def :* (scalar: Double): VectorR4 =
    new VectorR4(x * scalar, y * scalar, z * scalar, w * scalar)
  
  def *: (scalar: Real): VectorR4 =
    this :* scalar.toDouble
  
  def *: (scalar: Double): VectorR4 =
    this :* scalar
  
  def / (scalar: Real): VectorR4 =
    this / scalar.toDouble
  
  def / (scalar: Double): VectorR4 =
    new VectorR4(x / scalar, y / scalar, z / scalar, w / scalar)
  
  def ⋅ (that: VectorR4): Real =
    new Real(x * that.x + y * that.y + z * that.z + w * that.w)
  
  def norm: Real = new Real(math.sqrt(x * x + y * y + z * z + w * w))
  
  override def equals(other: Any): Boolean = other match {
    case that: VectorR4 => x == that.x && y == that.y && z == that.z && w == that.w
    case _ => false
  }
  
  override def hashCode: Int =
    mash(mix(mix(mix(mix(-1131959685, x), y), z), w))
  
  override def toString: String =
    "VectorR4"+"("+ x +", "+ y +", "+ z +", "+ w +")"
}

object VectorR4 extends Struct4[Double, Double, Double, Double, VectorR4] {
  def Zero = new VectorR4(0.0, 0.0, 0.0, 0.0)
  
  def apply(x: Double, y: Double, z: Double, w: Double): VectorR4 =
    new VectorR4(x, y, z, w)
  
  def unapply(vector: VectorR4): Some[(Double, Double, Double, Double)] =
    Some(vector.x, vector.y, vector.z, vector.w)
  
  override def load(data: Data, address: Long): VectorR4 = {
    val x = data.loadDouble(address + offset1)
    val y = data.loadDouble(address + offset2)
    val z = data.loadDouble(address + offset3)
    val w = data.loadDouble(address + offset4)
    new VectorR4(x, y, z, w)
  }
  
  override def store(data: Data, address: Long, vector: VectorR4) {
    data.storeDouble(address + offset1, vector.x)
    data.storeDouble(address + offset2, vector.y)
    data.storeDouble(address + offset3, vector.z)
    data.storeDouble(address + offset4, vector.w)
  }
  
  implicit def struct = this
  
  override def toString = "VectorR4"
}
