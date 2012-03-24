/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.memory._
import basis.util.MurmurHash._

final class VectorR2(val x: Double, val y: Double)
  extends EuclideanVector[VectorR2, Real] {
  
  def + (that: VectorR2): VectorR2 =
    new VectorR2(x + that.x, y + that.y)
  
  def unary_- : VectorR2 =
    new VectorR2(-x, -y)
  
  def - (that: VectorR2): VectorR2 =
    new VectorR2(x - that.x, y - that.y)
  
  def :* (scalar: Real): VectorR2 =
    this :* scalar.toDouble
  
  def :* (scalar: Double): VectorR2 =
    new VectorR2(x * scalar, y * scalar)
  
  def *: (scalar: Real): VectorR2 =
    this :* scalar.toDouble
  
  def *: (scalar: Double): VectorR2 =
    this :* scalar
  
  def / (scalar: Real): VectorR2 =
    this / scalar.toDouble
  
  def / (scalar: Double): VectorR2 =
    new VectorR2(x / scalar, y / scalar)
  
  def ⋅ (that: VectorR2): Real =
    new Real(x * that.x + y * that.y)
  
  def norm: Real = new Real(math.sqrt(x * x + y * y))
  
  override def equals(other: Any): Boolean = other match {
    case that: VectorR2 => x == that.x && y == that.y
    case _ => false
  }
  
  override def hashCode: Int =
    mash(mix(mix(-1131959687, x), y))
  
  override def toString: String =
    "VectorR2"+"("+ x +", "+ y +")"
}

object VectorR2 extends Struct2[Double, Double, VectorR2] {
  def Zero = new VectorR2(0.0, 0.0)
  
  def apply(x: Double, y: Double): VectorR2 =
    new VectorR2(x, y)
  
  def unapply(vector: VectorR2): Some[(Double, Double)] =
    Some(vector.x, vector.y)
  
  override def load(data: Data, address: Long): VectorR2 = {
    val x = data.loadDouble(address + offset1)
    val y = data.loadDouble(address + offset2)
    new VectorR2(x, y)
  }
  
  override def store(data: Data, address: Long, vector: VectorR2) {
    data.storeDouble(address + offset1, vector.x)
    data.storeDouble(address + offset2, vector.y)
  }
  
  implicit def struct = this
  
  override def toString = "VectorR2"
}
