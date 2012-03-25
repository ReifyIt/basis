/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.memory._
import basis.util.MurmurHash._

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

object PointR2 extends Struct2[Double, Double, PointR2] {
  def Origin: PointR2 = new PointR2(0.0, 0.0)
  
  def apply(x: Double, y: Double): PointR2 =
    new PointR2(x, y)
  
  def unapply(point: PointR2): Some[(Double, Double)] =
    Some(point.x, point.y)
  
  override def load(data: Data, address: Long): PointR2 = {
    val x = data.loadDouble(address + offset1)
    val y = data.loadDouble(address + offset2)
    new PointR2(x, y)
  }
  
  override def store(data: Data, address: Long, point: PointR2) {
    data.storeDouble(address + offset1, point.x)
    data.storeDouble(address + offset2, point.y)
  }
  
  implicit def struct: this.type = this
  
  override def toString: String = "PointR2"
}
