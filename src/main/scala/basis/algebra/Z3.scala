/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.util.MurmurHash._

object Z3 extends Module {
  type Scalar = Integer
  
  final class Vector(val x: Long, val y: Long, val z: Long)
    extends IntegerVector[Vector] {
    
    def + (that: Vector): Vector =
      new Vector(x + that.x, y + that.y, z + that.z)
    
    def unary_- : Vector = new Vector(-x, -y, -z)
    
    def - (that: Vector): Vector =
      new Vector(x - that.x, y - that.y, z - that.z)
    
    def :* (scalar: Long): Vector =
      new Vector(x * scalar, y * scalar, z * scalar)
    
    def *: (scalar: Long): Vector = this :* scalar
    
    override def equals(other: Any): Boolean = other match {
      case that: Vector => x == that.x && y == that.y && z == that.z
      case _ => false
    }
    
    override def hashCode: Int =
      mash(mix(mix(mix(-1273642782, x), y), z))
    
    override def toString: String =
      "<"+ x +", "+ y +", "+ z +">"
  }
  
  val zero: Vector = new Vector(0L, 0L, 0L)
  
  def apply(x: Long, y: Long, z: Long): Vector =
    new Vector(x, y, z)
  
  def unapply(vector: Vector): Some[(Long, Long, Long)] =
    Some(vector.x, vector.y, vector.z)
  
  override def toString: String = "Z3"
}
