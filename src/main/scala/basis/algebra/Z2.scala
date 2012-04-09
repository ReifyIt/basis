/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.util.MurmurHash._

object Z2 extends Module {
  type Scalar = Integer
  
  final class Vector(val x: Long, val y: Long)
    extends IntegerVector[Vector] {
    
    def + (that: Vector): Vector =
      new Vector(x + that.x, y + that.y)
    
    def unary_- : Vector = new Vector(-x, -y)
    
    def - (that: Vector): Vector =
      new Vector(x - that.x, y - that.y)
    
    def :* (scalar: Long): Vector =
      new Vector(x * scalar, y * scalar)
    
    def *: (scalar: Long): Vector = this :* scalar
    
    override def equals(other: Any): Boolean = other match {
      case that: Vector => x == that.x && y == that.y
      case _ => false
    }
    
    override def hashCode: Int =
      mash(mix(mix(1278514179, x), y))
    
    override def toString: String =
      "<"+ x +", "+ y +">"
  }
  
  val Scalar = Integer
  
  val zero: Vector = new Vector(0L, 0L)
  
  def apply(x: Long, y: Long): Vector =
    new Vector(x, y)
  
  def unapply(vector: Vector): Some[(Long, Long)] =
    Some(vector.x, vector.y)
  
  override def toString: String = "Z2"
}
