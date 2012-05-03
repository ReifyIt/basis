/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

final class VectorZ2(val x: Integer, val y: Integer)
  extends Vector2 with IntegerVector {
  
  override type Vector = VectorZ2
  
  override def Vector = VectorZ2
  
  override def N: Int = 2
  
  override def apply(i: Int): Integer = i match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException(i.toString)
  }
  
  override def + (that: Vector): Vector =
    new Vector(x + that.x, y + that.y)
  
  override def unary_- : Vector = new Vector(-x, -y)
  
  override def - (that: Vector): Vector =
    new Vector(x - that.x, y - that.y)
  
  override def :* (scalar: Integer): Vector =
    new Vector(x * scalar, y * scalar)
  
  override def *: (scalar: Integer): Vector = this :* scalar
  
  override def ⋅ (that: Vector): Integer =
    x * that.x + y * that.y
}

object VectorZ2 extends OrderedRing.Scalar with Affine.Space with Vector2.Space with IntegerVector.Space {
  override type Point  = Vector
  override type Vector = VectorZ2
  
  override def zero: Vector = new Vector(0L, 0L)
  
  override def apply(coords: Array[Long]): Vector = {
    if (coords.length != 2) throw new DimensionException
    new Vector(coords(0), coords(1))
  }
  
  override def apply(x: Integer, y: Integer): Vector =
    new Vector(x, y)
  
  override def toString: String = "Z2"
}
