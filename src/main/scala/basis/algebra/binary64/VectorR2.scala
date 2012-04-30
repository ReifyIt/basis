/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

final class VectorR2(val x: Real, val y: Real)
  extends Vector2Like with RealVectorLike {
  
  override type Vector = VectorR2
  
  override def Vector: R2 = R2
  
  override def N: Int = 2
  
  override def apply(i: Int): Real = i match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException(i.toString)
  }
  
  override def + (that: Vector): Vector =
    new Vector(x + that.x, y + that.y)
  
  override def unary_- : Vector = new Vector(-x, -y)
  
  override def - (that: Vector): Vector =
    new Vector(x - that.x, y - that.y)
  
  override def :* (scalar: Real): Vector =
    new Vector(x * scalar, y * scalar)
  
  override def *: (scalar: Real): Vector = this :* scalar
  
  override def ⋅ (that: Vector): Real =
    x * that.x + y * that.y
}
