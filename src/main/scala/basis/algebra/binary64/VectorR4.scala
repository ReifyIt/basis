/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

final class VectorR4(val x: Real, val y: Real, val z: Real, val w: Real)
  extends Vector4Like with RealVectorLike {
  
  override type Vector = VectorR4
  
  override def Vector: R4 = R4
  
  override def N: Int = 4
  
  override def apply(i: Int): Real = i match {
    case 0 => x
    case 1 => y
    case 2 => z
    case 3 => w
    case _ => throw new IndexOutOfBoundsException(i.toString)
  }
  
  override def + (that: Vector): Vector =
    new Vector(x + that.x, y + that.y, z + that.z, w + that.w)
  
  override def unary_- : Vector = new Vector(-x, -y, -z, -w)
  
  override def - (that: Vector): Vector =
    new Vector(x - that.x, y - that.y, z - that.z, w - that.w)
  
  override def :* (scalar: Real): Vector =
    new Vector(x * scalar, y * scalar, z * scalar, w * scalar)
  
  override def *: (scalar: Real): Vector = this :* scalar
  
  override def ⋅ (that: Vector): Real =
    x * that.x + y * that.y + z * that.z + w * that.w
}
