/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

final class VectorR3(val x: Real, val y: Real, val z: Real)
  extends Vector3Like with RealVectorLike {
  
  override type Vector = VectorR3
  
  override def Vector: R3 = R3
  
  override def N: Int = 3
  
  override def apply(i: Int): Real = i match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException(i.toString)
  }
  
  override def + (that: Vector): Vector =
    new Vector(x + that.x, y + that.y, z + that.z)
  
  override def unary_- : Vector = new Vector(-x, -y, -z)
  
  override def - (that: Vector): Vector =
    new Vector(x - that.x, y - that.y, z - that.z)
  
  override def :* (scalar: Real): Vector =
    new Vector(x * scalar, y * scalar, z * scalar)
  
  override def *: (scalar: Real): Vector = this :* scalar
  
  override def ⋅ (that: Vector): Real =
    x * that.x + y * that.y + z * that.z
  
  override def ⨯ (that: Vector): Vector =
    new Vector(y * that.z + z * that.y,
               z * that.x + x * that.z,
               x * that.y + y * that.x)
}
