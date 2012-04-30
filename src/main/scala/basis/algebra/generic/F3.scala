/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

class F3[F <: Ring { type Vector = F }] extends Vector3Space {
  override type Vector = VectorF3[F]
  override type Scalar = F
  
  override def apply(x: Scalar, y: Scalar, z: Scalar): Vector =
    new Vector(this, x, y, z)
  
  override def toString: String = "F3"
}
