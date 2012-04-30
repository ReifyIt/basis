/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

class F2[F <: Ring { type Vector = F }] extends Vector2Space {
  override type Vector = VectorF2[F]
  override type Scalar = F
  
  override def apply(x: Scalar, y: Scalar): Vector =
    new Vector(this, x, y)
  
  override def toString: String = "F2"
}
