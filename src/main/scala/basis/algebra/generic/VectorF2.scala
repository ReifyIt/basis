/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

final class VectorF2[F <: Ring { type Vector = F }] private[generic]
    (val Vector: F2[F], val x: F, val y: F)
  extends Vector2Like {
  
  override type Vector = VectorF2[F]
  override type Scalar = F
}
