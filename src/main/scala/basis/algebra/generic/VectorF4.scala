/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

final class VectorF4[F <: Ring { type Vector = F }] private[generic]
    (val Vector: F4[F], val x: F, val y: F, val z: F, val w: F)
  extends Vector4Like {
  
  override type Vector = VectorF4[F]
  override type Scalar = F
}
