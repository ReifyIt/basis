/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

final class VectorF3[F <: Ring { type Vector = F }] private[generic]
    (val Vector: F3[F], val x: F, val y: F, val z: F)
  extends Vector3Like {
  
  override type Vector = VectorF3[F]
  override type Scalar = F
}
