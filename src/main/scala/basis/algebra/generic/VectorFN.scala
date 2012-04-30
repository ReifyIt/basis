/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

final class VectorFN[F <: Ring { type Vector = F }] private[generic]
    (val Vector: FN[F], coords: Array[AnyRef])
  extends VectorLike {
  
  if (coords.length != Vector.N) throw new DimensionException
  
  override type Vector = VectorFN[F]
  override type Scalar = F
  
  override def N: Int = coords.length
  
  override def apply(i: Int): Scalar = coords(i).asInstanceOf[Scalar]
}
