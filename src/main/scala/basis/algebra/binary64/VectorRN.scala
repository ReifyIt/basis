/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

final class VectorRN private[binary64]
    (val Vector: RN, coords: Array[Double])
  extends RealVectorLike {
  
  if (coords.length != Vector.N) throw new DimensionException
  
  override type Vector = VectorRN
  
  override def N: Int = coords.length
  
  override def apply(i: Int): Real = coords(i)
}
