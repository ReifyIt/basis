/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

final class MatrixRMxN
    [V <: RealVector { type Vector = V },
     W <: RealVector { type Vector = W }] private[binary64]
    (val Matrix: RMxN[V, W], entries: Array[Double])
  extends RealMatrixLike { self =>
  
  if (entries.length != Col.N * Row.N) throw new DimensionException
  
  override type Matrix = MatrixRMxN[V, W]
  override type T      = MatrixRMxN[W, V]
  override type Row    = V
  override type Col    = W
  
  override def Row: RealVectorSpace {
    type Vector = self.Row
  } = Matrix.Row
  
  override def Col: RealVectorSpace {
    type Vector = self.Col
  } = Matrix.Col
  
  override val M: Int = Col.N
  override val N: Int = Row.N
  
  override def apply(k: Int): Real = entries(k)
}
