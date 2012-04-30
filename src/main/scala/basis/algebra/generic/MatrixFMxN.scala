/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

final class MatrixFMxN
    [V <: Vector { type Vector = V; type Scalar = F },
     W <: Vector { type Vector = W; type Scalar = F },
     F <: Ring { type Vector = F }] private[generic]
    (val Matrix: FMxN[V, W, F], entries: Array[AnyRef])
  extends MatrixLike { self =>
  
  if (entries.length != Col.N * Row.N) throw new DimensionException
  
  override type Matrix = MatrixFMxN[V, W, F]
  override type T      = MatrixFMxN[W, V, F]
  override type Row    = V
  override type Col    = W
  override type Scalar = F
  
  override def Row: VectorSpace {
    type Vector = self.Row
    type Scalar = self.Scalar
  } = Matrix.Row
  
  override def Col: VectorSpace {
    type Vector = self.Col
    type Scalar = self.Scalar
  } = Matrix.Col
  
  override val M: Int = Col.N
  override val N: Int = Row.N
  
  override def apply(k: Int): Scalar = entries(k).asInstanceOf[Scalar]
}
