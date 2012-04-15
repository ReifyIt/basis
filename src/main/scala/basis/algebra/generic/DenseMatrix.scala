/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

final class DenseMatrix[V <: VectorFN { type Vector = V; type Scalar = S },
                        W <: VectorFN { type Vector = W; type Scalar = S },
                        S <: Ring { type Scalar = S }]
    (val Space: DenseMatrixModule[V, W, S] with Singleton)
    (entries: Array[AnyRef])
  extends MatrixFMxN {
  
  type Space = DenseMatrixModule[V, W, S] with Singleton
  type Matrix = DenseMatrix[V, W, S]
  type Transpose = DenseMatrix[W, V, S]
  type RowVector = V
  type ColumnVector = W
  type Scalar = S
  
  def entry(k: Int): Scalar = entries(k).asInstanceOf[Scalar]
}
