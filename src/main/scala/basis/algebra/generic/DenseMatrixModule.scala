/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

class DenseMatrixModule[V <: VectorFN { type Vector = V; type Scalar = S },
                        W <: VectorFN { type Vector = W; type Scalar = S },
                        S <: Ring { type Scalar = S }](
    val Row: FN { type Vector = V; type Scalar = S },
    val Column: FN { type Vector = W; type Scalar = S })
  extends FMxN { self =>
  
  type Matrix = DenseMatrix[V, W, S]
  type Transpose = DenseMatrix[W, V, S]
  type RowVector = V
  type ColumnVector = W
  type Scalar = S
  
  lazy val Transpose = new DenseMatrixModule[W, V, S](Column, Row)
  
  lazy val Scalar: ScalarModule { type Scalar = self.Scalar } = Row.Scalar
  
  def apply(entries: TraversableOnce[Scalar]): Matrix =
    new DenseMatrix[V, W, S](this)(entries.toArray[AnyRef])
  
  def apply(entries: Scalar*): Matrix =
    new DenseMatrix[V, W, S](this)(entries.toArray[AnyRef])
  
  override def toString: String =
    "("+ Row +" => "+ Column +")"
}
