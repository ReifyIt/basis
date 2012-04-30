/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

class FMxN
    [V <: Vector { type Vector = V; type Scalar = F },
     W <: Vector { type Vector = W; type Scalar = F },
     F <: Ring { type Vector = F }]
    (val Row: VectorSpace { type Vector = V; type Scalar = F },
     val Col: VectorSpace { type Vector = W; type Scalar = F })
  extends MatrixSpace {
  
  override type Matrix = MatrixFMxN[V, W, F]
  override type T      = MatrixFMxN[W, V, F]
  override type Row    = V
  override type Col    = W
  override type Scalar = F
  
  private var Transpose: FMxN[W, V, F] = null
  
  override def T: FMxN[W, V, F] = synchronized {
    if (Transpose == null) {
      Transpose = new FMxN[W, V, F](Col, Row)
      Transpose.Transpose = this
    }
    Transpose
  }
  
  override def M: Int = Col.N
  override def N: Int = Row.N
  
  override def apply(entries: TraversableOnce[Scalar]): Matrix =
    new Matrix(this, entries.asInstanceOf[TraversableOnce[AnyRef]].toArray[AnyRef])
  
  def apply(entries: Scalar*): Matrix =
    new Matrix(this, entries.asInstanceOf[Seq[AnyRef]].toArray[AnyRef])
  
  def rows(vectors: Row*): Matrix = super.rows(vectors)
  
  def cols(vectors: Col*): Matrix = super.cols(vectors)
  
  override def toString: String = "("+ Row +" map "+ Col +")"
}
