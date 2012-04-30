/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

class RMxN
    [V <: RealVector { type Vector = V },
     W <: RealVector { type Vector = W }]
    (val Row: RealVectorSpace { type Vector = V },
     val Col: RealVectorSpace { type Vector = W })
  extends RealMatrixSpace {
  
  override type Matrix = MatrixRMxN[V, W]
  override type T      = MatrixRMxN[W, V]
  override type Row    = V
  override type Col    = W
  
  private var Transpose: RMxN[W, V] = null
  
  override def T: RMxN[W, V] = synchronized {
    if (Transpose == null) {
      Transpose = new RMxN[W, V](Col, Row)
      Transpose.Transpose = this
    }
    Transpose
  }
  
  override def M: Int = Col.N
  override def N: Int = Row.N
  
  override def apply(entries: TraversableOnce[Real]): Matrix =
    new Matrix(this, entries.map(_.toDouble).toArray[Double])
  
  override def apply(entries: Array[Double]): Matrix =
    new Matrix(this, entries)
  
  def apply(entries: Double*): Matrix =
    new Matrix(this, entries.toArray[Double])
  
  def rows(vectors: Row*): Matrix = super.rows(vectors)
  
  def cols(vectors: Col*): Matrix = super.cols(vectors)
  
  override def toString: String = "("+ Row +" map "+ Col +")"
}
