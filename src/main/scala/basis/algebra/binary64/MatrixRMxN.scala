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
     W <: RealVector { type Vector = W }] private
    (val Matrix: MatrixRMxN.Space[V, W], entries: Array[Double])
  extends RealMatrix.Template { self =>
  
  if (entries.length != Col.N * Row.N) throw new DimensionException
  
  override type Matrix = MatrixRMxN[V, W]
  override type T      = MatrixRMxN[W, V]
  override type Row    = V
  override type Col    = W
  
  override def Row: RealVector.Space {
    type Vector = self.Row
  } = Matrix.Row
  
  override def Col: RealVector.Space {
    type Vector = self.Col
  } = Matrix.Col
  
  override val M: Int = Col.N
  override val N: Int = Row.N
  
  override def apply(k: Int): Real = entries(k)
}

object MatrixRMxN {
  def apply(Row: RealVector.Space, Col: RealVector.Space) =
    new Space[Row.type#Vector, Col.type#Vector](Row, Col)
  
  class Space
      [V <: RealVector { type Vector = V },
       W <: RealVector { type Vector = W }]
      (val Row: RealVector.Space { type Vector = V },
       val Col: RealVector.Space { type Vector = W })
    extends RealField.Scalar with RealMatrix.Space {
    
    override type Matrix = MatrixRMxN[V, W]
    override type T      = MatrixRMxN[W, V]
    override type Row    = V
    override type Col    = W
    
    private var Transpose: MatrixRMxN.Space[W, V] = null
    
    override def T: MatrixRMxN.Space[W, V] = synchronized {
      if (Transpose == null) {
        Transpose = new MatrixRMxN.Space[W, V](Col, Row)
        Transpose.Transpose = this
      }
      Transpose
    }
    
    override def M: Int = Col.N
    override def N: Int = Row.N
    
    override lazy val zero: Matrix =
      new Matrix(this, new Array[Double](M * N))
    
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
}
