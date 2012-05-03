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
     F <: Ring { type Vector = F }] private
    (val Matrix: MatrixFMxN.Space[V, W, F], entries: Array[AnyRef])
  extends Matrix { self =>
  
  if (entries.length != M * N) throw new DimensionException
  
  override type Matrix = MatrixFMxN[V, W, F]
  override type T      = MatrixFMxN[W, V, F]
  override type Row    = V
  override type Col    = W
  override type Scalar = F
  
  override def apply(k: Int): Scalar = entries(k).asInstanceOf[Scalar]
}

object MatrixFMxN {
  def apply[F <: Ring { type Vector = F }]
      (Row: Vector.Space { type Scalar = F },
       Col: Vector.Space { type Scalar = F },
       Scalar: Ring.Space { type Vector = F }) =
    new Space[Row.type#Vector, Col.type#Vector, F](Row, Col, Scalar)
  
  class Space
      [V <: Vector { type Vector = V; type Scalar = F },
       W <: Vector { type Vector = W; type Scalar = F },
       F <: Ring { type Vector = F }]
      (val Row: Vector.Space { type Vector = V; type Scalar = F },
       val Col: Vector.Space { type Vector = W; type Scalar = F },
       val Scalar: Ring.Space { type Vector = F })
    extends Matrix.Space {
    
    override type Matrix = MatrixFMxN[V, W, F]
    override type T      = MatrixFMxN[W, V, F]
    override type Row    = V
    override type Col    = W
    override type Scalar = F
    
    private var Transpose: MatrixFMxN.Space[W, V, F] = null
    
    override def T: MatrixFMxN.Space[W, V, F] = synchronized {
      if (Transpose == null) {
        Transpose = new MatrixFMxN.Space[W, V, F](Col, Row, Scalar)
        Transpose.Transpose = this
      }
      Transpose
    }
    
    override lazy val zero: Matrix = super.zero
    
    override def apply(entries: TraversableOnce[Scalar]): Matrix =
      new Matrix(this, entries.asInstanceOf[TraversableOnce[AnyRef]].toArray[AnyRef])
    
    def apply(entries: Scalar*): Matrix =
      new Matrix(this, entries.asInstanceOf[Seq[AnyRef]].toArray[AnyRef])
    
    def rows(vectors: Row*): Matrix = super.rows(vectors)
    
    def cols(vectors: Col*): Matrix = super.cols(vectors)
    
    override def toString: String = "("+ Row +" map "+ Col +")"
  }
}
