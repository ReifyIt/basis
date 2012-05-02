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
  extends Matrix.Template { self =>
  
  if (entries.length != Col.N * Row.N) throw new DimensionException
  
  override type Matrix = MatrixFMxN[V, W, F]
  override type T      = MatrixFMxN[W, V, F]
  override type Row    = V
  override type Col    = W
  override type Scalar = F
  
  override def Row: Vector.Space {
    type Vector = self.Row
    type Scalar = self.Scalar
  } = Matrix.Row
  
  override def Col: Vector.Space {
    type Vector = self.Col
    type Scalar = self.Scalar
  } = Matrix.Col
  
  override val M: Int = Col.N
  override val N: Int = Row.N
  
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
    extends Ring.Scalar with Matrix.Space {
    
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
    
    override def M: Int = Col.N
    override def N: Int = Row.N
    
    lazy val zero: Matrix = {
      val z = Scalar.zero.asInstanceOf[AnyRef]
      val entries = new Array[AnyRef](M * N)
      var i = 0
      while (i < entries.length) {
        entries(i) = z
        i += 1
      }
      apply(wrapRefArray(entries).asInstanceOf[Seq[Scalar]]: _*)
    }
    
    override def apply(entries: TraversableOnce[Scalar]): Matrix =
      new Matrix(this, entries.asInstanceOf[TraversableOnce[AnyRef]].toArray[AnyRef])
    
    def apply(entries: Scalar*): Matrix =
      new Matrix(this, entries.asInstanceOf[Seq[AnyRef]].toArray[AnyRef])
    
    def rows(vectors: Row*): Matrix = super.rows(vectors)
    
    def cols(vectors: Col*): Matrix = super.cols(vectors)
    
    override def toString: String = "("+ Row +" map "+ Col +")"
  }
}
