/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Matrix2x2Space extends SquareMatrixSpace { self =>
  override type Matrix <: Matrix2x2 {
    type Matrix = self.Matrix
    type Vec    = self.Vec
    type Scalar = self.Scalar
  }
  
  override type Vec <: Vector2 {
    type Vector = self.Vec
    type Scalar = self.Scalar
  }
  
  override type Scalar
  
  override def M: Int = 2
  
  override def N: Int = 2
  
  override def apply(entries: TraversableOnce[Scalar]): Matrix = {
    val xs = entries.toSeq
    if (xs.length != 4) throw new DimensionException
    apply(xs(0), xs(1),  xs(2), xs(3))
  }
  
  def apply(
      _1_1: Scalar, _1_2: Scalar,
      _2_1: Scalar, _2_2: Scalar): Matrix
  
  override def rows(vectors: TraversableOnce[Row]): Matrix = {
    val vs = vectors.toSeq
    if (vs.length != 2) throw new DimensionException
    rows(vs(0), vs(1))
  }
  
  def rows(row1: Row, row2: Row): Matrix =
    apply(row1.x, row1.y,
          row2.x, row2.y)
  
  override def cols(vectors: TraversableOnce[Col]): Matrix = {
    val ws = vectors.toSeq
    if (ws.length != 2) throw new DimensionException
    cols(ws(0), ws(1))
  }
  
  def cols(col1: Col, col2: Col): Matrix =
    apply(col1.x, col2.x,
          col1.y, col2.y)
}
