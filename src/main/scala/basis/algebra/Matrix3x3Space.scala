/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Matrix3x3Space extends SquareMatrixSpace { self =>
  override type Matrix <: Matrix3x3 {
    type Matrix = self.Matrix
    type Vec    = self.Vec
    type Scalar = self.Scalar
  }
  
  override type Vec <: Vector3 {
    type Vector = self.Vec
    type Scalar = self.Scalar
  }
  
  override type Scalar
  
  override def M: Int = 3
  
  override def N: Int = 3
  
  override def apply(entries: TraversableOnce[Scalar]): Matrix = {
    val xs = entries.toSeq
    if (xs.length != 9) throw new DimensionException
    apply(xs(0), xs(1), xs(2),
          xs(3), xs(4), xs(5),
          xs(6), xs(7), xs(8))
  }
  
  def apply(
      _1_1: Scalar, _1_2: Scalar, _1_3: Scalar,
      _2_1: Scalar, _2_2: Scalar, _2_3: Scalar,
      _3_1: Scalar, _3_2: Scalar, _3_3: Scalar): Matrix
  
  override def rows(vectors: TraversableOnce[Row]): Matrix = {
    val vs = vectors.toSeq
    if (vs.length != 3) throw new DimensionException
    rows(vs(0), vs(1), vs(2))
  }
  
  def rows(row1: Row, row2: Row, row3: Row): Matrix =
    apply(row1.x, row1.y, row1.z,
          row2.x, row2.y, row2.z,
          row3.x, row3.y, row3.z)
  
  override def cols(vectors: TraversableOnce[Col]): Matrix = {
    val ws = vectors.toSeq
    if (ws.length != 3) throw new DimensionException
    cols(ws(0), ws(1), ws(2))
  }
  
  def cols(col1: Col, col2: Col, col3: Col): Matrix =
    apply(col1.x, col2.x, col3.x,
          col1.y, col2.y, col3.y,
          col1.z, col2.z, col3.z)
}
