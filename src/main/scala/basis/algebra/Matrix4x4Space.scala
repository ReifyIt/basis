/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Matrix4x4Space extends SquareMatrixSpace { self =>
  override type Matrix <: Matrix4x4 {
    type Matrix = self.Matrix
    type Vec    = self.Vec
    type Scalar = self.Scalar
  }
  
  override type Vec <: Vector4 {
    type Vector = self.Vec
    type Scalar = self.Scalar
  }
  
  override type Scalar
  
  override def M: Int = 4
  
  override def N: Int = 4
  
  override def apply(entries: TraversableOnce[Scalar]): Matrix = {
    val xs = entries.toSeq
    if (xs.length != 16) throw new DimensionException
    apply(xs( 0), xs( 1), xs( 2), xs( 3),
          xs( 4), xs( 5), xs( 6), xs( 7),
          xs( 8), xs( 9), xs(10), xs(11),
          xs(12), xs(13), xs(14), xs(15))
  }
  
  def apply(
      _1_1: Scalar, _1_2: Scalar, _1_3: Scalar, _1_4: Scalar,
      _2_1: Scalar, _2_2: Scalar, _2_3: Scalar, _2_4: Scalar,
      _3_1: Scalar, _3_2: Scalar, _3_3: Scalar, _3_4: Scalar,
      _4_1: Scalar, _4_2: Scalar, _4_3: Scalar, _4_4: Scalar): Matrix
  
  override def rows(vectors: TraversableOnce[Row]): Matrix = {
    val vs = vectors.toSeq
    if (vs.length != 4) throw new DimensionException
    rows(vs(0), vs(1), vs(2), vs(3))
  }
  
  def rows(row1: Row, row2: Row, row3: Row, row4: Row): Matrix =
    apply(row1.x, row1.y, row1.z, row1.w,
          row2.x, row2.y, row2.z, row2.w,
          row3.x, row3.y, row3.z, row3.w,
          row4.x, row4.y, row4.z, row4.w)
  
  override def cols(vectors: TraversableOnce[Col]): Matrix = {
    val ws = vectors.toSeq
    if (ws.length != 4) throw new DimensionException
    cols(ws(0), ws(1), ws(2), ws(3))
  }
  
  def cols(col1: Col, col2: Col, col3: Col, col4: Col): Matrix =
    apply(col1.x, col2.x, col3.x, col4.x,
          col1.y, col2.y, col3.y, col4.y,
          col1.z, col2.z, col3.z, col4.z,
          col1.w, col2.w, col3.w, col4.w)
}
