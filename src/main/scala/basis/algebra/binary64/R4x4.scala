/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

class R4x4 extends Matrix4x4Space with RealMatrixSpace {
  override type Matrix = MatrixR4x4
  override type Vec    = VectorR4
  
  override def apply(entries: Array[Double]): Matrix = {
    if (entries.length != 16) throw new DimensionException
    new Matrix(
      entries( 0), entries( 1), entries( 2), entries( 3),
      entries( 4), entries( 5), entries( 6), entries( 7),
      entries( 8), entries( 9), entries(10), entries(11),
      entries(12), entries(13), entries(14), entries(15))
  }
  
  override def apply(
      _1_1: Real, _1_2: Real, _1_3: Real, _1_4: Real,
      _2_1: Real, _2_2: Real, _2_3: Real, _2_4: Real,
      _3_1: Real, _3_2: Real, _3_3: Real, _3_4: Real,
      _4_1: Real, _4_2: Real, _4_3: Real, _4_4: Real): Matrix =
    new Matrix(
      _1_1, _1_2, _1_3, _1_4,
      _2_1, _2_2, _2_3, _2_4,
      _3_1, _3_2, _3_3, _3_4,
      _4_1, _4_2, _4_3, _4_4)
  
  override def rows(row1: Row, row2: Row, row3: Row, row4: Row): Matrix =
    new Matrix(
      row1.x, row1.y, row1.z, row1.w,
      row2.x, row2.y, row2.z, row2.w,
      row3.x, row3.y, row3.z, row3.w,
      row4.x, row4.y, row4.z, row4.w)
  
  override def cols(col1: Col, col2: Col, col3: Col, col4: Col): Matrix =
    new Matrix(
      col1.x, col2.x, col3.x, col4.x,
      col1.y, col2.y, col3.y, col4.y,
      col1.z, col2.z, col3.z, col4.z,
      col1.w, col2.w, col3.w, col4.w)
  
  override def toString: String = "R4x4"
}
