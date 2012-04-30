/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

class R3x3 extends Matrix3x3Space with RealMatrixSpace {
  override type Matrix = MatrixR3x3
  override type Vec    = VectorR3
  
  override def apply(entries: Array[Double]): Matrix = {
    if (entries.length != 9) throw new DimensionException
    new Matrix(
      entries(0), entries(1), entries(2),
      entries(3), entries(4), entries(5),
      entries(6), entries(7), entries(8))
  }
  
  override def apply(
      _1_1: Real, _1_2: Real, _1_3: Real,
      _2_1: Real, _2_2: Real, _2_3: Real,
      _3_1: Real, _3_2: Real, _3_3: Real): Matrix =
    new Matrix(
      _1_1, _1_2, _1_3,
      _2_1, _2_2, _2_3,
      _3_1, _3_2, _3_3)
  
  override def rows(row1: Row, row2: Row, row3: Row): Matrix =
    new Matrix(
      row1.x, row1.y, row1.z,
      row2.x, row2.y, row2.z,
      row3.x, row3.y, row3.z)
  
  override def cols(col1: Col, col2: Col, col3: Col): Matrix =
    new Matrix(
      col1.x, col2.x, col3.x,
      col1.y, col2.y, col3.y,
      col1.z, col2.z, col3.z)
  
  override def toString: String = "R3x3"
}
