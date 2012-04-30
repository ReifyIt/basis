/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

class R2x2 extends Matrix2x2Space with RealMatrixSpace {
  override type Matrix = MatrixR2x2
  override type Vec    = VectorR2
  
  override def apply(entries: Array[Double]): Matrix = {
    if (entries.length != 4) throw new DimensionException
    new Matrix(
      entries(0), entries(1),
      entries(2), entries(3))
  }
  
  override def apply(
      _1_1: Real, _1_2: Real,
      _2_1: Real, _2_2: Real): Matrix =
    new Matrix(
      _1_1, _1_2,
      _2_1, _2_2)
  
  override def rows(row1: Row, row2: Row): Matrix =
    new Matrix(
      row1.x, row1.y,
      row2.x, row2.y)
  
  override def cols(col1: Col, col2: Col): Matrix =
    new Matrix(
      col1.x, col2.x,
      col1.y, col2.y)
  
  override def toString: String = "R2x2"
}
