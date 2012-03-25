/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** An element of a matrix space.
  * 
  * @author Chris Sachs
  * 
  * @tparam Matrix        the matrix type of the matrix space.
  * @tparam Transpose     the transpose matrix type of the matrix space.
  * @tparam ColumnVector  the vector type of the column space.
  * @tparam RowVector     the vector type of the row space.
  * @tparam Scalar        the scalar type of the matrix space.
  * 
  * @define vector  $matrix
  * @define matrix  matrix
  */
trait Matrix[Matrix, Transpose, ColumnVector, RowVector, -Scalar] extends Vector[Matrix, Scalar] {
  /** Multiplies a vector in the row space. Multiplies the vector on the right.
    * The method name uses the unicode dot operator U+22C5 to differentiate
    * vector multiplication from scalar multiplication.
    * 
    * @param  column  the vector to transform; treated as a column vector.
    * @return the transformed vector in the column space.
    */
  def :⋅ (column: RowVector): ColumnVector
  
  /** Multiples a vector in the column space. Multiplies the vector on the left.
    * The method name uses the unicode dot operator U+22C5 to differentiate
    * vector multiplication from scalar multiplication.
    * 
    * @param  row     the vector to transform; treated as a row vector.
    * @return the transformed vector in the row space.
    */
  def ⋅: (row: ColumnVector): RowVector
  
  /** Transposes this $matrix. */
  def transpose: Transpose
}
