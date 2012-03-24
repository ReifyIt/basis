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
  * @define vector  $matrix
  * @define matrix  matrix
  * 
  * @tparam Matrix        The `Matrix` type of the matrix space.
  * @tparam Transpose     The `Transpose` type of the matrix space.
  * @tparam ColumnVector  The `Vector` type of the column space.
  * @tparam RowVector     The `Vector` type of the row space.
  * @tparam Scalar        The `Scalar` type of this matrix space.
  */
trait Matrix[Matrix, Transpose, ColumnVector, RowVector, -Scalar] extends Vector[Matrix, Scalar] {
  /** Multiplies a vector in the row space.
    * 
    * @param  column  the vector to multiply, treated as a column vector.
    * @return a vector in the column space.
    */
  def :⋅ (column: RowVector): ColumnVector
  
  /** Multiples a vector in the column space.
    * 
    * @param  row     the vector to multiply, treated as a row vector.
    * @return a vector in the row space.
    */
  def ⋅: (row: ColumnVector): RowVector
  
  /** Transposes this $matrix. */
  def transpose: Transpose
}
