/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** A matrix in a square matrix space.
  * 
  * @author Chris Sachs
  * 
  * @tparam SquareMatrix  the matrix type of the square matrix space.
  * @tparam Vector        the vector type of the column and row spaces.
  * @tparam Scalar        the scalar type of the square matrix space.
  * 
  * @define matrix  square matrix
  */
trait SquareMatrix[SquareMatrix, Vector, Scalar]
  extends Matrix[SquareMatrix, SquareMatrix, Vector, Vector, Scalar] {
  
  /** Returns the determinant of this $matrix. */
  def determinant: Scalar
  
  /** Multiplies this $matrix by another $matrix.
    * 
    * @param  that  the $matrix to multiply by.
    * @return the matrix product of this $matrix and the other $matrix.
    */
  def * (that: SquareMatrix): SquareMatrix
  
  /** Returns the inverse of this $matrix, if it exists. */
  def inverse: Option[SquareMatrix]
}
