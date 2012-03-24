/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Matrix[Matrix, Transpose, ColumnVector, RowVector, -Scalar] extends Vector[Matrix, Scalar] {
  def :⋅ (column: RowVector): ColumnVector
  
  def ⋅: (row: ColumnVector): RowVector
  
  def transpose: Transpose
}
