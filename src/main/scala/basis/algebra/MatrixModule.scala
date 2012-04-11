/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait MatrixModule extends FreeModule { self =>
  type Matrix <: GeneralMatrix[Matrix, Transpose, ColumnVector, RowVector, Scalar]
  
  type Transpose <: GeneralMatrix[Transpose, Matrix, RowVector, ColumnVector, Scalar]
  
  type ColumnVector <: CoordinateVector[ColumnVector, Scalar]
  
  type RowVector <: CoordinateVector[RowVector, Scalar]
  
  type Vector = Matrix
  
  def Transpose: MatrixModule {
    type Matrix = self.Transpose
    type Transpose = self.Matrix
    type ColumnVector = self.RowVector
    type RowVector = self.ColumnVector
    type Scalar = self.Scalar
  }
  
  def Column: CoordinateModule {
    type Vector = self.ColumnVector
    type Scalar = self.Scalar
  }
  
  def Row: CoordinateModule {
    type Vector = self.RowVector
    type Scalar = self.Scalar
  }
  
  def zero: Matrix = {
    val z = Scalar.zero
    val entries = new Array[AnyRef](dimension)
    var k = 0
    while (k < entries.length) {
      entries(k) = z
      k += 1
    }
    apply(wrapRefArray(entries).asInstanceOf[Seq[Scalar]])
  }
  
  def dimension: Int = Column.dimension * Row.dimension
  
  def apply(entries: Seq[Scalar]): Matrix
}
