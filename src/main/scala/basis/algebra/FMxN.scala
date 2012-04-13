/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait FMxN extends LinearModule { self =>
  type Matrix <: MatrixFMxN[Matrix, Transpose, ColumnVector, RowVector, Scalar]
  
  type Transpose <: MatrixFMxN[Transpose, Matrix, RowVector, ColumnVector, Scalar]
  
  type ColumnVector <: VectorFN[ColumnVector, Scalar]
  
  type RowVector <: VectorFN[RowVector, Scalar]
  
  type Vector = Matrix
  
  def Transpose: FMxN {
    type Matrix = self.Transpose
    type Transpose = self.Matrix
    type ColumnVector = self.RowVector
    type RowVector = self.ColumnVector
    type Scalar = self.Scalar
  }
  
  def Column: FN {
    type Vector = self.ColumnVector
    type Scalar = self.Scalar
  }
  
  def Row: FN {
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
