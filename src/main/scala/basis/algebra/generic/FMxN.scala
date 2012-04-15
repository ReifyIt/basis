/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

trait FMxN extends LinearModule { self =>
  type Matrix <: MatrixFMxN {
    type Matrix       = self.Matrix
    type Transpose    = self.Transpose
    type RowVector    = self.RowVector
    type ColumnVector = self.ColumnVector
    type Scalar       = self.Scalar
  }
  
  type Transpose <: MatrixFMxN {
    type Matrix       = self.Transpose
    type Transpose    = self.Matrix
    type RowVector    = self.ColumnVector
    type ColumnVector = self.RowVector
    type Scalar       = self.Scalar
  }
  
  type RowVector <: VectorFN {
    type Vector = self.RowVector
    type Scalar = self.Scalar
  }
  
  type ColumnVector <: VectorFN {
    type Vector = self.ColumnVector
    type Scalar = self.Scalar
  }
  
  override type Point = Matrix
  
  override type Vector = Matrix
  
  def Transpose: FMxN {
    type Matrix       = self.Transpose
    type Transpose    = self.Matrix
    type RowVector    = self.ColumnVector
    type ColumnVector = self.RowVector
    type Scalar       = self.Scalar
  }
  
  def Row: FN {
    type Vector = self.RowVector
    type Scalar = self.Scalar
  }
  
  def Column: FN {
    type Vector = self.ColumnVector
    type Scalar = self.Scalar
  }
  
  def dimension: Int = Column.dimension * Row.dimension
  
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
  
  def apply(entries: TraversableOnce[Scalar]): Matrix
  
  def compose(that: FMxN { type ColumnVector = self.RowVector; type Scalar = self.Scalar }):
      FMxN {
        type RowVector    = self.ColumnVector
        type ColumnVector = that.RowVector
        type Scalar       = self.Scalar
      } =
    new DenseMatrixModule(Column, that.Row)
}
