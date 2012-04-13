/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait RMxN extends LinearSpace with FMxN { self =>
  type Matrix <: MatrixRMxN[Matrix, Transpose, ColumnVector, RowVector]
  
  type Transpose <: MatrixRMxN[Transpose, Matrix, RowVector, ColumnVector]
  
  type ColumnVector <: VectorRN[ColumnVector]
  
  type RowVector <: VectorRN[RowVector]
  
  type Scalar = Real
  
  val Scalar = Real
  
  def Transpose: RMxN {
    type Matrix = self.Transpose
    type Transpose = self.Matrix
    type ColumnVector = self.RowVector
    type RowVector = self.ColumnVector
    type Scalar = self.Scalar
  }
  
  def Column: RN {
    type Vector = self.ColumnVector
    type Scalar = self.Scalar
  }
  
  def Row: RN {
    type Vector = self.RowVector
    type Scalar = self.Scalar
  }
  
  override def zero: Matrix = apply(new Array[Double](dimension))
  
  override def apply(entries: Seq[Scalar]): Matrix = {
    if (entries.length != dimension) throw new DimensionException
    val xs = new Array[Double](dimension)
    var i = 0
    while (i < xs.length) {
      xs(i) = entries(i).toDouble
      i += 1
    }
    apply(xs)
  }
  
  def apply(entries: Array[Double]): Matrix
}
