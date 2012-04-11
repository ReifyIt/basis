/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait R2x2 extends F2x2 with RealMatrixSpace { self =>
  type Matrix <: MatrixR2x2[Matrix, RowVector]
  
  type RowVector <: VectorR2[RowVector]
  
  override def Column: R2 {
    type Vector = self.ColumnVector
    type Scalar = self.Scalar
  } = Row
  
  def Row: R2 {
    type Vector = self.RowVector
    type Scalar = self.Scalar
  }
  
  override def zero: Matrix =
    apply(0.0, 0.0,  0.0, 0.0)
  
  override def identity: Matrix =
    apply(1.0, 0.0,  0.0, 1.0)
  
  def apply(entries: Array[Double]): Matrix = {
    if (entries.length != 4) throw new DimensionException
    apply(entries(0), entries(1),  entries(2), entries(3))
  }
  
  def apply(_1_1: Scalar, _1_2: Scalar,
            _2_1: Scalar, _2_2: Scalar): Matrix =
    apply(_1_1, _1_2,  _2_1, _2_2)
  
  def apply(_1_1: Double, _1_2: Double,
            _2_1: Double, _2_2: Double): Matrix
}

object R2x2 extends R2x2 {
  final class Matrix(
      val _1_1: Double, val _1_2: Double,
      val _2_1: Double, val _2_2: Double)
    extends MatrixR2x2[Matrix, R2.Vector] {
    
    def Space = R2x2
  }
  
  type RowVector = R2.Vector
  
  override def Column = R2
  
  def Row = R2
  
  override val zero: Matrix = super.zero
  
  override val identity: Matrix = super.identity
  
  def apply(_1_1: Double, _1_2: Double,
            _2_1: Double, _2_2: Double): Matrix =
    new Matrix(_1_1, _1_2,  _2_1, _2_2)
  
  override def toString: String = "R2x2"
}
