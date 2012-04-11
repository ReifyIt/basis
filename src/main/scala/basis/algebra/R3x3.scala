/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait R3x3 extends F3x3 with RealMatrixSpace { self =>
  type Matrix <: MatrixR3x3[Matrix, RowVector]
  
  type RowVector <: VectorR3[RowVector]
  
  override def Column: R3 {
    type Vector = self.ColumnVector
    type Scalar = self.Scalar
  } = Row
  
  def Row: R3 {
    type Vector = self.RowVector
    type Scalar = self.Scalar
  }
  
  override def zero: Matrix =
    apply(0.0, 0.0, 0.0,  0.0, 0.0, 0.0,  0.0, 0.0, 0.0)
  
  override def identity: Matrix =
    apply(1.0, 0.0, 0.0,  0.0, 1.0, 0.0,  0.0, 0.0, 1.0)
  
  def apply(entries: Array[Double]): Matrix = {
    if (entries.length != 9) throw new DimensionException
    apply(entries(0), entries(1), entries(2),
          entries(3), entries(4), entries(5),
          entries(6), entries(7), entries(8))
  }
  
  def apply(_1_1: Scalar, _1_2: Scalar, _1_3: Scalar,
            _2_1: Scalar, _2_2: Scalar, _2_3: Scalar,
            _3_1: Scalar, _3_2: Scalar, _3_3: Scalar): Matrix =
    apply(_1_1.toDouble, _1_2.toDouble, _1_3.toDouble,
          _2_1.toDouble, _2_2.toDouble, _2_3.toDouble,
          _3_1.toDouble, _3_2.toDouble, _3_3.toDouble)
  
  def apply(_1_1: Double, _1_2: Double, _1_3: Double,
            _2_1: Double, _2_2: Double, _2_3: Double,
            _3_1: Double, _3_2: Double, _3_3: Double): Matrix
}

object R3x3 extends R3x3 {
  final class Matrix(
      val _1_1: Double, val _1_2: Double, val _1_3: Double,
      val _2_1: Double, val _2_2: Double, val _2_3: Double,
      val _3_1: Double, val _3_2: Double, val _3_3: Double)
    extends MatrixR3x3[Matrix, R3.Vector] {
    
    def Space = R3x3
  }
  
  type RowVector = R3.Vector
  
  override def Column = R3
  
  def Row = R3
  
  override val zero: Matrix = super.zero
  
  override val identity: Matrix = super.identity
  
  def apply(_1_1: Double, _1_2: Double, _1_3: Double,
            _2_1: Double, _2_2: Double, _2_3: Double,
            _3_1: Double, _3_2: Double, _3_3: Double): Matrix =
    new Matrix(_1_1, _1_2, _1_3,  _2_1, _2_2, _2_3,  _3_1, _3_2, _3_3)
  
  override def toString: String = "R3x3"
}
