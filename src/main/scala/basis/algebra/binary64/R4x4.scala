/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

trait R4x4 extends F4x4 with RMxN { self =>
  type Matrix <: MatrixR4x4 {
    type Matrix    = self.Matrix
    type RowVector = self.RowVector
  }
  
  type RowVector <: VectorR4 {
    type Vector = self.RowVector
  }
  
  override def Row: R4 {
    type Vector = self.RowVector
    type Scalar = self.Scalar
  }
  
  override def Column: R4 {
    type Vector = self.ColumnVector
    type Scalar = self.Scalar
  } = Row
  
  override def zero: Matrix =
    apply(0.0, 0.0, 0.0, 0.0,
          0.0, 0.0, 0.0, 0.0,
          0.0, 0.0, 0.0, 0.0,
          0.0, 0.0, 0.0, 0.0)
  
  override def identity: Matrix =
    apply(1.0, 0.0, 0.0, 0.0,
          0.0, 1.0, 0.0, 0.0,
          0.0, 0.0, 1.0, 0.0,
          0.0, 0.0, 0.0, 1.0)
  
  override def apply(entries: Array[Double]): Matrix = {
    if (entries.length != 16) throw new DimensionException
    apply(entries( 0), entries( 1), entries( 2), entries( 3),
          entries( 4), entries( 5), entries( 6), entries( 7),
          entries( 8), entries( 9), entries(10), entries(11),
          entries(12), entries(13), entries(14), entries(15))
  }
  
  override def apply(_1_1: Scalar, _1_2: Scalar, _1_3: Scalar, _1_4: Scalar,
                     _2_1: Scalar, _2_2: Scalar, _2_3: Scalar, _2_4: Scalar,
                     _3_1: Scalar, _3_2: Scalar, _3_3: Scalar, _3_4: Scalar,
                     _4_1: Scalar, _4_2: Scalar, _4_3: Scalar, _4_4: Scalar): Matrix =
    apply(_1_1.toDouble, _1_2.toDouble, _1_3.toDouble, _1_4.toDouble,
          _2_1.toDouble, _2_2.toDouble, _2_3.toDouble, _2_4.toDouble,
          _3_1.toDouble, _3_2.toDouble, _3_3.toDouble, _3_4.toDouble,
          _4_1.toDouble, _4_2.toDouble, _4_3.toDouble, _4_4.toDouble)
  
  def apply(_1_1: Double, _1_2: Double, _1_3: Double, _1_4: Double,
            _2_1: Double, _2_2: Double, _2_3: Double, _2_4: Double,
            _3_1: Double, _3_2: Double, _3_3: Double, _3_4: Double,
            _4_1: Double, _4_2: Double, _4_3: Double, _4_4: Double): Matrix
}

object R4x4 extends R4x4 {
  final class Matrix(
      val _1_1: Double, val _1_2: Double, val _1_3: Double, val _1_4: Double,
      val _2_1: Double, val _2_2: Double, val _2_3: Double, val _2_4: Double,
      val _3_1: Double, val _3_2: Double, val _3_3: Double, val _3_4: Double,
      val _4_1: Double, val _4_2: Double, val _4_3: Double, val _4_4: Double)
    extends MatrixR4x4 {
    
    override type Matrix = R4x4.Matrix
    override type RowVector = R4.Vector
    
    override def Space = R4x4
    
    def apply(k: Int): Double = k match {
      case  0 => _1_1
      case  1 => _1_2
      case  2 => _1_3
      case  3 => _1_4
      case  4 => _2_1
      case  5 => _2_2
      case  6 => _2_3
      case  7 => _2_4
      case  8 => _3_1
      case  9 => _3_2
      case 10 => _3_3
      case 11 => _3_4
      case 12 => _4_1
      case 13 => _4_2
      case 14 => _4_3
      case 15 => _4_4
      case _ => throw new IndexOutOfBoundsException(k.toString)
    }
  }
  
  override type RowVector = R4.Vector
  override type ColumnVector = R4.Vector
  
  override def Row = R4
  override def Column = R4
  
  override val zero: Matrix = super.zero
  
  override val identity: Matrix = super.identity
  
  def apply(_1_1: Double, _1_2: Double, _1_3: Double, _1_4: Double,
            _2_1: Double, _2_2: Double, _2_3: Double, _2_4: Double,
            _3_1: Double, _3_2: Double, _3_3: Double, _3_4: Double,
            _4_1: Double, _4_2: Double, _4_3: Double, _4_4: Double): Matrix =
    new Matrix(_1_1, _1_2, _1_3, _1_4,
               _2_1, _2_2, _2_3, _2_4,
               _3_1, _3_2, _3_3, _3_4,
               _4_1, _4_2, _4_3, _4_4)
  
  override def toString: String = "R4x4"
}
