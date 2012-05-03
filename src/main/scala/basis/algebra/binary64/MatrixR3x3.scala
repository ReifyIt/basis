/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

final class MatrixR3x3(
    val _1_1: Real, val _1_2: Real, val _1_3: Real,
    val _2_1: Real, val _2_2: Real, val _2_3: Real,
    val _3_1: Real, val _3_2: Real, val _3_3: Real)
  extends Matrix3x3 with RealMatrix {
  
  override type Matrix = MatrixR3x3
  override type Span = VectorR3
  
  override def Matrix = MatrixR3x3
  
  override def M: Int = 3
  override def N: Int = 3
  
  override def apply(k: Int): Real = k match {
    case 0 => _1_1
    case 1 => _1_2
    case 2 => _1_3
    case 3 => _2_1
    case 4 => _2_2
    case 5 => _2_3
    case 6 => _3_1
    case 7 => _3_2
    case 8 => _3_3
    case _ => throw new IndexOutOfBoundsException(k.toString)
  }
  
  override def apply(i: Int, j: Int): Real = {
    if (i < 0 || i >= 3 || j < 0 || j >= 3)
      throw new IndexOutOfBoundsException("row "+ i +", "+"col "+ j)
    apply(3 * i + j)
  }
  
  override def row(i: Int): Row = i match {
    case 0 => row1
    case 1 => row2
    case 2 => row3
    case _ => throw new IndexOutOfBoundsException("row "+ i)
  }
  
  override def row1: Row = new Row(_1_1, _1_2, _1_3)
  override def row2: Row = new Row(_2_1, _2_2, _2_3)
  override def row3: Row = new Row(_3_1, _3_2, _3_3)
  
  override def col(j: Int): Col = j match {
    case 0 => col1
    case 1 => col2
    case 2 => col3
    case _ => throw new IndexOutOfBoundsException("col "+ j)
  }
  
  override def col1: Col = new Col(_1_1, _2_1, _3_1)
  override def col2: Col = new Col(_1_2, _2_2, _3_2)
  override def col3: Col = new Col(_1_3, _2_3, _3_3)
  
  override def + (that: Matrix): Matrix =
    new Matrix(
      _1_1 + that._1_1, _1_2 + that._1_2, _1_3 + that._1_3,
      _2_1 + that._2_1, _2_2 + that._2_2, _2_3 + that._2_3,
      _3_1 + that._3_1, _3_2 + that._3_2, _3_3 + that._3_3)
  
  override def unary_- : Matrix =
    new Matrix(
      -_1_1, -_1_2, -_1_3,
      -_2_1, -_2_2, -_2_3,
      -_3_1, -_3_2, -_3_3)
  
  override def - (that: Matrix): Matrix =
    new Matrix(
      _1_1 - that._1_1, _1_2 - that._1_2, _1_3 - that._1_3,
      _2_1 - that._2_1, _2_2 - that._2_2, _2_3 - that._2_3,
      _3_1 - that._3_1, _3_2 - that._3_2, _3_3 - that._3_3)
  
  override def :* (scalar: Real): Matrix =
    new Matrix(
      _1_1 * scalar, _1_2 * scalar, _1_3 * scalar,
      _2_1 * scalar, _2_2 * scalar, _2_3 * scalar,
      _3_1 * scalar, _3_2 * scalar, _3_3 * scalar)
  
  override def *: (scalar: Real): Matrix = this :* scalar
  
  override def :⋅ (vector: Row): Col =
    new Col(
      _1_1 * vector.x + _1_2 * vector.y + _1_3 * vector.z,
      _2_1 * vector.x + _2_2 * vector.y + _2_3 * vector.z,
      _3_1 * vector.x + _3_2 * vector.y + _3_3 * vector.z)
  
  override def ⋅: (vector: Col): Row =
    new Row(
      vector.x * _1_1 + vector.y * _2_1 + vector.z * _3_1,
      vector.x * _1_2 + vector.y * _2_2 + vector.z * _3_2,
      vector.x * _1_3 + vector.y * _2_3 + vector.z * _3_3)
  
  override def T: T =
    new T(
      _1_1, _2_1, _3_1,
      _1_2, _2_2, _3_2,
      _1_3, _2_3, _3_3)
  
  override def * (that: Matrix): Matrix =
    new Matrix(
      _1_1 * that._1_1 + _1_2 * that._2_1 + _1_3 * that._3_1,
      _1_1 * that._1_2 + _1_2 * that._2_2 + _1_3 * that._3_2,
      _1_1 * that._1_3 + _1_2 * that._2_3 + _1_3 * that._3_3,
      _2_1 * that._1_1 + _2_2 * that._2_1 + _2_3 * that._3_1,
      _2_1 * that._1_2 + _2_2 * that._2_2 + _2_3 * that._3_2,
      _2_1 * that._1_3 + _2_2 * that._2_3 + _2_3 * that._3_3,
      _3_1 * that._1_1 + _3_2 * that._2_1 + _3_3 * that._3_1,
      _3_1 * that._1_2 + _3_2 * that._2_2 + _3_3 * that._3_2,
      _3_1 * that._1_3 + _3_2 * that._2_3 + _3_3 * that._3_3)
  
  override def inverse: Option[Matrix] = {
    val minor_1_1 = _2_2 * _3_3 - _2_3 * _3_2
    val minor_1_2 = _2_1 * _3_3 - _2_3 * _3_1
    val minor_1_3 = _2_1 * _3_2 - _2_2 * _3_1
    val minor_2_1 = _1_2 * _3_3 - _1_3 * _3_2
    val minor_2_2 = _1_1 * _3_3 - _1_3 * _3_1
    val minor_2_3 = _1_1 * _3_2 - _1_2 * _3_1
    val minor_3_1 = _1_2 * _2_3 - _1_3 * _2_2
    val minor_3_2 = _1_1 * _2_3 - _1_3 * _2_1
    val minor_3_3 = _1_1 * _2_2 - _1_2 * _2_1
    
    val det = _1_1 * minor_1_1 - _1_2 * minor_1_2 + _1_3 * minor_1_3
    if (det.abs >= Double.MinPositiveValue)
      Some(new Matrix(
         minor_1_1 / det, -minor_2_1 / det,  minor_3_1 / det,
        -minor_1_2 / det,  minor_2_2 / det, -minor_3_2 / det,
         minor_1_3 / det, -minor_2_3 / det,  minor_3_3 / det))
    else None
  }
  
  override def det: Real = {
    val minor_1_1 = _2_2 * _3_3 - _2_3 * _3_2
    val minor_1_2 = _2_1 * _3_3 - _2_3 * _3_1
    val minor_1_3 = _2_1 * _3_2 - _2_2 * _3_1
    
    _1_1 * minor_1_1 - _1_2 * minor_1_2 + _1_3 * minor_1_3
  }
  
  override def trace: Real = _1_1 + _2_2 + _3_3
}

object MatrixR3x3 extends RealField.Scalar with Matrix3x3.Space with RealMatrix.Space {
  override type Matrix = MatrixR3x3
  override type Span = VectorR3
  
  override def Row = VectorR3
  override def Col = VectorR3
  
  override lazy val zero: Matrix =
    new Matrix(0.0, 0.0, 0.0,  0.0, 0.0, 0.0,  0.0, 0.0, 0.0)
  
  override lazy val unit: Matrix =
    new Matrix(1.0, 0.0, 0.0,  0.0, 1.0, 0.0,  0.0, 0.0, 1.0)
  
  override def apply(entries: Array[Double]): Matrix = {
    if (entries.length != 9) throw new DimensionException
    new Matrix(
      entries(0), entries(1), entries(2),
      entries(3), entries(4), entries(5),
      entries(6), entries(7), entries(8))
  }
  
  override def apply(
      _1_1: Real, _1_2: Real, _1_3: Real,
      _2_1: Real, _2_2: Real, _2_3: Real,
      _3_1: Real, _3_2: Real, _3_3: Real): Matrix =
    new Matrix(
      _1_1, _1_2, _1_3,
      _2_1, _2_2, _2_3,
      _3_1, _3_2, _3_3)
  
  override def rows(row1: Row, row2: Row, row3: Row): Matrix =
    new Matrix(
      row1.x, row1.y, row1.z,
      row2.x, row2.y, row2.z,
      row3.x, row3.y, row3.z)
  
  override def cols(col1: Col, col2: Col, col3: Col): Matrix =
    new Matrix(
      col1.x, col2.x, col3.x,
      col1.y, col2.y, col3.y,
      col1.z, col2.z, col3.z)
  
  override def toString: String = "R3x3"
}
