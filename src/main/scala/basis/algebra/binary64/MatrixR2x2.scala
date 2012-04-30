/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

final class MatrixR2x2(
    val _1_1: Real, val _1_2: Real,
    val _2_1: Real, val _2_2: Real)
  extends Matrix2x2Like with RealMatrixLike {
  
  override type Matrix = MatrixR2x2
  override type Vec    = VectorR2
  
  override def Matrix: R2x2 = R2x2
  override def Row: R2 = R2
  override def Col: R2 = R2
  
  override def M: Int = 2
  override def N: Int = 2
  
  override def apply(k: Int): Real = k match {
    case 0 => _1_1
    case 1 => _1_2
    case 2 => _2_1
    case 3 => _2_2
    case _ => throw new IndexOutOfBoundsException(k.toString)
  }
  
  override def apply(i: Int, j: Int): Real = {
    if (i < 0 || i >= 2 || j < 0 || j >= 2)
      throw new IndexOutOfBoundsException("row "+ i +", "+"col "+ j)
    apply(2 * i + j)
  }
  
  override def row(i: Int): Row = i match {
    case 0 => row1
    case 1 => row2
    case _ => throw new IndexOutOfBoundsException("row "+ i)
  }
  
  override def row1: Row = new Row(_1_1, _1_2)
  override def row2: Row = new Row(_2_1, _2_2)
  
  override def col(j: Int): Col = j match {
    case 0 => col1
    case 1 => col2
    case _ => throw new IndexOutOfBoundsException("col "+ j)
  }
  
  override def col1: Col = new Col(_1_1, _2_1)
  override def col2: Col = new Col(_1_2, _2_2)
  
  override def + (that: Matrix): Matrix =
    new Matrix(
      _1_1 + that._1_1, _1_2 + that._1_2,
      _2_1 + that._2_1, _2_2 + that._2_2)
  
  override def unary_- : Matrix =
    new Matrix(
      -_1_1, -_1_2,
      -_2_1, -_2_2)
  
  override def - (that: Matrix): Matrix =
    new Matrix(
      _1_1 - that._1_1, _1_2 - that._1_2,
      _2_1 - that._2_1, _2_2 - that._2_2)
  
  override def :* (scalar: Real): Matrix =
    new Matrix(
      _1_1 * scalar, _1_2 * scalar,
      _2_1 * scalar, _2_2 * scalar)
  
  override def *: (scalar: Real): Matrix = this :* scalar
  
  override def :⋅ (vector: Row): Col =
    new Col(
      _1_1 * vector.x + _1_2 * vector.y,
      _2_1 * vector.x + _2_2 * vector.y)
  
  override def ⋅: (vector: Col): Row =
    new Row(
      vector.x * _1_1 + vector.y * _2_1,
      vector.x * _1_2 + vector.y * _2_2)
  
  override def T: T =
    new T(
      _1_1, _2_1,
      _1_2, _2_2)
  
  override def * (that: Matrix): Matrix =
    new Matrix(
      _1_1 * that._1_1 + _1_2 * that._2_1,
      _1_1 * that._1_2 + _1_2 * that._2_2,
      _2_1 * that._1_1 + _2_2 * that._2_1,
      _2_1 * that._1_2 + _2_2 * that._2_2)
  
  override def inverse: Option[Matrix] = {
    val det = this.det
    if (det.abs >= Double.MinPositiveValue)
      Some(new Matrix(
         _2_2 / det, -_1_2 / det,
        -_2_1 / det,  _1_1 / det))
    else None
  }
  
  override def det: Real = _1_1 * _2_2 - _1_2 * _2_1
  
  override def trace: Real = _1_1 + _2_2
}
