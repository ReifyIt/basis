/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

/** A 2x2 real matrix.
  * 
  * @author Chris Sachs
  */
final class MatrixR2x2(
    val _1_1: Real, val _1_2: Real,
    val _2_1: Real, val _2_2: Real)
  extends MatrixR2x2.Element {
  
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
  
  override def row(i: Int): VectorR2 = i match {
    case 0 => row1
    case 1 => row2
    case _ => throw new IndexOutOfBoundsException("row "+ i)
  }
  
  override def row1: VectorR2 = new VectorR2(_1_1, _1_2)
  override def row2: VectorR2 = new VectorR2(_2_1, _2_2)
  
  override def col(j: Int): VectorR2 = j match {
    case 0 => col1
    case 1 => col2
    case _ => throw new IndexOutOfBoundsException("col "+ j)
  }
  
  override def col1: VectorR2 = new VectorR2(_1_1, _2_1)
  override def col2: VectorR2 = new VectorR2(_1_2, _2_2)
  
  override def + (that: MatrixR2x2): MatrixR2x2 =
    new MatrixR2x2(
      _1_1 + that._1_1, _1_2 + that._1_2,
      _2_1 + that._2_1, _2_2 + that._2_2)
  
  override def unary_- : MatrixR2x2 =
    new MatrixR2x2(
      -_1_1, -_1_2,
      -_2_1, -_2_2)
  
  override def - (that: MatrixR2x2): MatrixR2x2 =
    new MatrixR2x2(
      _1_1 - that._1_1, _1_2 - that._1_2,
      _2_1 - that._2_1, _2_2 - that._2_2)
  
  override def :* (scalar: Real): MatrixR2x2 =
    new MatrixR2x2(
      _1_1 * scalar, _1_2 * scalar,
      _2_1 * scalar, _2_2 * scalar)
  
  override def *: (scalar: Real): MatrixR2x2 = this :* scalar
  
  override def :⋅ (vector: VectorR2): VectorR2 =
    new VectorR2(
      _1_1 * vector.x + _1_2 * vector.y,
      _2_1 * vector.x + _2_2 * vector.y)
  
  override def ⋅: (vector: VectorR2): VectorR2 =
    new VectorR2(
      vector.x * _1_1 + vector.y * _2_1,
      vector.x * _1_2 + vector.y * _2_2)
  
  override def T: MatrixR2x2 =
    new MatrixR2x2(
      _1_1, _2_1,
      _1_2, _2_2)
  
  override def * (that: MatrixR2x2): MatrixR2x2 =
    new MatrixR2x2(
      _1_1 * that._1_1 + _1_2 * that._2_1,
      _1_1 * that._1_2 + _1_2 * that._2_2,
      _2_1 * that._1_1 + _2_2 * that._2_1,
      _2_1 * that._1_2 + _2_2 * that._2_2)
  
  override def inverse: Option[MatrixR2x2] = {
    val det = this.det
    if (det.abs >= Double.MinPositiveValue)
      Some(new MatrixR2x2(
         _2_2 / det, -_1_2 / det,
        -_2_1 / det,  _1_1 / det))
    else None
  }
  
  override def det: Real = _1_1 * _2_2 - _1_2 * _2_1
  
  override def trace: Real = _1_1 + _2_2
}

/** A space of 2x2 real matrices. */
object MatrixR2x2 extends Matrix2x2Space[R2, R] with RealMatrixSpace[R2, R2] {
  trait Element extends super[Matrix2x2Space].Element with super[RealMatrixSpace].Element
  
  override type Matrix = MatrixR2x2
  
  override val Transpose: this.type = this
  
  override def Row: R2 = R2
  override def Col: R2 = R2
  
  override val zero: MatrixR2x2 =
    new MatrixR2x2(0.0, 0.0,  0.0, 0.0)
  
  override val unit: MatrixR2x2 =
    new MatrixR2x2(1.0, 0.0,  0.0, 1.0)
  
  override def apply(
      _1_1: Real, _1_2: Real,
      _2_1: Real, _2_2: Real): MatrixR2x2 =
    new MatrixR2x2(
      _1_1, _1_2,
      _2_1, _2_2)
  
  override def apply(entries: Array[Double]): MatrixR2x2 = {
    if (entries.length != 4) throw new DimensionException
    new MatrixR2x2(
      entries(0), entries(1),
      entries(2), entries(3))
  }
  
  override def rows(row1: VectorR2, row2: VectorR2): MatrixR2x2 =
    new MatrixR2x2(
      row1.x, row1.y,
      row2.x, row2.y)
  
  override def cols(col1: Col, col2: Col): MatrixR2x2 =
    new MatrixR2x2(
      col1.x, col2.x,
      col1.y, col2.y)
  
  override def toString: String = "R2x2"
}
