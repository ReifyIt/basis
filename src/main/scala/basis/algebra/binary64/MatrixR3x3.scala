/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

/** A 3x3 real matrix.
  * 
  * @author Chris Sachs
  */
final class MatrixR3x3(
    val _1_1: Real, val _1_2: Real, val _1_3: Real,
    val _2_1: Real, val _2_2: Real, val _2_3: Real,
    val _3_1: Real, val _3_2: Real, val _3_3: Real)
  extends MatrixR3x3.Element {
  
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
  
  override def row(i: Int): VectorR3 = i match {
    case 0 => row1
    case 1 => row2
    case 2 => row3
    case _ => throw new IndexOutOfBoundsException("row "+ i)
  }
  
  override def row1: VectorR3 = new VectorR3(_1_1, _1_2, _1_3)
  override def row2: VectorR3 = new VectorR3(_2_1, _2_2, _2_3)
  override def row3: VectorR3 = new VectorR3(_3_1, _3_2, _3_3)
  
  override def col(j: Int): VectorR3 = j match {
    case 0 => col1
    case 1 => col2
    case 2 => col3
    case _ => throw new IndexOutOfBoundsException("col "+ j)
  }
  
  override def col1: VectorR3 = new VectorR3(_1_1, _2_1, _3_1)
  override def col2: VectorR3 = new VectorR3(_1_2, _2_2, _3_2)
  override def col3: VectorR3 = new VectorR3(_1_3, _2_3, _3_3)
  
  override def + (that: MatrixR3x3): MatrixR3x3 =
    new MatrixR3x3(
      _1_1 + that._1_1, _1_2 + that._1_2, _1_3 + that._1_3,
      _2_1 + that._2_1, _2_2 + that._2_2, _2_3 + that._2_3,
      _3_1 + that._3_1, _3_2 + that._3_2, _3_3 + that._3_3)
  
  override def unary_- : MatrixR3x3 =
    new MatrixR3x3(
      -_1_1, -_1_2, -_1_3,
      -_2_1, -_2_2, -_2_3,
      -_3_1, -_3_2, -_3_3)
  
  override def - (that: MatrixR3x3): MatrixR3x3 =
    new MatrixR3x3(
      _1_1 - that._1_1, _1_2 - that._1_2, _1_3 - that._1_3,
      _2_1 - that._2_1, _2_2 - that._2_2, _2_3 - that._2_3,
      _3_1 - that._3_1, _3_2 - that._3_2, _3_3 - that._3_3)
  
  override def :* (scalar: Real): MatrixR3x3 =
    new MatrixR3x3(
      _1_1 * scalar, _1_2 * scalar, _1_3 * scalar,
      _2_1 * scalar, _2_2 * scalar, _2_3 * scalar,
      _3_1 * scalar, _3_2 * scalar, _3_3 * scalar)
  
  override def *: (scalar: Real): MatrixR3x3 = this :* scalar
  
  override def :⋅ (vector: VectorR3): VectorR3 =
    new VectorR3(
      _1_1 * vector.x + _1_2 * vector.y + _1_3 * vector.z,
      _2_1 * vector.x + _2_2 * vector.y + _2_3 * vector.z,
      _3_1 * vector.x + _3_2 * vector.y + _3_3 * vector.z)
  
  override def ⋅: (vector: VectorR3): VectorR3 =
    new VectorR3(
      vector.x * _1_1 + vector.y * _2_1 + vector.z * _3_1,
      vector.x * _1_2 + vector.y * _2_2 + vector.z * _3_2,
      vector.x * _1_3 + vector.y * _2_3 + vector.z * _3_3)
  
  override def transpose: MatrixR3x3 =
    new MatrixR3x3(
      _1_1, _2_1, _3_1,
      _1_2, _2_2, _3_2,
      _1_3, _2_3, _3_3)
  
  override def * (that: MatrixR3x3): MatrixR3x3 =
    new MatrixR3x3(
      _1_1 * that._1_1 + _1_2 * that._2_1 + _1_3 * that._3_1,
      _1_1 * that._1_2 + _1_2 * that._2_2 + _1_3 * that._3_2,
      _1_1 * that._1_3 + _1_2 * that._2_3 + _1_3 * that._3_3,
      _2_1 * that._1_1 + _2_2 * that._2_1 + _2_3 * that._3_1,
      _2_1 * that._1_2 + _2_2 * that._2_2 + _2_3 * that._3_2,
      _2_1 * that._1_3 + _2_2 * that._2_3 + _2_3 * that._3_3,
      _3_1 * that._1_1 + _3_2 * that._2_1 + _3_3 * that._3_1,
      _3_1 * that._1_2 + _3_2 * that._2_2 + _3_3 * that._3_2,
      _3_1 * that._1_3 + _3_2 * that._2_3 + _3_3 * that._3_3)
  
  override def inverse: Option[MatrixR3x3] = {
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
      Some(new MatrixR3x3(
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

/** A space of 3x3 real matrices. */
object MatrixR3x3 extends Matrix3x3Space[R3, R] with RealMatrixSpace[R3, R3] {
  trait Element extends super[Matrix3x3Space].Element with super[RealMatrixSpace].Element
  
  override type Matrix = MatrixR3x3
  
  override def Row: R3 = R3
  override def Col: R3 = R3
  
  override lazy val zero: MatrixR3x3 =
    new MatrixR3x3(0.0, 0.0, 0.0,  0.0, 0.0, 0.0,  0.0, 0.0, 0.0)
  
  override lazy val unit: MatrixR3x3 =
    new MatrixR3x3(1.0, 0.0, 0.0,  0.0, 1.0, 0.0,  0.0, 0.0, 1.0)
  
  override def apply(
      _1_1: Real, _1_2: Real, _1_3: Real,
      _2_1: Real, _2_2: Real, _2_3: Real,
      _3_1: Real, _3_2: Real, _3_3: Real): MatrixR3x3 =
    new MatrixR3x3(
      _1_1, _1_2, _1_3,
      _2_1, _2_2, _2_3,
      _3_1, _3_2, _3_3)
  
  override def apply(entries: Array[Double]): MatrixR3x3 = {
    if (entries.length != 9) throw new DimensionException
    new MatrixR3x3(
      entries(0), entries(1), entries(2),
      entries(3), entries(4), entries(5),
      entries(6), entries(7), entries(8))
  }
  
  override def rows(row1: VectorR3, row2: VectorR3, row3: VectorR3): MatrixR3x3 =
    new MatrixR3x3(
      row1.x, row1.y, row1.z,
      row2.x, row2.y, row2.z,
      row3.x, row3.y, row3.z)
  
  override def cols(col1: VectorR3, col2: VectorR3, col3: VectorR3): MatrixR3x3 =
    new MatrixR3x3(
      col1.x, col2.x, col3.x,
      col1.y, col2.y, col3.y,
      col1.z, col2.z, col3.z)
  
  override def toString: String = "R3x3"
}
