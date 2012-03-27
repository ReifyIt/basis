/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.util.MurmurHash._

/** A 4x4 matrix of `Real` values.
  * 
  * @author Chris Sachs
  * 
  * @constructor  Constructs a matrix with sixteen row-major `Double` values.
  * @param  _1_1  The entry at row 1, column 1.
  * @param  _1_2  The entry at row 1, column 2.
  * @param  _1_3  The entry at row 1, column 3.
  * @param  _1_4  The entry at row 1, column 4.
  * @param  _2_1  The entry at row 2, column 1.
  * @param  _2_2  The entry at row 2, column 2.
  * @param  _2_3  The entry at row 2, column 3.
  * @param  _2_4  The entry at row 2, column 4.
  * @param  _3_1  The entry at row 3, column 1.
  * @param  _3_2  The entry at row 3, column 2.
  * @param  _3_3  The entry at row 3, column 3.
  * @param  _3_4  The emtry at row 3, column 4.
  * @param  _4_1  The entry at row 4, column 1.
  * @param  _4_2  The entry at row 4, column 2.
  * @param  _4_3  The entry at row 4, column 3.
  * @param  _4_4  The entry at row 4, column 4.
  * 
  * @define scalar  `Real` value
  */
final class MatrixR4x4(
    val _1_1: Double, val _1_2: Double, val _1_3: Double, val _1_4: Double,
    val _2_1: Double, val _2_2: Double, val _2_3: Double, val _2_4: Double,
    val _3_1: Double, val _3_2: Double, val _3_3: Double, val _3_4: Double,
    val _4_1: Double, val _4_2: Double, val _4_3: Double, val _4_4: Double)
  extends SquareMatrix[MatrixR4x4, VectorR4, Real]
    with RealVector[MatrixR4x4] {
  
  /** The vector in the first column of the matrix. */
  def column1: VectorR4 = new VectorR4(_1_1, _2_1, _3_1, _4_1)
  
  /** The vector in the second column of the matrix. */
  def column2: VectorR4 = new VectorR4(_1_2, _2_2, _3_2, _4_2)
  
  /** The vector in the third column of the matrix. */
  def column3: VectorR4 = new VectorR4(_1_3, _2_3, _3_3, _4_3)
  
  /** The vector in the fourth column of the matrix. */
  def column4: VectorR4 = new VectorR4(_1_4, _2_4, _3_4, _4_4)
  
  /** The vector in the first row of the matrix. */
  def row1: VectorR4 = new VectorR4(_1_1, _1_2, _1_3, _1_4)
  
  /** The vector in the second row of the matrix. */
  def row2: VectorR4 = new VectorR4(_2_1, _2_2, _2_3, _2_4)
  
  /** The vector in the third row of the matrix. */
  def row3: VectorR4 = new VectorR4(_3_1, _3_2, _3_3, _3_4)
  
  /** The vector in the fourth row of the matrix. */
  def row4: VectorR4 = new VectorR4(_4_1, _4_2, _4_3, _4_4)
  
  def + (that: MatrixR4x4): MatrixR4x4 =
    new MatrixR4x4(
      _1_1 + that._1_1, _1_2 + that._1_2, _1_3 + that._1_3, _1_4 + that._1_4,
      _2_1 + that._2_1, _2_2 + that._2_2, _2_3 + that._2_3, _2_4 + that._2_4,
      _3_1 + that._3_1, _3_2 + that._3_2, _3_3 + that._3_3, _3_4 + that._3_4,
      _4_1 + that._4_1, _4_2 + that._4_2, _4_3 + that._4_3, _4_4 + that._4_4)
  
  def unary_- : MatrixR4x4 =
    new MatrixR4x4(
      -_1_1, -_1_2, -_1_3, -_1_4,
      -_2_1, -_2_2, -_2_3, -_2_4,
      -_3_1, -_3_2, -_3_3, -_3_4,
      -_4_1, -_4_2, -_4_3, -_4_4)
  
  def - (that: MatrixR4x4): MatrixR4x4 =
    new MatrixR4x4(
      _1_1 - that._1_1, _1_2 - that._1_2, _1_3 - that._1_3, _1_4 - that._1_4,
      _2_1 - that._2_1, _2_2 - that._2_2, _2_3 - that._2_3, _2_4 - that._2_4,
      _3_1 - that._3_1, _3_2 - that._3_2, _3_3 - that._3_3, _3_4 - that._3_4,
      _4_1 - that._4_1, _4_2 - that._4_2, _4_3 - that._4_3, _4_4 - that._4_4)
  
  def :* (scalar: Double): MatrixR4x4 =
    new MatrixR4x4(
      _1_1 * scalar, _1_2 * scalar, _1_3 * scalar, _1_4 * scalar,
      _2_1 * scalar, _2_2 * scalar, _2_3 * scalar, _2_4 * scalar,
      _3_1 * scalar, _3_2 * scalar, _3_3 * scalar, _3_4 * scalar,
      _4_1 * scalar, _4_2 * scalar, _4_3 * scalar, _4_4 * scalar)
  
  def *: (scalar: Double): MatrixR4x4 =
    this :* scalar
  
  def / (scalar: Double): MatrixR4x4 =
    new MatrixR4x4(
      _1_1 / scalar, _1_2 / scalar, _1_3 / scalar, _1_4 / scalar,
      _2_1 / scalar, _2_2 / scalar, _2_3 / scalar, _2_4 / scalar,
      _3_1 / scalar, _3_2 / scalar, _3_3 / scalar, _3_4 / scalar,
      _4_1 / scalar, _4_2 / scalar, _4_3 / scalar, _4_4 / scalar)
  
  def :⋅ (column: VectorR4): VectorR4 =
    new VectorR4(
      _1_1 * column.x + _1_2 * column.y + _1_3 * column.z + _1_4 * column.w,
      _2_1 * column.x + _2_2 * column.y + _2_3 * column.z + _2_4 * column.w,
      _3_1 * column.x + _3_2 * column.y + _3_3 * column.z + _3_4 * column.w,
      _4_1 * column.x + _4_2 * column.y + _4_3 * column.z + _4_4 * column.w)
  
  def ⋅: (row: VectorR4): VectorR4 =
    new VectorR4(
      row.x * _1_1 + row.y * _2_1 + row.z * _3_1 + row.w * _4_1,
      row.x * _1_2 + row.y * _2_2 + row.z * _3_2 + row.w * _4_2,
      row.x * _1_3 + row.y * _2_3 + row.z * _3_3 + row.w * _4_3,
      row.x * _1_4 + row.y * _2_4 + row.z * _3_4 + row.w * _4_4)
  
  def transpose: MatrixR4x4 =
    new MatrixR4x4(
      _1_1, _2_1, _3_1, _4_1,
      _1_2, _2_2, _3_2, _4_2,
      _1_3, _2_3, _3_3, _4_3,
      _1_4, _2_4, _3_4, _4_4)
  
  def determinant: Real = {
    // 2x2 determinants minor_i1_i2__j1_j2 with
    // rows i1 and i2 and columns j1 and j2 blocked out.
    val minor_1_2__1_2 = _3_3 * _4_4 - _3_4 * _4_3
    val minor_1_2__1_3 = _3_2 * _4_4 - _3_4 * _4_2
    val minor_1_2__1_4 = _3_2 * _4_3 - _3_3 * _4_2
    val minor_1_2__2_3 = _3_1 * _4_4 - _3_4 * _4_1
    val minor_1_2__2_4 = _3_1 * _4_3 - _3_3 * _4_1
    val minor_1_2__3_4 = _3_1 * _4_2 - _3_2 * _4_1
    
    // 3x3 determinants minor_i_j with row i and column j blocked out.
    val minor_1_1 = _2_2 * minor_1_2__1_2 - _2_3 * minor_1_2__1_3 + _2_4 * minor_1_2__1_4
    val minor_1_2 = _2_1 * minor_1_2__1_2 - _2_3 * minor_1_2__2_3 + _2_4 * minor_1_2__2_4
    val minor_1_3 = _2_1 * minor_1_2__1_3 - _2_2 * minor_1_2__2_3 + _2_4 * minor_1_2__3_4
    val minor_1_4 = _2_1 * minor_1_2__1_4 - _2_2 * minor_1_2__2_4 + _2_3 * minor_1_2__3_4
    
    new Real(_1_1 * minor_1_1 - _1_2 * minor_1_2 + _1_3 * minor_1_3 - _1_4 * minor_1_4)
  }
  
  def * (that: MatrixR4x4): MatrixR4x4 =
    new MatrixR4x4(
      _1_1 * that._1_1 + _1_2 * that._2_1 + _1_3 * that._3_1 + _1_4 * that._4_1,
      _1_1 * that._1_2 + _1_2 * that._2_2 + _1_3 * that._3_2 + _1_4 * that._4_2,
      _1_1 * that._1_3 + _1_2 * that._2_3 + _1_3 * that._3_3 + _1_4 * that._4_3,
      _1_1 * that._1_4 + _1_2 * that._2_4 + _1_3 * that._3_4 + _1_4 * that._4_4,
      _2_1 * that._1_1 + _2_2 * that._2_1 + _2_3 * that._3_1 + _2_4 * that._4_1,
      _2_1 * that._1_2 + _2_2 * that._2_2 + _2_3 * that._3_2 + _2_4 * that._4_2,
      _2_1 * that._1_3 + _2_2 * that._2_3 + _2_3 * that._3_3 + _2_4 * that._4_3,
      _2_1 * that._1_4 + _2_2 * that._2_4 + _2_3 * that._3_4 + _2_4 * that._4_4,
      _3_1 * that._1_1 + _3_2 * that._2_1 + _3_3 * that._3_1 + _3_4 * that._4_1,
      _3_1 * that._1_2 + _3_2 * that._2_2 + _3_3 * that._3_2 + _3_4 * that._4_2,
      _3_1 * that._1_3 + _3_2 * that._2_3 + _3_3 * that._3_3 + _3_4 * that._4_3,
      _3_1 * that._1_4 + _3_2 * that._2_4 + _3_3 * that._3_4 + _3_4 * that._4_4,
      _4_1 * that._1_1 + _4_2 * that._2_1 + _4_3 * that._3_1 + _4_4 * that._4_1,
      _4_1 * that._1_2 + _4_2 * that._2_2 + _4_3 * that._3_2 + _4_4 * that._4_2,
      _4_1 * that._1_3 + _4_2 * that._2_3 + _4_3 * that._3_3 + _4_4 * that._4_3,
      _4_1 * that._1_4 + _4_2 * that._2_4 + _4_3 * that._3_4 + _4_4 * that._4_4)
  
  def inverse: Option[MatrixR4x4] = {
    // all 2x2 determinants minor_i1_i2__j1_j2 with
    // rows i1 and i2 and columns j1 and j2 blocked out.
    val minor_1_2__1_2 = _3_3 * _4_4 - _3_4 * _4_3
    val minor_1_2__1_3 = _3_2 * _4_4 - _3_4 * _4_2
    val minor_1_2__1_4 = _3_2 * _4_3 - _3_3 * _4_2
    val minor_1_2__2_3 = _3_1 * _4_4 - _3_4 * _4_1
    val minor_1_2__2_4 = _3_1 * _4_3 - _3_3 * _4_1
    val minor_1_2__3_4 = _3_1 * _4_2 - _3_2 * _4_1
    val minor_1_3__1_2 = _2_3 * _4_4 - _2_4 * _4_3
    val minor_1_3__1_3 = _2_2 * _4_4 - _2_4 * _4_2
    val minor_1_3__1_4 = _2_2 * _4_3 - _2_3 * _4_2
    val minor_1_3__2_3 = _2_1 * _4_4 - _2_4 * _4_1
    val minor_1_3__2_4 = _2_1 * _4_3 - _2_3 * _4_1
    val minor_1_3__3_4 = _2_1 * _4_2 - _2_2 * _4_1
    val minor_1_4__1_2 = _2_3 * _3_4 - _2_4 * _3_3
    val minor_1_4__1_3 = _2_2 * _3_4 - _2_4 * _3_2
    val minor_1_4__1_4 = _2_2 * _3_3 - _2_3 * _3_2
    val minor_1_4__2_3 = _2_1 * _3_4 - _2_4 * _3_1
    val minor_1_4__2_4 = _2_1 * _3_3 - _2_3 * _3_1
    val minor_1_4__3_4 = _2_1 * _3_2 - _2_2 * _3_1
    
    // all 3x3 determinants minor_i_j with row i and column j blocked out.
    val minor_1_1 = _2_2 * minor_1_2__1_2 - _2_3 * minor_1_2__1_3 + _2_4 * minor_1_2__1_4
    val minor_1_2 = _2_1 * minor_1_2__1_2 - _2_3 * minor_1_2__2_3 + _2_4 * minor_1_2__2_4
    val minor_1_3 = _2_1 * minor_1_2__1_3 - _2_2 * minor_1_2__2_3 + _2_4 * minor_1_2__3_4
    val minor_1_4 = _2_1 * minor_1_2__1_4 - _2_2 * minor_1_2__2_4 + _2_3 * minor_1_2__3_4
    val minor_2_1 = _1_2 * minor_1_2__1_2 - _1_3 * minor_1_2__1_3 + _1_4 * minor_1_2__1_4
    val minor_2_2 = _1_1 * minor_1_2__1_2 - _1_3 * minor_1_2__2_3 + _1_4 * minor_1_2__2_4
    val minor_2_3 = _1_1 * minor_1_2__1_3 - _1_2 * minor_1_2__2_3 + _1_4 * minor_1_2__3_4
    val minor_2_4 = _1_1 * minor_1_2__1_4 - _1_2 * minor_1_2__2_4 + _1_3 * minor_1_2__3_4
    val minor_3_1 = _1_2 * minor_1_3__1_2 - _1_3 * minor_1_3__1_3 + _1_4 * minor_1_3__1_4
    val minor_3_2 = _1_1 * minor_1_3__1_2 - _1_3 * minor_1_3__2_3 + _1_4 * minor_1_3__2_4
    val minor_3_3 = _1_1 * minor_1_3__1_3 - _1_2 * minor_1_3__2_3 + _1_4 * minor_1_3__3_4
    val minor_3_4 = _1_1 * minor_1_3__1_4 - _1_2 * minor_1_3__2_4 + _1_3 * minor_1_3__3_4
    val minor_4_1 = _1_2 * minor_1_4__1_2 - _1_3 * minor_1_4__1_3 + _1_4 * minor_1_4__1_4
    val minor_4_2 = _1_1 * minor_1_4__1_2 - _1_3 * minor_1_4__2_3 + _1_4 * minor_1_4__2_4
    val minor_4_3 = _1_1 * minor_1_4__1_3 - _1_2 * minor_1_4__2_3 + _1_4 * minor_1_4__3_4
    val minor_4_4 = _1_1 * minor_1_4__1_4 - _1_2 * minor_1_4__2_4 + _1_3 * minor_1_4__3_4
    
    val det = _1_1 * minor_1_1 - _1_2 * minor_1_2 + _1_3 * minor_1_3 - _1_4 * minor_1_4
    if (math.abs(det) >= java.lang.Double.MIN_NORMAL)
      Some(new MatrixR4x4(
         minor_1_1 / det, -minor_2_1 / det,  minor_3_1 / det, -minor_4_1 / det,
        -minor_1_2 / det,  minor_2_2 / det, -minor_3_2 / det,  minor_4_2 / det,
         minor_1_3 / det, -minor_2_3 / det,  minor_3_3 / det, -minor_4_3 / det,
        -minor_1_4 / det,  minor_2_4 / det, -minor_3_4 / det,  minor_4_4 / det))
    else None
  }
  
  override def equals(other: Any): Boolean = other match {
    case that: MatrixR4x4 =>
      _1_1 == that._1_1 && _1_2 == that._1_2 && _1_3 == that._1_3 && _1_4 == that._1_4 &&
      _2_1 == that._2_1 && _2_2 == that._2_2 && _2_3 == that._2_3 && _2_4 == that._2_4 &&
      _3_1 == that._3_1 && _3_2 == that._3_2 && _3_3 == that._3_3 && _3_4 == that._3_4 &&
      _4_1 == that._4_1 && _4_2 == that._4_2 && _4_3 == that._4_3 && _4_4 == that._4_4
    case _ => false
  }
  
  override def hashCode: Int =
    mash(mix(mix(mix(mix(mix(mix(mix(mix(mix(mix(mix(mix(mix(mix(mix(mix(1138707957,
      _1_1), _1_2), _1_3), _1_4),
      _2_1), _2_2), _2_3), _2_4),
      _3_1), _3_2), _3_3), _3_4),
      _4_1), _4_2), _4_3), _4_4))
  
  override def toString: String =
    "MatrixR4x4"+"("+
      _1_1 +", "+ _1_2 +", "+ _1_3 +", "+ _1_4 +",  "+
      _2_1 +", "+ _2_2 +", "+ _2_3 +", "+ _2_4 +",  "+
      _3_1 +", "+ _3_2 +", "+ _3_3 +", "+ _3_4 +",  "+
      _4_1 +", "+ _4_2 +", "+ _4_3 +", "+ _4_4 +")"
}

/** Contains factory methods for matrices in `R4x4`. */
object MatrixR4x4 {
  /** The zero matrix of `R4x4`. */
  def Zero: MatrixR4x4 =
    new MatrixR4x4(
      0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0)
  
  /** The identity matrix of `R4x4`. */
  def Identity: MatrixR4x4 =
    new MatrixR4x4(
      1.0, 0.0, 0.0, 0.0,
      0.0, 1.0, 0.0, 0.0,
      0.0, 0.0, 1.0, 0.0,
      0.0, 0.0, 0.0, 1.0)
  
  def apply(
      _1_1: Double, _1_2: Double, _1_3: Double, _1_4: Double,
      _2_1: Double, _2_2: Double, _2_3: Double, _2_4: Double,
      _3_1: Double, _3_2: Double, _3_3: Double, _3_4: Double,
      _4_1: Double, _4_2: Double, _4_3: Double, _4_4: Double): MatrixR4x4 =
    new MatrixR4x4(
      _1_1, _1_2, _1_3, _1_4,
      _2_1, _2_2, _2_3, _2_4,
      _3_1, _3_2, _3_3, _3_4,
      _4_1, _4_2, _4_3, _4_4)
  
  def columns(
      column1: VectorR4,
      column2: VectorR4,
      column3: VectorR4,
      column4: VectorR4): MatrixR4x4 =
    new MatrixR4x4(
      column1.x, column2.x, column3.x, column4.x,
      column1.y, column2.y, column3.y, column4.y,
      column1.z, column2.z, column3.z, column4.z,
      column1.w, column2.w, column3.w, column4.w)
  
  def rows(
      row1: VectorR4,
      row2: VectorR4,
      row3: VectorR4,
      row4: VectorR4): MatrixR4x4 =
    new MatrixR4x4(
      row1.x, row1.y, row1.z, row1.w,
      row2.x, row2.y, row2.z, row2.w,
      row3.x, row3.y, row3.z, row3.w,
      row4.x, row4.y, row4.z, row4.w)
}
