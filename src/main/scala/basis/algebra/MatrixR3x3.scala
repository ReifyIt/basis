/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.util.MurmurHash._

/** A 3x3 matrix of `Real` values.
  * 
  * @author Chris Sachs
  * 
  * @constructor Constructs a matrix with nine row-major `Double` values.
  * @param  _1_1  The entry at row 1, column 1.
  * @param  _1_2  The entry at row 1, column 2.
  * @param  _1_3  The entry at row 1, column 3.
  * @param  _2_1  The entry at row 2, column 1.
  * @param  _2_2  The entry at row 2, column 2.
  * @param  _2_3  The entry at row 2, column 3.
  * @param  _3_1  The entry at row 3, column 1.
  * @param  _3_2  The entry at row 3, column 2.
  * @param  _3_3  The entry at row 3, column 3.
  * 
  * @define scalar  `Real` value
  */
final class MatrixR3x3(
    val _1_1: Double, val _1_2: Double, val _1_3: Double,
    val _2_1: Double, val _2_2: Double, val _2_3: Double,
    val _3_1: Double, val _3_2: Double, val _3_3: Double)
  extends RealVector[MatrixR3x3]
    with SquareMatrix[MatrixR3x3, VectorR3, Real] {
  
  /** The vector in the first column of the matrix. */
  def column1: VectorR3 = new VectorR3(_1_1, _2_1, _3_1)
  
  /** The vector in the second column of the matrix. */
  def column2: VectorR3 = new VectorR3(_1_2, _2_2, _3_2)
  
  /** The vector in the third column of the matrix. */
  def column3: VectorR3 = new VectorR3(_1_3, _2_3, _3_3)
  
  /** The vector in the first row of the matrix. */
  def row1: VectorR3 = new VectorR3(_1_1, _1_2, _1_3)
  
  /** The vector in the second row of the matrix. */
  def row2: VectorR3 = new VectorR3(_2_1, _2_2, _2_3)
  
  /** The vector in the third row of the matrix. */
  def row3: VectorR3 = new VectorR3(_3_1, _3_2, _3_3)
  
  def + (that: MatrixR3x3): MatrixR3x3 =
    new MatrixR3x3(
      _1_1 + that._1_1, _1_2 + that._1_2, _1_3 + that._1_3,
      _2_1 + that._2_1, _2_2 + that._2_2, _2_3 + that._2_3,
      _3_1 + that._3_1, _3_2 + that._3_2, _3_3 + that._3_3)
  
  def unary_- : MatrixR3x3 =
    new MatrixR3x3(
      -_1_1, -_1_2, -_1_3,
      -_2_1, -_2_2, -_2_3,
      -_3_1, -_3_2, -_3_3)
  
  def - (that: MatrixR3x3): MatrixR3x3 =
    new MatrixR3x3(
      _1_1 - that._1_1, _1_2 - that._1_2, _1_3 - that._1_3,
      _2_1 - that._2_1, _2_2 - that._2_2, _2_3 - that._2_3,
      _3_1 - that._3_1, _3_2 - that._3_2, _3_3 - that._3_3)
  
  def :* (scalar: Double): MatrixR3x3 =
    new MatrixR3x3(
      _1_1 * scalar, _1_2 * scalar, _1_3 * scalar,
      _2_1 * scalar, _2_2 * scalar, _2_3 * scalar,
      _3_1 * scalar, _3_2 * scalar, _3_3 * scalar)
  
  def *: (scalar: Double): MatrixR3x3 =
    this :* scalar
  
  def / (scalar: Double): MatrixR3x3 =
    new MatrixR3x3(
      _1_1 / scalar, _1_2 / scalar, _1_3 / scalar,
      _2_1 / scalar, _2_2 / scalar, _2_3 / scalar,
      _3_1 / scalar, _3_2 / scalar, _3_3 / scalar)
  
  def :⋅ (column: VectorR3): VectorR3 =
    new VectorR3(
      _1_1 * column.x + _1_2 * column.y + _1_3 * column.z,
      _2_1 * column.x + _2_2 * column.y + _2_3 * column.z,
      _3_1 * column.x + _3_2 * column.y + _3_3 * column.z)
  
  def ⋅: (row: VectorR3): VectorR3 =
    new VectorR3(
      row.x * _1_1 + row.y * _2_1 + row.z * _3_1,
      row.x * _1_2 + row.y * _2_2 + row.z * _3_2,
      row.x * _1_3 + row.y * _2_3 + row.z * _3_3)
  
  def transpose: MatrixR3x3 =
    new MatrixR3x3(
      _1_1, _2_1, _3_1,
      _1_2, _2_2, _3_2,
      _1_3, _2_3, _3_3)
  
  def determinant: Real = {
    // 2x2 determinants minor_i_j with row i and column j blocked out.
    val minor_1_1 = _2_2 * _3_3 - _2_3 * _3_2
    val minor_1_2 = _2_1 * _3_3 - _2_3 * _3_1
    val minor_1_3 = _2_1 * _3_2 - _2_2 * _3_1
    new Real(_1_1 * minor_1_1 - _1_2 * minor_1_2 + _1_3 * minor_1_3)
  }
  
  def * (that: MatrixR3x3): MatrixR3x3 =
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
  
  def inverse: Option[MatrixR3x3] = {
    // all 2x2 determinants minor_i_j with row i and column j blocked out.
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
    if (math.abs(det) >= java.lang.Double.MIN_NORMAL)
      Some(new MatrixR3x3(
         minor_1_1 / det, -minor_2_1 / det,  minor_3_1 / det,
        -minor_1_2 / det,  minor_2_2 / det, -minor_3_2 / det,
         minor_1_3 / det, -minor_2_3 / det,  minor_3_3 / det))
    else None
  }
  
  override def equals(other: Any): Boolean = other match {
    case that: MatrixR3x3 =>
      _1_1 == that._1_1 && _1_2 == that._1_2 && _1_3 == that._1_3 &&
      _2_1 == that._2_1 && _2_2 == that._2_2 && _2_3 == that._2_3 &&
      _3_1 == that._3_1 && _3_2 == that._3_2 && _3_3 == that._3_3
    case _ => false
  }
  
  override def hashCode: Int =
    mash(mix(mix(mix(mix(mix(mix(mix(mix(mix(1138706995,
      _1_1), _1_2), _1_3),
      _2_1), _2_2), _2_3),
      _3_1), _3_2), _3_3))
  
  override def toString: String =
    "MatrixR3x3"+"("+
      _1_1 +", "+ _1_2 +", "+ _1_3 +",  "+
      _2_1 +", "+ _2_2 +", "+ _2_3 +",  "+
      _3_1 +", "+ _3_2 +", "+ _3_3 +")"
}

/** Contains factory methods for matrices in `R3x3`. */
object MatrixR3x3 {
  def apply(
      _1_1: Double, _1_2: Double, _1_3: Double,
      _2_1: Double, _2_2: Double, _2_3: Double,
      _3_1: Double, _3_2: Double, _3_3: Double): MatrixR3x3 =
    new MatrixR3x3(
      _1_1, _1_2, _1_3,
      _2_1, _2_2, _2_3,
      _3_1, _3_2, _3_3)
  
  def columns(
      column1: VectorR3,
      column2: VectorR3,
      column3: VectorR3): MatrixR3x3 =
    new MatrixR3x3(
      column1.x, column2.x, column3.x,
      column1.y, column2.y, column3.y,
      column1.z, column2.z, column3.z)
  
  def rows(
      row1: VectorR3,
      row2: VectorR3,
      row3: VectorR3): MatrixR3x3 =
    new MatrixR3x3(
      row1.x, row1.y, row1.z,
      row2.x, row2.y, row2.z,
      row3.x, row3.y, row3.z)
  
  /** The additive identity of `R3x3`. */
  implicit val additiveIdentity =
    new AdditiveIdentity(
      new MatrixR3x3(
        0.0, 0.0, 0.0,
        0.0, 0.0, 0.0,
        0.0, 0.0, 0.0))
}
