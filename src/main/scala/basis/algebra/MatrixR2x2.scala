/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.util.MurmurHash._

final class MatrixR2x2(
    val _1_1: Double, val _1_2: Double,
    val _2_1: Double, val _2_2: Double)
  extends Matrix[MatrixR2x2, MatrixR2x2, VectorR2, VectorR2, Real] {
  
  def column1: VectorR2 = new VectorR2(_1_1, _2_1)
  
  def column2: VectorR2 = new VectorR2(_1_2, _2_2)
  
  def row1: VectorR2 = new VectorR2(_1_1, _1_2)
  
  def row2: VectorR2 = new VectorR2(_2_1, _2_2)
  
  def + (that: MatrixR2x2): MatrixR2x2 =
    new MatrixR2x2(
      _1_1 + that._1_1, _1_2 + that._1_2,
      _2_1 + that._2_1, _2_2 + that._2_2)
  
  def unary_- : MatrixR2x2 =
    new MatrixR2x2(
      -_1_1, -_1_2,
      -_2_1, -_2_2)
  
  def - (that: MatrixR2x2): MatrixR2x2 =
    new MatrixR2x2(
      _1_1 - that._1_1, _1_2 - that._1_2,
      _2_1 - that._2_1, _2_2 - that._2_2)
  
  def :* (scalar: Real): MatrixR2x2 =
    this :* scalar.toDouble
  
  def :* (scalar: Double): MatrixR2x2 =
    new MatrixR2x2(
      _1_1 * scalar, _1_2 * scalar,
      _2_1 * scalar, _2_2 * scalar)
  
  def *: (scalar: Real): MatrixR2x2 =
    this :* scalar.toDouble
  
  def *: (scalar: Double): MatrixR2x2 =
    this :* scalar
  
  def / (scalar: Real): MatrixR2x2 =
    this / scalar.toDouble
  
  def / (scalar: Double): MatrixR2x2 =
    new MatrixR2x2(
      _1_1 / scalar, _1_2 / scalar,
      _2_1 / scalar, _2_2 / scalar)
  
  def :⋅ (column: VectorR2): VectorR2 =
    new VectorR2(
      _1_1 * column.x + _1_2 * column.y,
      _2_1 * column.x + _2_2 * column.y)
  
  def ⋅: (row: VectorR2): VectorR2 =
    new VectorR2(
      row.x * _1_1 + row.y * _2_1,
      row.x * _1_2 + row.y * _2_2)
  
  def transpose: MatrixR2x2 =
    new MatrixR2x2(
      _1_1, _2_1,
      _1_2, _2_2)
  
  def determinant: Real = new Real(_1_1 * _2_2 - _1_2 * _2_1)
  
  def * (that: MatrixR2x2): MatrixR2x2 =
    new MatrixR2x2(
      _1_1 * that._1_1 + _1_2 * that._2_1,
      _1_1 * that._1_2 + _1_2 * that._2_2,
      _2_1 * that._1_1 + _2_2 * that._2_1,
      _2_1 * that._1_2 + _2_2 * that._2_2)
  
  def inverse: MatrixR2x2 = {
    val det = _1_1 * _2_2 - _1_2 * _2_1
    new MatrixR2x2(
       _2_2 / det, -_1_2 / det,
      -_2_1 / det,  _1_1 / det)
  }
  
  override def equals(other: Any): Boolean = other match {
    case that: MatrixR2x2 =>
      _1_1 == that._1_1 && _1_2 == that._1_2 &&
      _2_1 == that._2_1 && _2_2 == that._2_2
    case _ => false
  }
  
  override def hashCode: Int =
    mash(mix(mix(mix(mix(1138706033,
      _1_1), _1_2),
      _2_1), _2_2))
  
  override def toString: String =
    "MatrixR2x2"+"("+
      _1_1 +", "+ _1_2 +",  "+
      _2_1 +", "+ _2_2 +")"
}

object MatrixR2x2 {
  def Zero: MatrixR2x2 =
    new MatrixR2x2(
      0.0, 0.0,
      0.0, 0.0)
  
  def Identity: MatrixR2x2 =
    new MatrixR2x2(
      1.0, 0.0,
      0.0, 1.0)
  
  def apply(
      _1_1: Double, _1_2: Double,
      _2_1: Double, _2_2: Double): MatrixR2x2 =
    new MatrixR2x2(
      _1_1, _1_2,
      _2_1, _2_2)
  
  def columns(
      column1: VectorR2,
      column2: VectorR2): MatrixR2x2 =
    new MatrixR2x2(
      column1.x, column2.x,
      column1.y, column2.y)
  
  def rows(
      row1: VectorR2,
      row2: VectorR2): MatrixR2x2 =
    new MatrixR2x2(
      row1.x, row1.y,
      row2.x, row2.y)
}
