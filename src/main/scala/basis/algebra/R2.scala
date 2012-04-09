/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.util.MurmurHash._

object R2 extends HilbertSpace {
  type Scalar = Real
  
  final class Vector(val x: Double, val y: Double)
    extends RealVector[Vector] {
    
    def + (that: Vector): Vector =
      new Vector(x + that.x, y + that.y)
    
    def unary_- : Vector = new Vector(-x, -y)
    
    def - (that: Vector): Vector =
      new Vector(x - that.x, y - that.y)
    
    def :* (scalar: Double): Vector =
      new Vector(x * scalar, y * scalar)
    
    def *: (scalar: Double): Vector = this :* scalar
    
    def / (scalar: Double): Vector =
      new Vector(x / scalar, y / scalar)
    
    def ⋅ (that: Vector): Double = x * that.x + y * that.y
    
    def norm: Double = math.sqrt(x * x + y * y)
    
    override def equals(other: Any): Boolean = other match {
      case that: Vector => x == that.x && y == that.y
      case _ => false
    }
    
    override def hashCode: Int =
      mash(mix(mix(-1441719301, x), y))
    
    override def toString: String =
      "<"+ x +", "+ y +">"
  }
  
  final class Matrix(
      val _1_1: Double, val _1_2: Double,
      val _2_1: Double, val _2_2: Double)
    extends RealVector[Matrix] {
    
    def column1: Vector = new Vector(_1_1, _2_1)
    
    def column2: Vector = new Vector(_1_2, _2_2)
    
    def row1: Vector = new Vector(_1_1, _1_2)
    
    def row2: Vector = new Vector(_2_1, _2_2)
    
    def + (that: Matrix): Matrix =
      new Matrix(
        _1_1 + that._1_1, _1_2 + that._1_2,
        _2_1 + that._2_1, _2_2 + that._2_2)
    
    def unary_- : Matrix =
      new Matrix(
        -_1_1, -_1_2,
        -_2_1, -_2_2)
    
    def - (that: Matrix): Matrix =
      new Matrix(
        _1_1 - that._1_1, _1_2 - that._1_2,
        _2_1 - that._2_1, _2_2 - that._2_2)
    
    def :* (scalar: Double): Matrix =
      new Matrix(
        _1_1 * scalar, _1_2 * scalar,
        _2_1 * scalar, _2_2 * scalar)
    
    def *: (scalar: Double): Matrix = this :* scalar
    
    def / (scalar: Double): Matrix =
      new Matrix(
        _1_1 / scalar, _1_2 / scalar,
        _2_1 / scalar, _2_2 / scalar)
    
    def :* (column: Vector): Vector =
      new Vector(
        _1_1 * column.x + _1_2 * column.y,
        _2_1 * column.x + _2_2 * column.y)
    
    def *: (row: Vector): Vector =
      new Vector(
        row.x * _1_1 + row.y * _2_1,
        row.x * _1_2 + row.y * _2_2)
    
    def * (that: Matrix): Matrix =
      new Matrix(
        _1_1 * that._1_1 + _1_2 * that._2_1,
        _1_1 * that._1_2 + _1_2 * that._2_2,
        _2_1 * that._1_1 + _2_2 * that._2_1,
        _2_1 * that._1_2 + _2_2 * that._2_2)
    
    def inverse: Option[Matrix] = {
      val det = _1_1 * _2_2 - _1_2 * _2_1
      if (math.abs(det) >= java.lang.Double.MIN_NORMAL)
        Some(new Matrix(
           _2_2 / det, -_1_2 / det,
          -_2_1 / det,  _1_1 / det))
      else None
    }
    
    def transpose: Matrix =
      new Matrix(
        _1_1, _2_1,
        _1_2, _2_2)
    
    def determinant: Double = _1_1 * _2_2 - _1_2 * _2_1
    
    override def equals(other: Any): Boolean = other match {
      case that: Matrix =>
        _1_1 == that._1_1 && _1_2 == that._1_2 &&
        _2_1 == that._2_1 && _2_2 == that._2_2
      case _ => false
    }
    
    override def hashCode: Int =
      mash(mix(mix(mix(mix(-1702571399,
        _1_1), _1_2),
        _2_1), _2_2))
    
    override def toString: String =
      "[<"+ _1_1 +", "+ _1_2 +">, <"+
            _2_1 +", "+ _2_2 +">]"
  }
  
  class Endomorphism extends VectorSpace {
    type Scalar = Real
    type Vector = Matrix
    type ColumnVector = R2.Vector
    type RowVector = R2.Vector
    type Matrix = R2.Matrix
    
    val Scalar = Real
    
    val zero: Matrix =
      new Matrix(
        0.0, 0.0,
        0.0, 0.0)
    
    val identity: Matrix =
      new Matrix(
        1.0, 0.0,
        0.0, 1.0)
    
    def apply(
        _1_1: Double, _1_2: Double,
        _2_1: Double, _2_2: Double): Matrix =
      new Matrix(
        _1_1, _1_2,
        _2_1, _2_2)
    
    def columns(
        column1: ColumnVector,
        column2: ColumnVector): Matrix =
      new Matrix(
        column1.x, column2.x,
        column1.y, column2.y)
    
    def rows(
        row1: RowVector,
        row2: RowVector): Matrix =
      new Matrix(
        row1.x, row1.y,
        row2.x, row2.y)
    
    override def toString: String = "<R2 => R2>"
  }
  
  val Scalar = Real
  
  lazy val Matrix = new Endomorphism
  
  val zero: Vector = new Vector(0.0, 0.0)
  
  def apply(x: Double, y: Double): Vector =
    new Vector(x, y)
  
  def unapply(vector: Vector): Some[(Double, Double)] =
    Some(vector.x, vector.y)
  
  def innerProduct(u: Vector, v: Vector): Scalar = new Scalar(u ⋅ v)
  
  override def norm(u: Vector): Scalar = new Scalar(u.norm)
  
  override def normalize(u: Vector): Vector = u / u.norm
  
  override def distance(u: Vector, v: Vector): Scalar = {
    val dx = u.x - v.x
    val dy = u.y - v.y
    new Scalar(math.sqrt(dx * dx + dy * dy))
  }
  
  override def toString: String = "R2"
}
