/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.util.MurmurHash._

class F2[F <: Field[F]](field: ScalarSpace[F]) extends VectorSpace {
  type Scalar = F
  
  final class Vector(val x: Scalar, val y: Scalar)
    extends basis.algebra.Vector[Vector, Scalar] {
    
    def + (that: Vector): Vector =
      new Vector(x + that.x, y + that.y)
    
    def unary_- : Vector = new Vector(-x, -y)
    
    def - (that: Vector): Vector =
      new Vector(x - that.x, y - that.y)
    
    def :* (scalar: Scalar): Vector =
      new Vector(x * scalar, y * scalar)
    
    def *: (scalar: Scalar): Vector =
      new Vector(scalar * x, scalar * y)
    
    def / (scalar: Scalar): Vector =
      new Vector(x / scalar, y / scalar)
    
    override def equals(other: Any): Boolean = other match {
      case that: Vector =>
        x.equals(that.x) && y.equals(that.y)
      case _ => false
    }
    
    override def hashCode: Int =
      mash(mix(mix(-1227102225, x), y))
    
    override def toString: String =
      "<"+ x +", "+ y +">"
  }
  
  final class Matrix(
      val _1_1: Scalar, val _1_2: Scalar,
      val _2_1: Scalar, val _2_2: Scalar)
    extends basis.algebra.Vector[Matrix, Scalar] {
    
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
    
    def :* (scalar: Scalar): Matrix =
      new Matrix(
        _1_1 * scalar, _1_2 * scalar,
        _2_1 * scalar, _2_2 * scalar)
    
    def *: (scalar: Scalar): Matrix =
      new Matrix(
        scalar * _1_1, scalar * _1_2,
        scalar * _2_1, scalar * _2_2)
    
    def / (scalar: Scalar): Matrix =
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
      if (det != Scalar.zero)
        Some(new Matrix(
           _2_2 / det, -_1_2 / det,
          -_2_1 / det,  _1_1 / det))
      else None
    }
    
    def transpose: Matrix =
      new Matrix(
        _1_1, _2_1,
        _1_2, _2_2)
    
    def determinant: Scalar = _1_1 * _2_2 - _1_2 * _2_1
    
    override def equals(other: Any): Boolean = other match {
      case that: Matrix =>
        _1_1.equals(that._1_1) && _1_2.equals(that._1_2) &&
        _2_1.equals(that._2_1) && _2_2.equals(that._2_2)
      case _ => false
    }
    
    override def hashCode: Int =
      mash(mix(mix(mix(mix(-1487954323,
        _1_1), _1_2),
        _2_1), _2_2))
    
    override def toString: String =
      "[<"+ _1_1 +", "+ _1_2 +">, <"+
            _2_1 +", "+ _2_2 +">]"
  }
  
  class Endomorphism extends VectorSpace {
    type Scalar = F2.this.Scalar
    type Vector = Matrix
    type ColumnVector = F2.this.Vector
    type RowVector = F2.this.Vector
    type Matrix = F2.this.Matrix
    
    val Scalar = F2.this.Scalar
    
    lazy val zero: Matrix =
      new Matrix(
        Scalar.zero, Scalar.zero,
        Scalar.zero, Scalar.zero)
    
    lazy val identity: Matrix =
      new Matrix(
        Scalar.unit, Scalar.zero,
        Scalar.zero, Scalar.unit)
    
    def apply(
        _1_1: Scalar, _1_2: Scalar,
        _2_1: Scalar, _2_2: Scalar): Matrix =
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
    
    override def toString: String = "<"+ F2.this +" => "+ F2.this +">"
  }
  
  val Scalar = field: ScalarSpace[Scalar]
  
  lazy val Matrix = new Endomorphism
  
  lazy val zero: Vector = new Vector(Scalar.zero, Scalar.zero)
  
  def apply(x: Scalar, y: Scalar): Vector =
    new Vector(x, y)
  
  def unapply(vector: Vector): Some[(Scalar, Scalar)] =
    Some(vector.x, vector.y)
  
  override def toString: String = "F2"+"("+ Scalar +")"
}
