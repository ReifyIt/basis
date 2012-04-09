/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import basis.util.MurmurHash._

class F4[S <: Field[S]](field: ScalarSpace[S]) extends VectorSpace {
  type Scalar = S
  
  final class Vector(val x: Scalar, val y: Scalar, val z: Scalar, val w: Scalar)
    extends basis.algebra.Vector[Vector, Scalar] {
    
    def + (that: Vector): Vector =
      new Vector(x + that.x, y + that.y, z + that.z, w + that.w)
    
    def unary_- : Vector = new Vector(-x, -y, -z, -w)
    
    def - (that: Vector): Vector =
      new Vector(x - that.x, y - that.y, z - that.z, w - that.w)
    
    def :* (scalar: Scalar): Vector =
      new Vector(x * scalar, y * scalar, z * scalar, w * scalar)
    
    def *: (scalar: Scalar): Vector =
      new Vector(scalar * x, scalar * y, scalar * z, scalar * w)
    
    def / (scalar: Scalar)(implicit isField: Scalar <:< Field[Scalar]): Vector =
      new Vector(x / scalar, y / scalar, z / scalar, w / scalar)
    
    override def equals(other: Any): Boolean = other match {
      case that: Vector =>
        x.equals(that.x) && y.equals(that.y) && z.equals(that.z) && w.equals(that.w)
      case _ => false
    }
    
    override def hashCode: Int =
      mash(mix(mix(mix(mix(-2036448851, x), y), z), w))
    
    override def toString: String =
      "<"+ x +", "+ y +", "+ z +", "+ w +">"
  }
  
  final class Matrix(
      val _1_1: Scalar, val _1_2: Scalar, val _1_3: Scalar, val _1_4: Scalar,
      val _2_1: Scalar, val _2_2: Scalar, val _2_3: Scalar, val _2_4: Scalar,
      val _3_1: Scalar, val _3_2: Scalar, val _3_3: Scalar, val _3_4: Scalar,
      val _4_1: Scalar, val _4_2: Scalar, val _4_3: Scalar, val _4_4: Scalar)
    extends basis.algebra.Vector[Matrix, Scalar] {
    
    def column1: Vector = new Vector(_1_1, _2_1, _3_1, _4_1)
    
    def column2: Vector = new Vector(_1_2, _2_2, _3_2, _4_2)
    
    def column3: Vector = new Vector(_1_3, _2_3, _3_3, _4_3)
    
    def column4: Vector = new Vector(_1_4, _2_4, _3_4, _4_4)
    
    def row1: Vector = new Vector(_1_1, _1_2, _1_3, _1_4)
    
    def row2: Vector = new Vector(_2_1, _2_2, _2_3, _2_4)
    
    def row3: Vector = new Vector(_3_1, _3_2, _3_3, _3_4)
    
    def row4: Vector = new Vector(_4_1, _4_2, _4_3, _4_4)
    
    def + (that: Matrix): Matrix =
      new Matrix(
        _1_1 + that._1_1, _1_2 + that._1_2, _1_3 + that._1_3, _1_4 + that._1_4,
        _2_1 + that._2_1, _2_2 + that._2_2, _2_3 + that._2_3, _2_4 + that._2_4,
        _3_1 + that._3_1, _3_2 + that._3_2, _3_3 + that._3_3, _3_4 + that._3_4,
        _4_1 + that._4_1, _4_2 + that._4_2, _4_3 + that._4_3, _4_4 + that._4_4)
    
    def unary_- : Matrix =
      new Matrix(
        -_1_1, -_1_2, -_1_3, -_1_4,
        -_2_1, -_2_2, -_2_3, -_2_4,
        -_3_1, -_3_2, -_3_3, -_3_4,
        -_4_1, -_4_2, -_4_3, -_4_4)
    
    def - (that: Matrix): Matrix =
      new Matrix(
        _1_1 - that._1_1, _1_2 - that._1_2, _1_3 - that._1_3, _1_4 - that._1_4,
        _2_1 - that._2_1, _2_2 - that._2_2, _2_3 - that._2_3, _2_4 - that._2_4,
        _3_1 - that._3_1, _3_2 - that._3_2, _3_3 - that._3_3, _3_4 - that._3_4,
        _4_1 - that._4_1, _4_2 - that._4_2, _4_3 - that._4_3, _4_4 - that._4_4)
    
    def :* (scalar: Scalar): Matrix =
      new Matrix(
        _1_1 * scalar, _1_2 * scalar, _1_3 * scalar, _1_4 * scalar,
        _2_1 * scalar, _2_2 * scalar, _2_3 * scalar, _2_4 * scalar,
        _3_1 * scalar, _3_2 * scalar, _3_3 * scalar, _3_4 * scalar,
        _4_1 * scalar, _4_2 * scalar, _4_3 * scalar, _4_4 * scalar)
    
    def *: (scalar: Scalar): Matrix =
      new Matrix(
        scalar * _1_1, scalar * _1_2, scalar * _1_3, scalar * _1_4,
        scalar * _2_1, scalar * _2_2, scalar * _2_3, scalar * _2_4,
        scalar * _3_1, scalar * _3_2, scalar * _3_3, scalar * _3_4,
        scalar * _4_1, scalar * _4_2, scalar * _4_3, scalar * _4_4)
    
    def / (scalar: Scalar): Matrix =
      new Matrix(
        _1_1 / scalar, _1_2 / scalar, _1_3 / scalar, _1_4 / scalar,
        _2_1 / scalar, _2_2 / scalar, _2_3 / scalar, _2_4 / scalar,
        _3_1 / scalar, _3_2 / scalar, _3_3 / scalar, _3_4 / scalar,
        _4_1 / scalar, _4_2 / scalar, _4_3 / scalar, _4_4 / scalar)
    
    def :* (column: Vector): Vector =
      new Vector(
        _1_1 * column.x + _1_2 * column.y + _1_3 * column.z + _1_4 * column.w,
        _2_1 * column.x + _2_2 * column.y + _2_3 * column.z + _2_4 * column.w,
        _3_1 * column.x + _3_2 * column.y + _3_3 * column.z + _3_4 * column.w,
        _4_1 * column.x + _4_2 * column.y + _4_3 * column.z + _4_4 * column.w)
    
    def *: (row: Vector): Vector =
      new Vector(
        row.x * _1_1 + row.y * _2_1 + row.z * _3_1 + row.w * _4_1,
        row.x * _1_2 + row.y * _2_2 + row.z * _3_2 + row.w * _4_2,
        row.x * _1_3 + row.y * _2_3 + row.z * _3_3 + row.w * _4_3,
        row.x * _1_4 + row.y * _2_4 + row.z * _3_4 + row.w * _4_4)
    
    def * (that: Matrix): Matrix =
      new Matrix(
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
    
    def inverse: Option[Matrix] = {
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
      if (det != Scalar.zero)
        Some(new Matrix(
           minor_1_1 / det, -minor_2_1 / det,  minor_3_1 / det, -minor_4_1 / det,
          -minor_1_2 / det,  minor_2_2 / det, -minor_3_2 / det,  minor_4_2 / det,
           minor_1_3 / det, -minor_2_3 / det,  minor_3_3 / det, -minor_4_3 / det,
          -minor_1_4 / det,  minor_2_4 / det, -minor_3_4 / det,  minor_4_4 / det))
      else None
    }
    
    def transpose: Matrix =
      new Matrix(
        _1_1, _2_1, _3_1, _4_1,
        _1_2, _2_2, _3_2, _4_2,
        _1_3, _2_3, _3_3, _4_3,
        _1_4, _2_4, _3_4, _4_4)
    
    def determinant: Scalar = {
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
      
      _1_1 * minor_1_1 - _1_2 * minor_1_2 + _1_3 * minor_1_3 - _1_4 * minor_1_4
    }
    
    override def equals(other: Any): Boolean = other match {
      case that: Matrix =>
        _1_1.equals(that._1_1) && _1_2.equals(that._1_2) && _1_3.equals(that._1_3) && _1_4.equals(that._1_4) &&
        _2_1.equals(that._2_1) && _2_2.equals(that._2_2) && _2_3.equals(that._2_3) && _2_4.equals(that._2_4) &&
        _3_1.equals(that._3_1) && _3_2.equals(that._3_2) && _3_3.equals(that._3_3) && _3_4.equals(that._3_4) &&
        _4_1.equals(that._4_1) && _4_2.equals(that._4_2) && _4_3.equals(that._4_3) && _4_4.equals(that._4_4)
      case _ => false
    }
    
    override def hashCode: Int =
      mash(mix(mix(mix(mix(mix(mix(mix(mix(mix(mix(mix(mix(mix(mix(mix(mix(1997666347,
        _1_1), _1_2), _1_3), _1_4),
        _2_1), _2_2), _2_3), _2_4),
        _3_1), _3_2), _3_3), _3_4),
        _4_1), _4_2), _4_3), _4_4))
    
    override def toString: String =
      "[<"+ _1_1 +", "+ _1_2 +", "+ _1_3 +", "+ _1_4 +">, <"+
            _2_1 +", "+ _2_2 +", "+ _2_3 +", "+ _2_4 +">, <"+
            _3_1 +", "+ _3_2 +", "+ _3_3 +", "+ _3_4 +">, <"+
            _4_1 +", "+ _4_2 +", "+ _4_3 +", "+ _4_4 +">]"
  }
  
  class Endomorphism extends VectorSpace {
    type Scalar = F4.this.Scalar
    type Vector = Matrix
    type ColumnVector = F4.this.Vector
    type RowVector = F4.this.Vector
    type Matrix = F4.this.Matrix
    
    val Scalar = F4.this.Scalar
    
    lazy val zero: Matrix =
      new Matrix(
        Scalar.zero, Scalar.zero, Scalar.zero, Scalar.zero,
        Scalar.zero, Scalar.zero, Scalar.zero, Scalar.zero,
        Scalar.zero, Scalar.zero, Scalar.zero, Scalar.zero,
        Scalar.zero, Scalar.zero, Scalar.zero, Scalar.zero)
    
    lazy val identity: Matrix =
      new Matrix(
        Scalar.unit, Scalar.zero, Scalar.zero, Scalar.zero,
        Scalar.zero, Scalar.unit, Scalar.zero, Scalar.zero,
        Scalar.zero, Scalar.zero, Scalar.unit, Scalar.zero,
        Scalar.zero, Scalar.zero, Scalar.zero, Scalar.unit)
    
    def apply(
        _1_1: Scalar, _1_2: Scalar, _1_3: Scalar, _1_4: Scalar,
        _2_1: Scalar, _2_2: Scalar, _2_3: Scalar, _2_4: Scalar,
        _3_1: Scalar, _3_2: Scalar, _3_3: Scalar, _3_4: Scalar,
        _4_1: Scalar, _4_2: Scalar, _4_3: Scalar, _4_4: Scalar): Matrix =
      new Matrix(
        _1_1, _1_2, _1_3, _1_4,
        _2_1, _2_2, _2_3, _2_4,
        _3_1, _3_2, _3_3, _3_4,
        _4_1, _4_2, _4_3, _4_4)
    
    def columns(
        column1: ColumnVector,
        column2: ColumnVector,
        column3: ColumnVector,
        column4: ColumnVector): Matrix =
      new Matrix(
        column1.x, column2.x, column3.x, column4.x,
        column1.y, column2.y, column3.y, column4.y,
        column1.z, column2.z, column3.z, column4.z,
        column1.w, column2.w, column3.w, column4.w)
    
    def rows(
        row1: RowVector,
        row2: RowVector,
        row3: RowVector,
        row4: RowVector): Matrix =
      new Matrix(
        row1.x, row1.y, row1.z, row1.w,
        row2.x, row2.y, row2.z, row2.w,
        row3.x, row3.y, row3.z, row3.w,
        row4.x, row4.y, row4.z, row4.w)
    
    override def toString: String = "<"+ F4.this +" => "+ F4.this +">"
  }
  
  val Scalar = field: ScalarSpace[Scalar]
  
  lazy val Matrix = new Endomorphism
  
  lazy val zero: Vector = new Vector(Scalar.zero, Scalar.zero, Scalar.zero, Scalar.zero)
  
  def apply(x: Scalar, y: Scalar, z: Scalar, w: Scalar): Vector =
    new Vector(x, y, z, w)
  
  def unapply(vector: Vector): Some[(Scalar, Scalar, Scalar, Scalar)] =
    Some(vector.x, vector.y, vector.z, vector.w)
  
  override def toString: String = "F4"+"("+ Scalar +")"
}
