/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.math
package binary64

/** A 4 by 4 double-precision floating-point real matrix space.
  * 
  * @author Chris Sachs
  * @since  0.0
  * @group  Real
  */
object R4x4 extends F4x4 with RMxN {
  final class Value(
      override val _1_1: Scalar, override val _1_2: Scalar, override val _1_3: Scalar, override val _1_4: Scalar,
      override val _2_1: Scalar, override val _2_2: Scalar, override val _2_3: Scalar, override val _2_4: Scalar,
      override val _3_1: Scalar, override val _3_2: Scalar, override val _3_3: Scalar, override val _3_4: Scalar,
      override val _4_1: Scalar, override val _4_2: Scalar, override val _4_3: Scalar, override val _4_4: Scalar)
    extends super[F4x4].Value
       with super[RMxN].Value {
    
    override def Row: R4.type = R4
    override def Col: R4.type = R4
    
    override def apply(k: Int): Scalar = k match {
      case  0 => _1_1
      case  1 => _1_2
      case  2 => _1_3
      case  3 => _1_4
      case  4 => _2_1
      case  5 => _2_2
      case  6 => _2_3
      case  7 => _2_4
      case  8 => _3_1
      case  9 => _3_2
      case 10 => _3_3
      case 11 => _3_4
      case 12 => _4_1
      case 13 => _4_2
      case 14 => _4_3
      case 15 => _4_4
      case _ => throw new java.lang.IndexOutOfBoundsException(k.toString)
    }
    
    override def apply(i: Int, j: Int): Scalar = {
      if (i < 0 || i >= 4 || j < 0 || j >= 4)
        throw new java.lang.IndexOutOfBoundsException("row "+ i +", "+"col "+ j)
      apply(4 * i + j)
    }
    
    override def row1: Row = new Row(_1_1, _1_2, _1_3, _1_4)
    override def row2: Row = new Row(_2_1, _2_2, _2_3, _2_4)
    override def row3: Row = new Row(_3_1, _3_2, _3_3, _3_4)
    override def row4: Row = new Row(_4_1, _4_2, _4_3, _4_4)
    
    override def row(i: Int): Row = i match {
      case 0 => row1
      case 1 => row2
      case 2 => row3
      case 3 => row4
      case _ => throw new java.lang.IndexOutOfBoundsException("row "+ i)
    }
    
    override def col1: Col = new Col(_1_1, _2_1, _3_1, _4_1)
    override def col2: Col = new Col(_1_2, _2_2, _3_2, _4_2)
    override def col3: Col = new Col(_1_3, _2_3, _3_3, _4_3)
    override def col4: Col = new Col(_1_4, _2_4, _3_4, _4_4)
    
    override def col(j: Int): Col = j match {
      case 0 => col1
      case 1 => col2
      case 2 => col3
      case 3 => col4
      case _ => throw new java.lang.IndexOutOfBoundsException("col "+ j)
    }
    
    override def + (that: Matrix): Matrix =
      new Matrix(
        _1_1 + that._1_1, _1_2 + that._1_2, _1_3 + that._1_3, _1_4 + that._1_4,
        _2_1 + that._2_1, _2_2 + that._2_2, _2_3 + that._2_3, _2_4 + that._2_4,
        _3_1 + that._3_1, _3_2 + that._3_2, _3_3 + that._3_3, _3_4 + that._3_4,
        _4_1 + that._4_1, _4_2 + that._4_2, _4_3 + that._4_3, _4_4 + that._4_4)
    
    override def unary_- : Matrix =
      new Matrix(
        -_1_1, -_1_2, -_1_3, -_1_4,
        -_2_1, -_2_2, -_2_3, -_2_4,
        -_3_1, -_3_2, -_3_3, -_3_4,
        -_4_1, -_4_2, -_4_3, -_4_4)
    
    override def - (that: Matrix): Matrix =
      new Matrix(
        _1_1 - that._1_1, _1_2 - that._1_2, _1_3 - that._1_3, _1_4 - that._1_4,
        _2_1 - that._2_1, _2_2 - that._2_2, _2_3 - that._2_3, _2_4 - that._2_4,
        _3_1 - that._3_1, _3_2 - that._3_2, _3_3 - that._3_3, _3_4 - that._3_4,
        _4_1 - that._4_1, _4_2 - that._4_2, _4_3 - that._4_3, _4_4 - that._4_4)
    
    override def :* (scalar: Scalar): Matrix =
      new Matrix(
        _1_1 * scalar, _1_2 * scalar, _1_3 * scalar, _1_4 * scalar,
        _2_1 * scalar, _2_2 * scalar, _2_3 * scalar, _2_4 * scalar,
        _3_1 * scalar, _3_2 * scalar, _3_3 * scalar, _3_4 * scalar,
        _4_1 * scalar, _4_2 * scalar, _4_3 * scalar, _4_4 * scalar)
    
    override def *: (scalar: Scalar): Matrix = this :* scalar
    
    override def ∘ (that: Matrix): Matrix =
      new Matrix(
        _1_1 * that._1_1, _1_2 * that._1_2, _1_3 * that._1_3, _1_4 * that._1_4,
        _2_1 * that._2_1, _2_2 * that._2_2, _2_3 * that._2_3, _2_4 * that._2_4,
        _3_1 * that._3_1, _3_2 * that._3_2, _3_3 * that._3_3, _3_4 * that._3_4,
        _4_1 * that._4_1, _4_2 * that._4_2, _4_3 * that._4_3, _4_4 * that._4_4)
    
    override def :⋅ (vector: Row): Col =
      new Col(
        _1_1 * vector.x + _1_2 * vector.y + _1_3 * vector.z + _1_4 * vector.w,
        _2_1 * vector.x + _2_2 * vector.y + _2_3 * vector.z + _2_4 * vector.w,
        _3_1 * vector.x + _3_2 * vector.y + _3_3 * vector.z + _3_4 * vector.w,
        _4_1 * vector.x + _4_2 * vector.y + _4_3 * vector.z + _4_4 * vector.w)
    
    override def ⋅: (vector: Col): Row =
      new Row(
        vector.x * _1_1 + vector.y * _2_1 + vector.z * _3_1 + vector.w * _4_1,
        vector.x * _1_2 + vector.y * _2_2 + vector.z * _3_2 + vector.w * _4_2,
        vector.x * _1_3 + vector.y * _2_3 + vector.z * _3_3 + vector.w * _4_3,
        vector.x * _1_4 + vector.y * _2_4 + vector.z * _3_4 + vector.w * _4_4)
    
    override def * (that: Matrix): Matrix =
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
    
    override def inverse: Matrix = {
      // all 2x2 determinants minor_i1_i2__j1_j2 with
      // rows i1 and i2, and cols j1 and j2 blocked out
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
      
      // all 3x3 determinants minor_i_j with
      // row i and col j blocked out
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
      new Matrix(
         minor_1_1 / det, -minor_2_1 / det,  minor_3_1 / det, -minor_4_1 / det,
        -minor_1_2 / det,  minor_2_2 / det, -minor_3_2 / det,  minor_4_2 / det,
         minor_1_3 / det, -minor_2_3 / det,  minor_3_3 / det, -minor_4_3 / det,
        -minor_1_4 / det,  minor_2_4 / det, -minor_3_4 / det,  minor_4_4 / det)
    }
    
    override def transpose: Matrix =
      new Matrix(
        _1_1, _2_1, _3_1, _4_1,
        _1_2, _2_2, _3_2, _4_2,
        _1_3, _2_3, _3_3, _4_3,
        _1_4, _2_4, _3_4, _4_4)
    
    override def det: Scalar = {
      val minor_1_2__1_2 = _3_3 * _4_4 - _3_4 * _4_3
      val minor_1_2__1_3 = _3_2 * _4_4 - _3_4 * _4_2
      val minor_1_2__1_4 = _3_2 * _4_3 - _3_3 * _4_2
      val minor_1_2__2_3 = _3_1 * _4_4 - _3_4 * _4_1
      val minor_1_2__2_4 = _3_1 * _4_3 - _3_3 * _4_1
      val minor_1_2__3_4 = _3_1 * _4_2 - _3_2 * _4_1
      
      val minor_1_1 = _2_2 * minor_1_2__1_2 - _2_3 * minor_1_2__1_3 + _2_4 * minor_1_2__1_4
      val minor_1_2 = _2_1 * minor_1_2__1_2 - _2_3 * minor_1_2__2_3 + _2_4 * minor_1_2__2_4
      val minor_1_3 = _2_1 * minor_1_2__1_3 - _2_2 * minor_1_2__2_3 + _2_4 * minor_1_2__3_4
      val minor_1_4 = _2_1 * minor_1_2__1_4 - _2_2 * minor_1_2__2_4 + _2_3 * minor_1_2__3_4
      
      _1_1 * minor_1_1 - _1_2 * minor_1_2 + _1_3 * minor_1_3 - _1_4 * minor_1_4
    }
    
    override def trace: Scalar = _1_1 + _2_2 + _3_3 + _4_4
  }
  
  override type Matrix = Value
  
  override val Transpose: this.type = this
  
  override val Row: R4.type = R4
  override val Col: R4.type = R4
  
  override val Scalar: Real.type = Real
  
  override def dim: Int = 16
  
  override val zero: Matrix = {
    val z = Scalar.zero
    new Matrix(z, z, z, z,  z, z, z, z,  z, z, z, z,  z, z, z, z)
  }
  
  override val unit: Matrix = {
    val z = Scalar.zero
    val u = Scalar.unit
    new Matrix(u, z, z, z,  z, u, z, z,  z, z, u, z,  z, z, z, u)
  }
  
  override def apply(
      _1_1: Scalar, _1_2: Scalar, _1_3: Scalar, _1_4: Scalar,
      _2_1: Scalar, _2_2: Scalar, _2_3: Scalar, _2_4: Scalar,
      _3_1: Scalar, _3_2: Scalar, _3_3: Scalar, _3_4: Scalar,
      _4_1: Scalar, _4_2: Scalar, _4_3: Scalar, _4_4: Scalar): Matrix =
    new Matrix(
      _1_1, _1_2, _1_3, _1_4,
      _2_1, _2_2, _2_3, _2_4,
      _3_1, _3_2, _3_3, _3_4,
      _4_1, _4_2, _4_3, _4_4)
  
  override def apply(entries: Array[Double]): Matrix = {
    if (entries.length != 16) throw new DimensionException
    new Matrix(
      new Scalar(entries( 0)), new Scalar(entries( 1)), new Scalar(entries( 2)), new Scalar(entries( 3)),
      new Scalar(entries( 4)), new Scalar(entries( 5)), new Scalar(entries( 6)), new Scalar(entries( 7)),
      new Scalar(entries( 8)), new Scalar(entries( 9)), new Scalar(entries(10)), new Scalar(entries(11)),
      new Scalar(entries(12)), new Scalar(entries(13)), new Scalar(entries(14)), new Scalar(entries(15)))
  }
  
  override def rows(row1: Row, row2: Row, row3: Row, row4: Row): Matrix =
    new Matrix(
      row1.x, row1.y, row1.z, row1.w,
      row2.x, row2.y, row2.z, row2.w,
      row3.x, row3.y, row3.z, row3.w,
      row4.x, row4.y, row4.z, row4.w)
  
  override def cols(col1: Col, col2: Col, col3: Col, col4: Col): Matrix =
    new Matrix(
      col1.x, col2.x, col3.x, col4.x,
      col1.y, col2.y, col3.y, col4.y,
      col1.z, col2.z, col3.z, col4.z,
      col1.w, col2.w, col3.w, col4.w)
  
  override def toString: String = "R4x4"
}
