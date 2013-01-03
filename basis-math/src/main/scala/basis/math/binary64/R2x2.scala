/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.math
package binary64

/** A 2 by 2 double-precision floating-point matrix space.
  * 
  * @author Chris Sachs
  * @since  0.0
  * @group  Real
  */
object R2x2 extends F2x2 with RMxN {
  final class Value(
      override val _1_1: Scalar, override val _1_2: Scalar,
      override val _2_1: Scalar, override val _2_2: Scalar)
    extends super[F2x2].Value
       with super[RMxN].Value {
    
    override def Row: R2.type = R2
    override def Col: R2.type = R2
    
    override def apply(k: Int): Scalar = k match {
      case 0 => _1_1
      case 1 => _1_2
      case 2 => _2_1
      case 3 => _2_2
      case _ => throw new java.lang.IndexOutOfBoundsException(k.toString)
    }
    
    override def apply(i: Int, j: Int): Scalar = {
      if (i < 0 || i >= 2 || j < 0 || j >= 2)
        throw new java.lang.IndexOutOfBoundsException("row "+ i +", "+"col "+ j)
      apply(2 * i + j)
    }
    
    override def row1: Row = new Row(_1_1, _1_2)
    override def row2: Row = new Row(_2_1, _2_2)
    
    override def row(i: Int): Row = i match {
      case 0 => row1
      case 1 => row2
      case _ => throw new java.lang.IndexOutOfBoundsException("row "+ i)
    }
    
    override def col1: Col = new Col(_1_1, _2_1)
    override def col2: Col = new Col(_1_2, _2_2)
    
    override def col(j: Int): Col = j match {
      case 0 => col1
      case 1 => col2
      case _ => throw new java.lang.IndexOutOfBoundsException("col "+ j)
    }
    
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
    
    override def :* (scalar: Scalar): Matrix =
      new Matrix(
        _1_1 * scalar, _1_2 * scalar,
        _2_1 * scalar, _2_2 * scalar)
    
    override def *: (scalar: Scalar): Matrix = this :* scalar
    
    override def :⋅ (vector: Row): Col =
      new Col(
        _1_1 * vector.x + _1_2 * vector.y,
        _2_1 * vector.x + _2_2 * vector.y)
    
    override def ⋅: (vector: Col): Row =
      new Row(
        vector.x * _1_1 + vector.y * _2_1,
        vector.x * _1_2 + vector.y * _2_2)
    
    override def * (that: Matrix): Matrix =
      new Matrix(
        _1_1 * that._1_1 + _1_2 * that._2_1,
        _1_1 * that._1_2 + _1_2 * that._2_2,
        _2_1 * that._1_1 + _2_2 * that._2_1,
        _2_1 * that._1_2 + _2_2 * that._2_2)
    
    override def inverse: Matrix = {
      val det = this.det
      new Matrix(
         _2_2 / det, -_1_2 / det,
        -_2_1 / det,  _1_1 / det)
    }
    
    override def transpose: Matrix =
      new Matrix(
        _1_1, _2_1,
        _1_2, _2_2)
    
    override def det: Scalar =
      _1_1 * _2_2 - _1_2 * _2_1
    
    override def trace: Scalar =
      _1_1 + _2_2
  }
  
  override type Matrix = Value
  
  override val Transpose: this.type = this
  
  override val Row: R2.type = R2
  override val Col: R2.type = R2
  
  override val Scalar: Real.type = Real
  
  override def dim: Int = 4
  
  override val zero: Matrix = {
    val z = Scalar.zero
    new Matrix(z, z,  z, z)
  }
  
  override val unit: Matrix = {
    val z = Scalar.zero
    val u = Scalar.unit
    new Matrix(u, z,  z, u)
  }
  
  override def apply(
      _1_1: Scalar, _1_2: Scalar,
      _2_1: Scalar, _2_2: Scalar): Matrix =
    new Matrix(
      _1_1, _1_2,
      _2_1, _2_2)
  
  override def apply(entries: Array[Double]): Matrix = {
    if (entries.length != 4) throw new DimensionException
    new Matrix(
      new Scalar(entries(0)), new Scalar(entries(1)),
      new Scalar(entries(2)), new Scalar(entries(3)))
  }
  
  override def rows(row1: Row, row2: Row): Matrix =
    new Matrix(
      row1.x, row1.y,
      row2.x, row2.y)
  
  override def cols(col1: Col, col2: Col): Matrix =
    new Matrix(
      col1.x, col2.x,
      col1.y, col2.y)
  
  override def toString: String = "R2x2"
}
