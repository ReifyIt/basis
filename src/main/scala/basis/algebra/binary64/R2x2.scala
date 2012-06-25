/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

/** A 2 by 2 real matrix space.
  * 
  * @author Chris Sachs
  */
object R2x2 extends F2x2[R2.type, Real.type] with RMxN[R2.type, R2.type] {
  final class Element(
      val _1_1: Real, val _1_2: Real,
      val _2_1: Real, val _2_2: Real)
    extends super[F2x2].Element with super[RMxN].Element {
    
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
    
    override def row1: Row = new Row(_1_1, _1_2)
    override def row2: Row = new Row(_2_1, _2_2)
    
    override def row(i: Int): Row = i match {
      case 0 => row1
      case 1 => row2
      case _ => throw new IndexOutOfBoundsException("row "+ i)
    }
    
    override def col1: Col = new Col(_1_1, _2_1)
    override def col2: Col = new Col(_1_2, _2_2)
    
    override def col(j: Int): Col = j match {
      case 0 => col1
      case 1 => col2
      case _ => throw new IndexOutOfBoundsException("col "+ j)
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
    
    override def :* (scalar: Real): Matrix =
      new Matrix(
        _1_1 * scalar, _1_2 * scalar,
        _2_1 * scalar, _2_2 * scalar)
    
    override def *: (scalar: Real): Matrix = this :* scalar
    
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
    
    override def inverse(implicit isSquare: R2.type =:= R2.type): Option[Matrix] = {
      val det = this.det
      if (det.abs >= Double.MinPositiveValue)
        Some(new Matrix(
           _2_2 / det, -_1_2 / det,
          -_2_1 / det,  _1_1 / det))
      else None
    }
    
    override def T: Matrix =
      new Matrix(
        _1_1, _2_1,
        _1_2, _2_2)
    
    override def det(implicit isSquare: R2.type =:= R2.type): Real =
      _1_1 * _2_2 - _1_2 * _2_1
    
    override def trace(implicit isSquare: R2.type =:= R2.type): Real = _1_1 + _2_2
  }
  
  override type Matrix = Element
  
  override def Row: R2.type = R2
  override def Col: R2.type = R2
  
  override val zero: Matrix =
    new Matrix(0.0, 0.0,  0.0, 0.0)
  
  override val unit: Matrix =
    new Matrix(1.0, 0.0,  0.0, 1.0)
  
  override def identity(implicit isSquare: R2.type =:= R2.type): Matrix = unit
  
  override def apply(
      _1_1: Real, _1_2: Real,
      _2_1: Real, _2_2: Real): Matrix =
    new Matrix(
      _1_1, _1_2,
      _2_1, _2_2)
  
  override def apply(entries: Array[Double]): Matrix = {
    if (entries.length != 4) throw new DimensionException
    new Matrix(
      entries(0), entries(1),
      entries(2), entries(3))
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
