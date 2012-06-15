/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Matrix2x2Space
    [V <: Vector2Space[S] with Singleton,
     W <: Vector2Space[S] with Singleton,
     S <: Field with Singleton]
  extends MatrixRing[V, W, S] {
  
  trait Element extends Any with super.Element { this: Matrix =>
    override protected def Matrix: Matrix2x2Space.this.type = Matrix2x2Space.this
    
    def _1_1: Scalar
    def _1_2: Scalar
    def _2_1: Scalar
    def _2_2: Scalar
    
    override def M: Int = 2
    override def N: Int = 2
    
    override def apply(k: Int): Scalar = k match {
      case 0 => _1_1
      case 1 => _1_2
      case 2 => _2_1
      case 3 => _2_2
      case _ => throw new IndexOutOfBoundsException(k.toString)
    }
    
    override def apply(i: Int, j: Int): Scalar = {
      if (i < 0 || i >= 2 || j < 0 || j >= 2)
        throw new IndexOutOfBoundsException("row "+ i +", "+"col "+ j)
      apply(2 * i + j)
    }
    
    override def row(i: Int): Row = i match {
      case 0 => row1
      case 1 => row2
      case _ => throw new IndexOutOfBoundsException("row "+ i)
    }
    
    def row1: Row = Row(_1_1, _1_2)
    def row2: Row = Row(_2_1, _2_2)
    
    override def col(j: Int): Col = j match {
      case 0 => col1
      case 1 => col2
      case _ => throw new IndexOutOfBoundsException("col "+ j)
    }
    
    def col1: Col = Col(_1_1, _2_1)
    def col2: Col = Col(_1_2, _2_2)
    
    override def + (that: Matrix): Matrix =
      Matrix(
        _1_1 + that._1_1, _1_2 + that._1_2,
        _2_1 + that._2_1, _2_2 + that._2_2)
    
    override def unary_- : Matrix =
      Matrix(
        -_1_1, -_1_2,
        -_2_1, -_2_2)
    
    override def - (that: Matrix): Matrix =
      Matrix(
        _1_1 - that._1_1, _1_2 - that._1_2,
        _2_1 - that._2_1, _2_2 - that._2_2)
    
    override def :* (scalar: Scalar): Matrix =
      Matrix(
        _1_1 * scalar, _1_2 * scalar,
        _2_1 * scalar, _2_2 * scalar)
    
    override def *: (scalar: Scalar): Matrix =
      Matrix(
        scalar * _1_1, scalar * _1_2,
        scalar * _2_1, scalar * _2_2)
    
    override def :⋅ (vector: Row): Col =
      Col(_1_1 * vector.x + _1_2 * vector.y,
          _2_1 * vector.x + _2_2 * vector.y)
    
    override def ⋅: (vector: Col): Row =
      Row(vector.x * _1_1 + vector.y * _2_1,
          vector.x * _1_2 + vector.y * _2_2)
    
    override def T: Transpose.Matrix =
      Transpose(
        _1_1, _2_1,
        _1_2, _2_2)
    
    override def * (that: Matrix): Matrix =
      Matrix(
        _1_1 * that._1_1 + _1_2 * that._2_1,
        _1_1 * that._1_2 + _1_2 * that._2_2,
        _2_1 * that._1_1 + _2_2 * that._2_1,
        _2_1 * that._1_2 + _2_2 * that._2_2)
    
    override def inverse: Option[Matrix] = {
      val det = this.det
      Some(Matrix(
         _2_2 / det, -_1_2 / det,
        -_2_1 / det,  _1_1 / det))
    }
    
    override def det: Scalar = _1_1 * _2_2 - _1_2 * _2_1
    
    override def trace: Scalar = _1_1 + _2_2
  }
  
  override type Matrix <: Element
  
  override val Transpose: Matrix2x2Space[W, V, S]
  
  override def Row: V
  override def Col: W
  override def Scalar: S
  
  override def M: Int = 2
  override def N: Int = 2
  
  override def apply(entries: Scalar*): Matrix = {
    if (entries.length != 4) throw new DimensionException
    apply(entries(0), entries(1),  entries(2), entries(3))
  }
  
  def apply(
      _1_1: Scalar, _1_2: Scalar,
      _2_1: Scalar, _2_2: Scalar): Matrix
  
  override def rows(rows: Row*): Matrix = {
    if (rows.length != 2) throw new DimensionException
    this.rows(rows(0), rows(1))
  }
  
  def rows(row1: Row, row2: Row): Matrix =
    apply(row1.x, row1.y,
          row2.x, row2.y)
  
  override def cols(cols: Col*): Matrix = {
    if (cols.length != 2) throw new DimensionException
    this.cols(cols(0), cols(1))
  }
  
  def cols(col1: Col, col2: Col): Matrix =
    apply(col1.x, col2.x,
          col1.y, col2.y)
  
  override def zero: Matrix = {
    val z = Scalar.zero
    apply(z, z,  z, z)
  }
  
  override def unit: Matrix = {
    val z = Scalar.zero
    val u = Scalar.unit
    apply(u, z,  z, u)
  }
}
