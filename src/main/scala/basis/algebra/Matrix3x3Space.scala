/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Matrix3x3Space
    [V <: Vector3Space[S] with Singleton,
     W <: Vector3Space[S] with Singleton,
     S <: Field with Singleton]
  extends MatrixRing[V, W, S] {
  
  trait Element extends Any with super.Element { this: Matrix =>
    override protected def Matrix: Matrix3x3Space.this.type = Matrix3x3Space.this
    
    def _1_1: Scalar
    def _1_2: Scalar
    def _1_3: Scalar
    def _2_1: Scalar
    def _2_2: Scalar
    def _2_3: Scalar
    def _3_1: Scalar
    def _3_2: Scalar
    def _3_3: Scalar
    
    override def M: Int = 3
    override def N: Int = 3
    
    override def apply(k: Int): Scalar = k match {
      case 0 => _1_1
      case 1 => _1_2
      case 2 => _1_3
      case 3 => _2_1
      case 4 => _2_2
      case 5 => _2_3
      case 6 => _3_1
      case 7 => _3_2
      case 8 => _3_3
      case _ => throw new IndexOutOfBoundsException(k.toString)
    }
    
    override def apply(i: Int, j: Int): Scalar = {
      if (i < 0 || i >= 3 || j < 0 || j >= 3)
        throw new IndexOutOfBoundsException("row "+ i +", "+"col "+ j)
      apply(3 * i + j)
    }
    
    override def row(i: Int): Row = i match {
      case 0 => row1
      case 1 => row2
      case 2 => row3
      case _ => throw new IndexOutOfBoundsException("row "+ i)
    }
    
    def row1: Row = Row(_1_1, _1_2, _1_3)
    def row2: Row = Row(_2_1, _2_2, _2_3)
    def row3: Row = Row(_3_1, _3_2, _3_3)
    
    override def col(j: Int): Col = j match {
      case 0 => col1
      case 1 => col2
      case 2 => col3
      case _ => throw new IndexOutOfBoundsException("col "+ j)
    }
    
    def col1: Col = Col(_1_1, _2_1, _3_1)
    def col2: Col = Col(_1_2, _2_2, _3_2)
    def col3: Col = Col(_1_3, _2_3, _3_3)
    
    override def + (that: Matrix): Matrix =
      Matrix(
        _1_1 + that._1_1, _1_2 + that._1_2, _1_3 + that._1_3,
        _2_1 + that._2_1, _2_2 + that._2_2, _2_3 + that._2_3,
        _3_1 + that._3_1, _3_2 + that._3_2, _3_3 + that._3_3)
    
    override def unary_- : Matrix =
      Matrix(
        -_1_1, -_1_2, -_1_3,
        -_2_1, -_2_2, -_2_3,
        -_3_1, -_3_2, -_3_3)
    
    override def - (that: Matrix): Matrix =
      Matrix(
        _1_1 - that._1_1, _1_2 - that._1_2, _1_3 - that._1_3,
        _2_1 - that._2_1, _2_2 - that._2_2, _2_3 - that._2_3,
        _3_1 - that._3_1, _3_2 - that._3_2, _3_3 - that._3_3)
    
    override def :* (scalar: Scalar): Matrix =
      Matrix(
        _1_1 * scalar, _1_2 * scalar, _1_3 * scalar,
        _2_1 * scalar, _2_2 * scalar, _2_3 * scalar,
        _3_1 * scalar, _3_2 * scalar, _3_3 * scalar)
    
    override def *: (scalar: Scalar): Matrix =
      Matrix(
        scalar * _1_1, scalar * _1_2, scalar * _1_3,
        scalar * _2_1, scalar * _2_2, scalar * _2_3,
        scalar * _3_1, scalar * _3_2, scalar * _3_3)
    
    override def :⋅ (vector: Row): Col =
      Col(_1_1 * vector.x + _1_2 * vector.y + _1_3 * vector.z,
          _2_1 * vector.x + _2_2 * vector.y + _2_3 * vector.z,
          _3_1 * vector.x + _3_2 * vector.y + _3_3 * vector.z)
    
    override def ⋅: (vector: Col): Row =
      Row(vector.x * _1_1 + vector.y * _2_1 + vector.z * _3_1,
          vector.x * _1_2 + vector.y * _2_2 + vector.z * _3_2,
          vector.x * _1_3 + vector.y * _2_3 + vector.z * _3_3)
    
    override def T: Transpose.Matrix =
      Transpose(
        _1_1, _2_1, _3_1,
        _1_2, _2_2, _3_2,
        _1_3, _2_3, _3_3)
    
    override def * (that: Matrix): Matrix =
      Matrix(
        _1_1 * that._1_1 + _1_2 * that._2_1 + _1_3 * that._3_1,
        _1_1 * that._1_2 + _1_2 * that._2_2 + _1_3 * that._3_2,
        _1_1 * that._1_3 + _1_2 * that._2_3 + _1_3 * that._3_3,
        _2_1 * that._1_1 + _2_2 * that._2_1 + _2_3 * that._3_1,
        _2_1 * that._1_2 + _2_2 * that._2_2 + _2_3 * that._3_2,
        _2_1 * that._1_3 + _2_2 * that._2_3 + _2_3 * that._3_3,
        _3_1 * that._1_1 + _3_2 * that._2_1 + _3_3 * that._3_1,
        _3_1 * that._1_2 + _3_2 * that._2_2 + _3_3 * that._3_2,
        _3_1 * that._1_3 + _3_2 * that._2_3 + _3_3 * that._3_3)
    
    override def inverse: Option[Matrix] = {
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
      Some(Matrix(
         minor_1_1 / det, -minor_2_1 / det,  minor_3_1 / det,
        -minor_1_2 / det,  minor_2_2 / det, -minor_3_2 / det,
         minor_1_3 / det, -minor_2_3 / det,  minor_3_3 / det))
    }
    
    override def det: Scalar = {
      val minor_1_1 = _2_2 * _3_3 - _2_3 * _3_2
      val minor_1_2 = _2_1 * _3_3 - _2_3 * _3_1
      val minor_1_3 = _2_1 * _3_2 - _2_2 * _3_1
      
      _1_1 * minor_1_1 - _1_2 * minor_1_2 + _1_3 * minor_1_3
    }
    
    override def trace: Scalar = _1_1 + _2_2 + _3_3
  }
  
  override type Matrix <: Element
  
  override val Transpose: Matrix3x3Space[W, V, S]
  
  override def Row: V
  override def Col: W
  override def Scalar: S
  
  override def M: Int = 3
  override def N: Int = 3
  
  override def apply(entries: TraversableOnce[Scalar]): Matrix = {
    val xs = entries.toSeq
    if (xs.length != 9) throw new DimensionException
    apply(xs(0), xs(1), xs(2),
          xs(3), xs(4), xs(5),
          xs(6), xs(7), xs(8))
  }
  
  def apply(
      _1_1: Scalar, _1_2: Scalar, _1_3: Scalar,
      _2_1: Scalar, _2_2: Scalar, _2_3: Scalar,
      _3_1: Scalar, _3_2: Scalar, _3_3: Scalar): Matrix
  
  override def rows(vectors: TraversableOnce[Row]): Matrix = {
    val vs = vectors.toSeq
    if (vs.length != 3) throw new DimensionException
    rows(vs(0), vs(1), vs(2))
  }
  
  def rows(row1: Row, row2: Row, row3: Row): Matrix =
    apply(row1.x, row1.y, row1.z,
          row2.x, row2.y, row2.z,
          row3.x, row3.y, row3.z)
  
  override def cols(vectors: TraversableOnce[Col]): Matrix = {
    val ws = vectors.toSeq
    if (ws.length != 3) throw new DimensionException
    cols(ws(0), ws(1), ws(2))
  }
  
  def cols(col1: Col, col2: Col, col3: Col): Matrix =
    apply(col1.x, col2.x, col3.x,
          col1.y, col2.y, col3.y,
          col1.z, col2.z, col3.z)
  
  override def zero: Matrix = {
    val z = Scalar.zero
    apply(z, z, z,  z, z, z,  z, z, z)
  }
  
  override def unit: Matrix = {
    val z = Scalar.zero
    val u = Scalar.unit
    apply(u, z, z,  z, u, z,  z, z, u)
  }
}
