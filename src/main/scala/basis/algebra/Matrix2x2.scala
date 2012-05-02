/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Matrix2x2 extends Any with MatrixRing { self =>
  override type Matrix
  
  override type Span
  
  override type Scalar
  
  override def Row: Vector2.Space {
    type Vector = self.Span
    type Scalar = self.Scalar
  }
  
  override def Col: Vector2.Space {
    type Vector = self.Span
    type Scalar = self.Scalar
  }
  
  def _1_1: Scalar
  def _1_2: Scalar
  def _2_1: Scalar
  def _2_2: Scalar
  
  override def M: Int
  
  override def N: Int
  
  override def apply(k: Int): Scalar
  
  override def apply(i: Int, j: Int): Scalar
  
  override def row(i: Int): Row
  
  def row1: Row
  
  def row2: Row
  
  override def col(j: Int): Col
  
  def col1: Col
  
  def col2: Col
  
  override def + (that: Matrix): Matrix
  
  override def unary_- : Matrix
  
  override def - (that: Matrix): Matrix
  
  override def :* (scalar: Scalar): Matrix
  
  override def *: (scalar: Scalar): Matrix
  
  override def :⋅ (vector: Row): Col
  
  override def ⋅: (vector: Col): Row
  
  override def ⋅ [U <: basis.algebra.Vector { type Vector = U; type Scalar = self.Scalar }]
      (that: basis.algebra.Matrix { type Row = U; type Col = self.Row; type Scalar = self.Scalar })
    : basis.algebra.Matrix { type Row = U; type Col = self.Col; type Scalar = self.Scalar }
  
  override def T: T
  
  override def * (that: Matrix): Matrix
  
  override def inverse: Option[Matrix]
  
  override def det: Scalar
  
  override def trace: Scalar
}

object Matrix2x2 {
  trait Space extends MatrixRing.Space { self =>
    override type Matrix <: Matrix2x2 {
      type Matrix = self.Matrix
      type Span   = self.Span
      type Scalar = self.Scalar
    }
    
    override type Span <: Vector2 {
      type Vector = self.Span
      type Scalar = self.Scalar
    }
    
    override type Scalar
    
    override def M: Int = 2
    
    override def N: Int = 2
    
    override def apply(entries: TraversableOnce[Scalar]): Matrix = {
      val xs = entries.toSeq
      if (xs.length != 4) throw new DimensionException
      apply(xs(0), xs(1),  xs(2), xs(3))
    }
    
    def apply(
        _1_1: Scalar, _1_2: Scalar,
        _2_1: Scalar, _2_2: Scalar): Matrix
    
    override def rows(vectors: TraversableOnce[Row]): Matrix = {
      val vs = vectors.toSeq
      if (vs.length != 2) throw new DimensionException
      rows(vs(0), vs(1))
    }
    
    def rows(row1: Row, row2: Row): Matrix =
      apply(row1.x, row1.y,
            row2.x, row2.y)
    
    override def cols(vectors: TraversableOnce[Col]): Matrix = {
      val ws = vectors.toSeq
      if (ws.length != 2) throw new DimensionException
      cols(ws(0), ws(1))
    }
    
    def cols(col1: Col, col2: Col): Matrix =
      apply(col1.x, col2.x,
            col1.y, col2.y)
  }
  
  trait Template extends Any with MatrixRing.Template with Matrix2x2 { self =>
    override type Matrix <: Matrix2x2 {
      type Matrix = self.Matrix
      type Span   = self.Span
      type Scalar = self.Scalar
    }
    
    override type Span <: Vector2 {
      type Vector = self.Span
      type Scalar = self.Scalar
    }
    
    override type Scalar <: Field {
      type Vector = self.Scalar
    }
    
    override def Matrix: Matrix2x2.Space {
      type Matrix = self.Matrix
      type Span   = self.Span
      type Scalar = self.Scalar
    }
    
    override def Row: Vector2.Space {
      type Vector = self.Span
      type Scalar = self.Scalar
    }
    
    override def Col: Vector2.Space {
      type Vector = self.Span
      type Scalar = self.Scalar
    }
    
    override def _1_1: Scalar
    override def _1_2: Scalar
    override def _2_1: Scalar
    override def _2_2: Scalar
    
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
    
    override def row1: Row = Row(_1_1, _1_2)
    override def row2: Row = Row(_2_1, _2_2)
    
    override def col(j: Int): Col = j match {
      case 0 => col1
      case 1 => col2
      case _ => throw new IndexOutOfBoundsException("col "+ j)
    }
    
    override def col1: Col = Col(_1_1, _2_1)
    override def col2: Col = Col(_1_2, _2_2)
    
    override def + (that: Matrix): Matrix =
      Matrix(_1_1 + that._1_1, _1_2 + that._1_2,
             _2_1 + that._2_1, _2_2 + that._2_2)
    
    override def unary_- : Matrix =
      Matrix(-_1_1, -_1_2,
             -_2_1, -_2_2)
    
    override def - (that: Matrix): Matrix =
      Matrix(_1_1 - that._1_1, _1_2 - that._1_2,
             _2_1 - that._2_1, _2_2 - that._2_2)
    
    override def :* (scalar: Scalar): Matrix =
      Matrix(_1_1 * scalar, _1_2 * scalar,
             _2_1 * scalar, _2_2 * scalar)
    
    override def *: (scalar: Scalar): Matrix =
      Matrix(scalar * _1_1, scalar * _1_2,
             scalar * _2_1, scalar * _2_2)
    
    override def :⋅ (vector: Row): Col =
      Col(_1_1 * vector.x + _1_2 * vector.y,
          _2_1 * vector.x + _2_2 * vector.y)
    
    override def ⋅: (vector: Col): Row =
      Row(vector.x * _1_1 + vector.y * _2_1,
          vector.x * _1_2 + vector.y * _2_2)
    
    override def T: T =
      Matrix.T(_1_1, _2_1,
               _1_2, _2_2)
    
    override def * (that: Matrix): Matrix =
      Matrix(_1_1 * that._1_1 + _1_2 * that._2_1,
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
}
