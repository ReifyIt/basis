/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Matrix4x4 extends Any with MatrixRing { self =>
  override type Matrix
  
  override type Span
  
  override type Scalar
  
  override def Row: Vector4.Space {
    type Vector = self.Span
    type Scalar = self.Scalar
  }
  
  override def Col: Vector4.Space {
    type Vector = self.Span
    type Scalar = self.Scalar
  }
  
  def _1_1: Scalar
  def _1_2: Scalar
  def _1_3: Scalar
  def _1_4: Scalar
  def _2_1: Scalar
  def _2_2: Scalar
  def _2_3: Scalar
  def _2_4: Scalar
  def _3_1: Scalar
  def _3_2: Scalar
  def _3_3: Scalar
  def _3_4: Scalar
  def _4_1: Scalar
  def _4_2: Scalar
  def _4_3: Scalar
  def _4_4: Scalar
  
  override def M: Int
  
  override def N: Int
  
  override def apply(k: Int): Scalar
  
  override def apply(i: Int, j: Int): Scalar
  
  override def row(i: Int): Row
  
  def row1: Row
  
  def row2: Row
  
  def row3: Row
  
  def row4: Row
  
  override def col(j: Int): Col
  
  def col1: Col
  
  def col2: Col
  
  def col3: Col
  
  def col4: Col
  
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

object Matrix4x4 {
  trait Space extends MatrixRing.Space { self =>
    override type Matrix <: Matrix4x4 {
      type Matrix = self.Matrix
      type Span   = self.Span
      type Scalar = self.Scalar
    }
    
    override type Span <: Vector4 {
      type Vector = self.Span
      type Scalar = self.Scalar
    }
    
    override type Scalar
    
    override def M: Int = 4
    
    override def N: Int = 4
    
    override def apply(entries: TraversableOnce[Scalar]): Matrix = {
      val xs = entries.toSeq
      if (xs.length != 16) throw new DimensionException
      apply(xs( 0), xs( 1), xs( 2), xs( 3),
            xs( 4), xs( 5), xs( 6), xs( 7),
            xs( 8), xs( 9), xs(10), xs(11),
            xs(12), xs(13), xs(14), xs(15))
    }
    
    def apply(
        _1_1: Scalar, _1_2: Scalar, _1_3: Scalar, _1_4: Scalar,
        _2_1: Scalar, _2_2: Scalar, _2_3: Scalar, _2_4: Scalar,
        _3_1: Scalar, _3_2: Scalar, _3_3: Scalar, _3_4: Scalar,
        _4_1: Scalar, _4_2: Scalar, _4_3: Scalar, _4_4: Scalar): Matrix
    
    override def rows(vectors: TraversableOnce[Row]): Matrix = {
      val vs = vectors.toSeq
      if (vs.length != 4) throw new DimensionException
      rows(vs(0), vs(1), vs(2), vs(3))
    }
    
    def rows(row1: Row, row2: Row, row3: Row, row4: Row): Matrix =
      apply(row1.x, row1.y, row1.z, row1.w,
            row2.x, row2.y, row2.z, row2.w,
            row3.x, row3.y, row3.z, row3.w,
            row4.x, row4.y, row4.z, row4.w)
    
    override def cols(vectors: TraversableOnce[Col]): Matrix = {
      val ws = vectors.toSeq
      if (ws.length != 4) throw new DimensionException
      cols(ws(0), ws(1), ws(2), ws(3))
    }
    
    def cols(col1: Col, col2: Col, col3: Col, col4: Col): Matrix =
      apply(col1.x, col2.x, col3.x, col4.x,
            col1.y, col2.y, col3.y, col4.y,
            col1.z, col2.z, col3.z, col4.z,
            col1.w, col2.w, col3.w, col4.w)
  }
  
  trait Template extends Any with MatrixRing.Template with Matrix4x4 { self =>
    override type Matrix <: Matrix4x4 {
      type Matrix = self.Matrix
      type Span   = self.Span
      type Scalar = self.Scalar
    }
    
    override type Span <: Vector4 {
      type Vector = self.Span
      type Scalar = self.Scalar
    }
    
    override type Scalar <: Field {
      type Vector = self.Scalar
    }
    
    override def Matrix: Matrix4x4.Space {
      type Matrix = self.Matrix
      type Span   = self.Span
      type Scalar = self.Scalar
    }
    
    override def Row: Vector4.Space {
      type Vector = self.Span
      type Scalar = self.Scalar
    }
    
    override def Col: Vector4.Space {
      type Vector = self.Span
      type Scalar = self.Scalar
    }
    
    override def _1_1: Scalar
    override def _1_2: Scalar
    override def _1_3: Scalar
    override def _1_4: Scalar
    override def _2_1: Scalar
    override def _2_2: Scalar
    override def _2_3: Scalar
    override def _2_4: Scalar
    override def _3_1: Scalar
    override def _3_2: Scalar
    override def _3_3: Scalar
    override def _3_4: Scalar
    override def _4_1: Scalar
    override def _4_2: Scalar
    override def _4_3: Scalar
    override def _4_4: Scalar
    
    override def M: Int = 4
    override def N: Int = 4
    
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
      case _ => throw new IndexOutOfBoundsException(k.toString)
    }
    
    override def apply(i: Int, j: Int): Scalar = {
      if (i < 0 || i >= 4 || j < 0 || j >= 4)
        throw new IndexOutOfBoundsException("row "+ i +", "+"col "+ j)
      apply(4 * i + j)
    }
    
    override def row(i: Int): Row = i match {
      case 0 => row1
      case 1 => row2
      case 2 => row3
      case 3 => row4
      case _ => throw new IndexOutOfBoundsException("row "+ i)
    }
    
    override def row1: Row = Row(_1_1, _1_2, _1_3, _1_4)
    override def row2: Row = Row(_2_1, _2_2, _2_3, _2_4)
    override def row3: Row = Row(_3_1, _3_2, _3_3, _3_4)
    override def row4: Row = Row(_4_1, _4_2, _4_3, _4_4)
    
    override def col(j: Int): Col = j match {
      case 0 => col1
      case 1 => col2
      case 2 => col3
      case 3 => col4
      case _ => throw new IndexOutOfBoundsException("col "+ j)
    }
    
    override def col1: Col = Col(_1_1, _2_1, _3_1, _4_1)
    override def col2: Col = Col(_1_2, _2_2, _3_2, _4_2)
    override def col3: Col = Col(_1_3, _2_3, _3_3, _4_3)
    override def col4: Col = Col(_1_4, _2_4, _3_4, _4_4)
    
    override def + (that: Matrix): Matrix =
      Matrix(_1_1 + that._1_1, _1_2 + that._1_2, _1_3 + that._1_3, _1_4 + that._1_4,
             _2_1 + that._2_1, _2_2 + that._2_2, _2_3 + that._2_3, _2_4 + that._2_4,
             _3_1 + that._3_1, _3_2 + that._3_2, _3_3 + that._3_3, _3_4 + that._3_4,
             _4_1 + that._4_1, _4_2 + that._4_2, _4_3 + that._4_3, _4_4 + that._4_4)
    
    override def unary_- : Matrix =
      Matrix(-_1_1, -_1_2, -_1_3, -_1_4,
             -_2_1, -_2_2, -_2_3, -_2_4,
             -_3_1, -_3_2, -_3_3, -_3_4,
             -_4_1, -_4_2, -_4_3, -_4_4)
    
    override def - (that: Matrix): Matrix =
      Matrix(_1_1 - that._1_1, _1_2 - that._1_2, _1_3 - that._1_3, _1_4 - that._1_4,
             _2_1 - that._2_1, _2_2 - that._2_2, _2_3 - that._2_3, _2_4 - that._2_4,
             _3_1 - that._3_1, _3_2 - that._3_2, _3_3 - that._3_3, _3_4 - that._3_4,
             _4_1 - that._4_1, _4_2 - that._4_2, _4_3 - that._4_3, _4_4 - that._4_4)
    
    override def :* (scalar: Scalar): Matrix =
      Matrix(_1_1 * scalar, _1_2 * scalar, _1_3 * scalar, _1_4 * scalar,
             _2_1 * scalar, _2_2 * scalar, _2_3 * scalar, _2_4 * scalar,
             _3_1 * scalar, _3_2 * scalar, _3_3 * scalar, _3_4 * scalar,
             _4_1 * scalar, _4_2 * scalar, _4_3 * scalar, _4_4 * scalar)
    
    override def *: (scalar: Scalar): Matrix =
      Matrix(scalar * _1_1, scalar * _1_2, scalar * _1_3, scalar * _1_4,
             scalar * _2_1, scalar * _2_2, scalar * _2_3, scalar * _2_4,
             scalar * _3_1, scalar * _3_2, scalar * _3_3, scalar * _3_4,
             scalar * _4_1, scalar * _4_2, scalar * _4_3, scalar * _4_4)
    
    override def :⋅ (vector: Row): Col =
      Col(_1_1 * vector.x + _1_2 * vector.y + _1_3 * vector.z + _1_4 * vector.w,
          _2_1 * vector.x + _2_2 * vector.y + _2_3 * vector.z + _2_4 * vector.w,
          _3_1 * vector.x + _3_2 * vector.y + _3_3 * vector.z + _3_4 * vector.w,
          _4_1 * vector.x + _4_2 * vector.y + _4_3 * vector.z + _4_4 * vector.w)
    
    override def ⋅: (vector: Col): Row =
      Row(vector.x * _1_1 + vector.y * _2_1 + vector.z * _3_1 + vector.w * _4_1,
          vector.x * _1_2 + vector.y * _2_2 + vector.z * _3_2 + vector.w * _4_2,
          vector.x * _1_3 + vector.y * _2_3 + vector.z * _3_3 + vector.w * _4_3,
          vector.x * _1_4 + vector.y * _2_4 + vector.z * _3_4 + vector.w * _4_4)
    
    override def T: T =
      Matrix.T(_1_1, _2_1, _3_1, _4_1,
               _1_2, _2_2, _3_2, _4_2,
               _1_3, _2_3, _3_3, _4_3,
               _1_4, _2_4, _3_4, _4_4)
    
    override def * (that: Matrix): Matrix =
      Matrix(_1_1 * that._1_1 + _1_2 * that._2_1 + _1_3 * that._3_1 + _1_4 * that._4_1,
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
    
    override def inverse: Option[Matrix] = {
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
      Some(Matrix(
         minor_1_1 / det, -minor_2_1 / det,  minor_3_1 / det, -minor_4_1 / det,
        -minor_1_2 / det,  minor_2_2 / det, -minor_3_2 / det,  minor_4_2 / det,
         minor_1_3 / det, -minor_2_3 / det,  minor_3_3 / det, -minor_4_3 / det,
        -minor_1_4 / det,  minor_2_4 / det, -minor_3_4 / det,  minor_4_4 / det))
    }
    
    override def det: Scalar = {
      // 2x2 determinants minor_i1_i2__j1_j2 with
      // rows i1 and i2, and cols j1 and j2 blocked out
      val minor_1_2__1_2 = _3_3 * _4_4 - _3_4 * _4_3
      val minor_1_2__1_3 = _3_2 * _4_4 - _3_4 * _4_2
      val minor_1_2__1_4 = _3_2 * _4_3 - _3_3 * _4_2
      val minor_1_2__2_3 = _3_1 * _4_4 - _3_4 * _4_1
      val minor_1_2__2_4 = _3_1 * _4_3 - _3_3 * _4_1
      val minor_1_2__3_4 = _3_1 * _4_2 - _3_2 * _4_1
      
      // 3x3 determinants minor_i_j with
      // row i and col j blocked out
      val minor_1_1 = _2_2 * minor_1_2__1_2 - _2_3 * minor_1_2__1_3 + _2_4 * minor_1_2__1_4
      val minor_1_2 = _2_1 * minor_1_2__1_2 - _2_3 * minor_1_2__2_3 + _2_4 * minor_1_2__2_4
      val minor_1_3 = _2_1 * minor_1_2__1_3 - _2_2 * minor_1_2__2_3 + _2_4 * minor_1_2__3_4
      val minor_1_4 = _2_1 * minor_1_2__1_4 - _2_2 * minor_1_2__2_4 + _2_3 * minor_1_2__3_4
      
      _1_1 * minor_1_1 - _1_2 * minor_1_2 + _1_3 * minor_1_3 - _1_4 * minor_1_4
    }
    
    override def trace: Scalar = _1_1 + _2_2 + _3_3 + _4_4
  }
}
