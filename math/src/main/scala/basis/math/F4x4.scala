//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.math

/** An asbtract 4 by 4 matrix space over a field.
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    MatrixSpaces
  */
trait F4x4 extends Ring with FMxN { F4x4 =>
  /** The type of elements in this $space; equivalent to the type of matrices. */
  override type Element = Matrix

  override type Matrix <: MatrixF4x4

  override val Transpose: F4x4 {
    val Row: F4x4.Col.type
    val Col: F4x4.Row.type
    val Scalar: F4x4.Scalar.type
  }

  override val Row: F4 { val Scalar: F4x4.Scalar.type }
  override val Col: F4 { val Scalar: F4x4.Scalar.type }

  override val Scalar: Field

  override def dim: Int = 16

  override def zero: Matrix = {
    val z = Scalar.zero
    apply(z, z, z, z,  z, z, z, z,  z, z, z, z,  z, z, z, z)
  }

  override def unit: Matrix = {
    val z = Scalar.zero
    val u = Scalar.unit
    apply(u, z, z, z,  z, u, z, z,  z, z, u, z,  z, z, z, u)
  }

  def apply(
      _1_1: Scalar, _1_2: Scalar, _1_3: Scalar, _1_4: Scalar,
      _2_1: Scalar, _2_2: Scalar, _2_3: Scalar, _2_4: Scalar,
      _3_1: Scalar, _3_2: Scalar, _3_3: Scalar, _3_4: Scalar,
      _4_1: Scalar, _4_2: Scalar, _4_3: Scalar, _4_4: Scalar): Matrix

  override def apply(entries: Array[Scalar]): Matrix = {
    if (entries.length != 16) throw new DimensionException
    apply(entries( 0), entries( 1), entries( 2), entries( 3),
          entries( 4), entries( 5), entries( 6), entries( 7),
          entries( 8), entries( 9), entries(10), entries(11),
          entries(12), entries(13), entries(14), entries(15))
  }

  def rows(row1: Row, row2: Row, row3: Row, row4: Row): Matrix =
    apply(row1.x, row1.y, row1.z, row1.w,
          row2.x, row2.y, row2.z, row2.w,
          row3.x, row3.y, row3.z, row3.w,
          row4.x, row4.y, row4.z, row4.w)

  override def rows(rows: Row*): Matrix = {
    if (rows.length != 4) throw new DimensionException
    this.rows(rows(0), rows(1), rows(2), rows(3))
  }

  def cols(col1: Col, col2: Col, col3: Col, col4: Col): Matrix =
    apply(col1.x, col2.x, col3.x, col4.x,
          col1.y, col2.y, col3.y, col4.y,
          col1.z, col2.z, col3.z, col4.z,
          col1.w, col2.w, col3.w, col4.w)

  override def cols(cols: Col*): Matrix = {
    if (cols.length != 4) throw new DimensionException
    this.cols(cols(0), cols(1), cols(2), cols(3))
  }

  trait MatrixF4x4 extends Any with RingElement with MatrixFMxN {
    override def Row: F4x4.Row.type = F4x4.Row
    override def Col: F4x4.Col.type = F4x4.Col

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

    def row1: Row = Row(_1_1, _1_2, _1_3, _1_4)
    def row2: Row = Row(_2_1, _2_2, _2_3, _2_4)
    def row3: Row = Row(_3_1, _3_2, _3_3, _3_4)
    def row4: Row = Row(_4_1, _4_2, _4_3, _4_4)

    override def row(i: Int): Row = i match {
      case 0 => row1
      case 1 => row2
      case 2 => row3
      case 3 => row4
      case _ => throw new IndexOutOfBoundsException("row "+ i)
    }

    def col1: Col = Col(_1_1, _2_1, _3_1, _4_1)
    def col2: Col = Col(_1_2, _2_2, _3_2, _4_2)
    def col3: Col = Col(_1_3, _2_3, _3_3, _4_3)
    def col4: Col = Col(_1_4, _2_4, _3_4, _4_4)

    override def col(j: Int): Col = j match {
      case 0 => col1
      case 1 => col2
      case 2 => col3
      case 3 => col4
      case _ => throw new IndexOutOfBoundsException("col "+ j)
    }

    override def + (that: Matrix): Matrix =
      F4x4(_1_1 + that._1_1, _1_2 + that._1_2, _1_3 + that._1_3, _1_4 + that._1_4,
           _2_1 + that._2_1, _2_2 + that._2_2, _2_3 + that._2_3, _2_4 + that._2_4,
           _3_1 + that._3_1, _3_2 + that._3_2, _3_3 + that._3_3, _3_4 + that._3_4,
           _4_1 + that._4_1, _4_2 + that._4_2, _4_3 + that._4_3, _4_4 + that._4_4)

    override def unary_- : Matrix =
      F4x4(-_1_1, -_1_2, -_1_3, -_1_4,
           -_2_1, -_2_2, -_2_3, -_2_4,
           -_3_1, -_3_2, -_3_3, -_3_4,
           -_4_1, -_4_2, -_4_3, -_4_4)

    override def - (that: Matrix): Matrix =
      F4x4(_1_1 - that._1_1, _1_2 - that._1_2, _1_3 - that._1_3, _1_4 - that._1_4,
           _2_1 - that._2_1, _2_2 - that._2_2, _2_3 - that._2_3, _2_4 - that._2_4,
           _3_1 - that._3_1, _3_2 - that._3_2, _3_3 - that._3_3, _3_4 - that._3_4,
           _4_1 - that._4_1, _4_2 - that._4_2, _4_3 - that._4_3, _4_4 - that._4_4)

    override def :* (scalar: Scalar): Matrix =
      F4x4(_1_1 * scalar, _1_2 * scalar, _1_3 * scalar, _1_4 * scalar,
           _2_1 * scalar, _2_2 * scalar, _2_3 * scalar, _2_4 * scalar,
           _3_1 * scalar, _3_2 * scalar, _3_3 * scalar, _3_4 * scalar,
           _4_1 * scalar, _4_2 * scalar, _4_3 * scalar, _4_4 * scalar)

    override def *: (scalar: Scalar): Matrix =
      F4x4(scalar * _1_1, scalar * _1_2, scalar * _1_3, scalar * _1_4,
           scalar * _2_1, scalar * _2_2, scalar * _2_3, scalar * _2_4,
           scalar * _3_1, scalar * _3_2, scalar * _3_3, scalar * _3_4,
           scalar * _4_1, scalar * _4_2, scalar * _4_3, scalar * _4_4)

    override def ∘ (that: Matrix): Matrix =
      F4x4(_1_1 * that._1_1, _1_2 * that._1_2, _1_3 * that._1_3, _1_4 * that._1_4,
           _2_1 * that._2_1, _2_2 * that._2_2, _2_3 * that._2_3, _2_4 * that._2_4,
           _3_1 * that._3_1, _3_2 * that._3_2, _3_3 * that._3_3, _3_4 * that._3_4,
           _4_1 * that._4_1, _4_2 * that._4_2, _4_3 * that._4_3, _4_4 * that._4_4)

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

    override def * (that: Matrix): Matrix =
      F4x4(_1_1 * that._1_1 + _1_2 * that._2_1 + _1_3 * that._3_1 + _1_4 * that._4_1,
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
      F4x4( minor_1_1 / det, -minor_2_1 / det,  minor_3_1 / det, -minor_4_1 / det,
           -minor_1_2 / det,  minor_2_2 / det, -minor_3_2 / det,  minor_4_2 / det,
            minor_1_3 / det, -minor_2_3 / det,  minor_3_3 / det, -minor_4_3 / det,
           -minor_1_4 / det,  minor_2_4 / det, -minor_3_4 / det,  minor_4_4 / det)
    }

    override def transpose: Transpose =
      Transpose(
        _1_1, _2_1, _3_1, _4_1,
        _1_2, _2_2, _3_2, _4_2,
        _1_3, _2_3, _3_3, _4_3,
        _1_4, _2_4, _3_4, _4_4)

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

    override def trace: Scalar =
      _1_1 + _2_2 + _3_3 + _4_4
  }
}
