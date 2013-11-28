//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.math

/** An asbtract 2 by 2 matrix space over a field.
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    MatrixSpaces
  */
trait F2x2 extends Ring with FMxN { F2x2 =>
  /** The type of elements in this $space; equivalent to the type of matrices. */
  override type Element = Matrix

  override type Matrix <: MatrixF2x2

  override val Transpose: F2x2 {
    val Row: F2x2.Col.type
    val Col: F2x2.Row.type
    val Scalar: F2x2.Scalar.type
  }

  override val Row: F2 { val Scalar: F2x2.Scalar.type }
  override val Col: F2 { val Scalar: F2x2.Scalar.type }

  override val Scalar: Field

  override def dim: Int = 4

  override def zero: Matrix = {
    val z = Scalar.zero
    apply(z, z,  z, z)
  }

  override def unit: Matrix = {
    val z = Scalar.zero
    val u = Scalar.unit
    apply(u, z,  z, u)
  }

  def apply(
      _1_1: Scalar, _1_2: Scalar,
      _2_1: Scalar, _2_2: Scalar): Matrix

  override def apply(entries: Array[Scalar]): Matrix = {
    if (entries.length != 4) throw new DimensionException
    apply(entries(0), entries(1),  entries(2), entries(3))
  }

  def rows(row1: Row, row2: Row): Matrix =
    apply(row1.x, row1.y,
          row2.x, row2.y)

  override def rows(rows: Row*): Matrix = {
    if (rows.length != 2) throw new DimensionException
    this.rows(rows(0), rows(1))
  }

  def cols(col1: Col, col2: Col): Matrix =
    apply(col1.x, col2.x,
          col1.y, col2.y)

  override def cols(cols: Col*): Matrix = {
    if (cols.length != 2) throw new DimensionException
    this.cols(cols(0), cols(1))
  }

  trait MatrixF2x2 extends Any with RingElement with MatrixFMxN {
    override def Row: F2x2.Row.type = F2x2.Row
    override def Col: F2x2.Col.type = F2x2.Col

    def _1_1: Scalar
    def _1_2: Scalar
    def _2_1: Scalar
    def _2_2: Scalar

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

    def row1: Row = Row(_1_1, _1_2)
    def row2: Row = Row(_2_1, _2_2)

    override def row(i: Int): Row = i match {
      case 0 => row1
      case 1 => row2
      case _ => throw new IndexOutOfBoundsException("row "+ i)
    }

    def col1: Col = Col(_1_1, _2_1)
    def col2: Col = Col(_1_2, _2_2)

    override def col(j: Int): Col = j match {
      case 0 => col1
      case 1 => col2
      case _ => throw new IndexOutOfBoundsException("col "+ j)
    }

    override def + (that: Matrix): Matrix =
      F2x2(_1_1 + that._1_1, _1_2 + that._1_2,
           _2_1 + that._2_1, _2_2 + that._2_2)

    override def unary_- : Matrix =
      F2x2(-_1_1, -_1_2,
           -_2_1, -_2_2)

    override def - (that: Matrix): Matrix =
      F2x2(_1_1 - that._1_1, _1_2 - that._1_2,
           _2_1 - that._2_1, _2_2 - that._2_2)

    override def :* (scalar: Scalar): Matrix =
      F2x2(_1_1 * scalar, _1_2 * scalar,
           _2_1 * scalar, _2_2 * scalar)

    override def *: (scalar: Scalar): Matrix =
      F2x2(scalar * _1_1, scalar * _1_2,
           scalar * _2_1, scalar * _2_2)

    override def ∘ (that: Matrix): Matrix =
      F2x2(_1_1 * that._1_1, _1_2 * that._1_2,
           _2_1 * that._2_1, _2_2 * that._2_2)

    override def :⋅ (vector: Row): Col =
      Col(_1_1 * vector.x + _1_2 * vector.y,
          _2_1 * vector.x + _2_2 * vector.y)

    override def ⋅: (vector: Col): Row =
      Row(vector.x * _1_1 + vector.y * _2_1,
          vector.x * _1_2 + vector.y * _2_2)

    override def * (that: Matrix): Matrix =
      F2x2(_1_1 * that._1_1 + _1_2 * that._2_1,
           _1_1 * that._1_2 + _1_2 * that._2_2,
           _2_1 * that._1_1 + _2_2 * that._2_1,
           _2_1 * that._1_2 + _2_2 * that._2_2)

    override def inverse: Matrix = {
      val det = this.det
      F2x2( _2_2 / det, -_1_2 / det,
           -_2_1 / det,  _1_1 / det)
    }

    override def transpose: Transpose =
      Transpose(
        _1_1, _2_1,
        _1_2, _2_2)

    override def det: Scalar =
      _1_1 * _2_2 - _1_2 * _2_1

    override def trace: Scalar =
      _1_1 + _2_2
  }
}
