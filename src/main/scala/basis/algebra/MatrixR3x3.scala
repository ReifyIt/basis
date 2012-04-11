/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait MatrixR3x3[M <: MatrixR3x3[M, V], V <: VectorR3[V]]
  extends MatrixF3x3[M, V, Real] with RealMatrix[M, M, V, V] {
  
  def Space: R3x3 {
    type Matrix = M
    type RowVector = V
  }
  
  def _1_1: Double
  def _1_2: Double
  def _1_3: Double
  def _2_1: Double
  def _2_2: Double
  def _2_3: Double
  def _3_1: Double
  def _3_2: Double
  def _3_3: Double
  
  def apply(k: Int): Double = k match {
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
  
  override def column1: V = Space.Column(_1_1, _2_1, _3_1)
  
  override def column2: V = Space.Column(_1_2, _2_2, _3_2)
  
  override def column3: V = Space.Column(_1_3, _2_3, _3_3)
  
  override def row1: V = Space.Row(_1_1, _1_2, _1_3)
  
  override def row2: V = Space.Row(_2_1, _2_2, _2_3)
  
  override def row3: V = Space.Row(_3_1, _3_2, _3_3)
  
  override def + (that: M): M =
    Space(_1_1 + that._1_1, _1_2 + that._1_2, _1_3 + that._1_3,
          _2_1 + that._2_1, _2_2 + that._2_2, _2_3 + that._2_3,
          _3_1 + that._3_1, _3_2 + that._3_2, _3_3 + that._3_3)
  
  override def unary_- : M =
    Space(-_1_1, -_1_2, -_1_3,
          -_2_1, -_2_2, -_2_3,
          -_3_1, -_3_2, -_3_3)
  
  override def - (that: M): M =
    Space(_1_1 - that._1_1, _1_2 - that._1_2, _1_3 - that._1_3,
          _2_1 - that._2_1, _2_2 - that._2_2, _2_3 - that._2_3,
          _3_1 - that._3_1, _3_2 - that._3_2, _3_3 - that._3_3)
  
  override def :* (scalar: Double): M =
    Space(_1_1 * scalar, _1_2 * scalar, _1_3 * scalar,
          _2_1 * scalar, _2_2 * scalar, _2_3 * scalar,
          _3_1 * scalar, _3_2 * scalar, _3_3 * scalar)
  
  override def *: (scalar: Double): M = this :* scalar
  
  override def :* (vector: V): V =
    Space.Column(_1_1 * vector(0) + _1_2 * vector(1) + _1_3 * vector(2),
                 _2_1 * vector(0) + _2_2 * vector(1) + _2_3 * vector(2),
                 _3_1 * vector(0) + _3_2 * vector(1) + _3_3 * vector(2))
  
  override def *: (vector: V): V =
    Space.Row(vector(0) * _1_1 + vector(1) * _2_1 + vector(2) * _3_1,
              vector(0) * _1_2 + vector(1) * _2_2 + vector(2) * _3_2,
              vector(0) * _1_3 + vector(1) * _2_3 + vector(2) * _3_3)
  
  override def * (that: M): M =
    Space(_1_1 * that._1_1 + _1_2 * that._2_1 + _1_3 * that._3_1,
          _1_1 * that._1_2 + _1_2 * that._2_2 + _1_3 * that._3_2,
          _1_1 * that._1_3 + _1_2 * that._2_3 + _1_3 * that._3_3,
          _2_1 * that._1_1 + _2_2 * that._2_1 + _2_3 * that._3_1,
          _2_1 * that._1_2 + _2_2 * that._2_2 + _2_3 * that._3_2,
          _2_1 * that._1_3 + _2_2 * that._2_3 + _2_3 * that._3_3,
          _3_1 * that._1_1 + _3_2 * that._2_1 + _3_3 * that._3_1,
          _3_1 * that._1_2 + _3_2 * that._2_2 + _3_3 * that._3_2,
          _3_1 * that._1_3 + _3_2 * that._2_3 + _3_3 * that._3_3)
  
  override def inverse: Option[M] = {
    // all 2x2 determinants minor_i_j with row i and column j blocked out.
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
    if (math.abs(det) >= java.lang.Double.MIN_NORMAL)
      Some(Space(minor_1_1 / det, -minor_2_1 / det,  minor_3_1 / det,
                -minor_1_2 / det,  minor_2_2 / det, -minor_3_2 / det,
                 minor_1_3 / det, -minor_2_3 / det,  minor_3_3 / det))
    else None
  }
  
  override def transpose: M =
    Space(_1_1, _2_1, _3_1,
          _1_2, _2_2, _3_2,
          _1_3, _2_3, _3_3)
  
  override def determinant: Real = {
    // 2x2 determinants minor_i_j with row i and column j blocked out.
    val minor_1_1 = _2_2 * _3_3 - _2_3 * _3_2
    val minor_1_2 = _2_1 * _3_3 - _2_3 * _3_1
    val minor_1_3 = _2_1 * _3_2 - _2_2 * _3_1
    new Real(_1_1 * minor_1_1 - _1_2 * minor_1_2 + _1_3 * minor_1_3)
  }
}
