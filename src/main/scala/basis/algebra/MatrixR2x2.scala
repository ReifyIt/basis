/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait MatrixR2x2[M <: MatrixR2x2[M, V], V <: VectorR2[V]]
  extends MatrixF2x2[M, V, Real] with RealMatrix[M, M, V, V] {
  
  def Space: R2x2 {
    type Matrix = M
    type RowVector = V
  }
  
  def _1_1: Double
  def _1_2: Double
  def _2_1: Double
  def _2_2: Double
  
  def apply(k: Int): Double = k match {
    case 0 => _1_1
    case 1 => _1_2
    case 2 => _2_1
    case 3 => _2_2
    case _ => throw new IndexOutOfBoundsException(k.toString)
  }
  
  override def column1: V = Space.Column(_1_1, _2_1)
  
  override def column2: V = Space.Column(_1_2, _2_2)
  
  override def row1: V = Space.Row(_1_1, _1_2)
  
  override def row2: V = Space.Row(_2_1, _2_2)
  
  override def + (that: M): M =
    Space(_1_1 + that._1_1, _1_2 + that._1_2,
          _2_1 + that._2_1, _2_2 + that._2_2)
  
  override def unary_- : M =
    Space(-_1_1, -_1_2,
          -_2_1, -_2_2)
  
  override def - (that: M): M =
    Space(_1_1 - that._1_1, _1_2 - that._1_2,
          _2_1 - that._2_1, _2_2 - that._2_2)
  
  override def :* (scalar: Double): M =
    Space(_1_1 * scalar, _1_2 * scalar,
          _2_1 * scalar, _2_2 * scalar)
  
  override def *: (scalar: Double): M = this :* scalar
  
  override def :* (vector: V): V =
    Space.Column(_1_1 * vector(0) + _1_2 * vector(1),
                 _2_1 * vector(0) + _2_2 * vector(1))
  
  override def *: (vector: V): V =
    Space.Row(vector(0) * _1_1 + vector(1) * _2_1,
              vector(0) * _1_2 + vector(1) * _2_2)
  
  override def * (that: M): M =
    Space(_1_1 * that._1_1 + _1_2 * that._2_1,
          _1_1 * that._1_2 + _1_2 * that._2_2,
          _2_1 * that._1_1 + _2_2 * that._2_1,
          _2_1 * that._1_2 + _2_2 * that._2_2)
  
  override def inverse: Option[M] = {
    val det = _1_1 * _2_2 - _1_2 * _2_1
    if (math.abs(det) >= java.lang.Double.MIN_NORMAL)
      Some(Space(_2_2 / det, -_1_2 / det,
                -_2_1 / det,  _1_1 / det))
    else None
  }
  
  override def transpose: M =
    Space.Transpose(_1_1, _2_1,  _1_2, _2_2)
  
  override def determinant: Real = new Real(_1_1 * _2_2 - _1_2 * _2_1)
}
