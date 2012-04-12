/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait MatrixF2x2[M <: MatrixF2x2[M, V, S],
                 V <: VectorF2[V, S],
                 S <: Field[S]]
  extends MatrixFMxN[M, M, V, V, S] {
  
  def Space: F2x2 {
    type Matrix = M
    type RowVector = V
    type Scalar = S
  }
  
  final override def M: Int = 2
  
  final override def N: Int = 2
  
  def column1: V = Space.Column(entry(0), entry(2))
  
  def column2: V = Space.Column(entry(1), entry(3))
  
  def row1: V = Space.Row(entry(0), entry(1))
  
  def row2: V = Space.Row(entry(2), entry(3))
  
  override def + (that: M): M =
    Space(entry(0) + that.entry(0), entry(1) + that.entry(1),
          entry(2) + that.entry(2), entry(3) + that.entry(3))
  
  override def unary_- : M =
    Space(-entry(0), -entry(1),
          -entry(2), -entry(3))
  
  override def - (that: M): M =
    Space(entry(0) - that.entry(0), entry(1) - that.entry(1),
          entry(2) - that.entry(2), entry(3) - that.entry(3))
  
  override def :* (scalar: S): M =
    Space(entry(0) * scalar, entry(1) * scalar,
          entry(2) * scalar, entry(3) * scalar)
  
  override def *: (scalar: S): M =
    Space(scalar * entry(0), scalar * entry(1),
          scalar * entry(2), scalar * entry(3))
  
  override def :* (vector: V): V =
    Space.Column(entry(0) * vector.coord(0) + entry(1) * vector.coord(1),
                 entry(2) * vector.coord(0) + entry(3) * vector.coord(1))
  
  override def *: (vector: V): V =
    Space.Row(vector.coord(0) * entry(0) + vector.coord(1) * entry(2),
              vector.coord(0) * entry(1) + vector.coord(1) * entry(3))
  
  def * (that: M): M =
    Space(entry(0) * that.entry(0) + entry(1) * that.entry(2),
          entry(0) * that.entry(1) + entry(1) * that.entry(3),
          entry(2) * that.entry(0) + entry(3) * that.entry(2),
          entry(2) * that.entry(1) + entry(3) * that.entry(3))
  
  def inverse: Option[M] = {
    val det = determinant
    if (det != Space.Scalar.zero)
      Some(Space(entry(3) / det, -entry(1) / det,
                -entry(2) / det,  entry(0) / det))
    else None
  }
  
  override def transpose: M =
    Space.Transpose(entry(0), entry(2),  entry(1), entry(3))
  
  def determinant: S =
    entry(0) * entry(3) - entry(1) * entry(2)
}
