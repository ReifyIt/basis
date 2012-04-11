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
  
  override def column1: V = Space.Column(this(0), this(2))
  
  override def column2: V = Space.Column(this(1), this(3))
  
  override def row1: V = Space.Row(this(0), this(1))
  
  override def row2: V = Space.Row(this(2), this(3))
  
  override def + (that: M): M =
    Space(this(0) + that(0), this(1) + that(1),
          this(2) + that(2), this(3) + that(3))
  
  override def unary_- : M =
    Space(-this(0), -this(1),
          -this(2), -this(3))
  
  override def - (that: M): M =
    Space(this(0) - that(0), this(1) - that(1),
          this(2) - that(2), this(3) - that(3))
  
  override def :* (scalar: Double): M =
    Space(this(0) * scalar, this(1) * scalar,
          this(2) * scalar, this(3) * scalar)
  
  override def *: (scalar: Double): M = this :* scalar
  
  override def :* (vector: V): V =
    Space.Column(this(0) * vector(0) + this(1) * vector(1),
                 this(2) * vector(0) + this(3) * vector(1))
  
  override def *: (vector: V): V =
    Space.Row(vector(0) * this(0) + vector(1) * this(2),
              vector(0) * this(1) + vector(1) * this(3))
  
  override def * (that: M): M =
    Space(this(0) * that(0) + this(1) * that(2),
          this(0) * that(1) + this(1) * that(3),
          this(2) * that(0) + this(3) * that(2),
          this(2) * that(1) + this(3) * that(3))
  
  override def inverse: Option[M] = {
    val det = this(0) * this(3) - this(1) * this(2)
    if (math.abs(det) >= java.lang.Double.MIN_NORMAL)
      Some(Space(this(3) / det, -this(1) / det,
                -this(2) / det,  this(0) / det))
    else None
  }
  
  override def transpose: M =
    Space.Transpose(this(0), this(2),  this(1), this(3))
  
  override def determinant: Real =
    new Real(this(0) * this(3) - this(1) * this(2))
}
