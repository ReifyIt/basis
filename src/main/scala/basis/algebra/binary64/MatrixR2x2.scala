/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

import generic._

trait MatrixR2x2 extends MatrixF2x2 with MatrixRMxN { self =>
  override type Matrix >: self.type <: MatrixR2x2 {
    type Matrix    = self.Matrix
    type RowVector = self.RowVector
  }
  
  override type RowVector <: VectorR2 {
    type Vector = self.RowVector
  }
  
  override def Space: R2x2 {
    type Matrix    = self.Matrix
    type RowVector = self.RowVector
  }
  
  override def row1: RowVector = Space.Row(this(0), this(1))
  
  override def row2: RowVector = Space.Row(this(2), this(3))
  
  override def column1: ColumnVector = Space.Column(this(0), this(2))
  
  override def column2: ColumnVector = Space.Column(this(1), this(3))
  
  override def + (that: Matrix): Matrix =
    Space(this(0) + that(0), this(1) + that(1),
          this(2) + that(2), this(3) + that(3))
  
  override def unary_- : Matrix =
    Space(-this(0), -this(1),
          -this(2), -this(3))
  
  override def - (that: Matrix): Matrix =
    Space(this(0) - that(0), this(1) - that(1),
          this(2) - that(2), this(3) - that(3))
  
  override def :* (scalar: Double): Matrix =
    Space(this(0) * scalar, this(1) * scalar,
          this(2) * scalar, this(3) * scalar)
  
  override def *: (scalar: Double): Matrix = this :* scalar
  
  override def :* (vector: RowVector): ColumnVector =
    Space.Column(this(0) * vector(0) + this(1) * vector(1),
                 this(2) * vector(0) + this(3) * vector(1))
  
  override def *: (vector: ColumnVector): RowVector =
    Space.Row(vector(0) * this(0) + vector(1) * this(2),
              vector(0) * this(1) + vector(1) * this(3))
  
  override def * (that: Matrix): Matrix =
    Space(this(0) * that(0) + this(1) * that(2),
          this(0) * that(1) + this(1) * that(3),
          this(2) * that(0) + this(3) * that(2),
          this(2) * that(1) + this(3) * that(3))
  
  override def inverse: Option[Matrix] = {
    val det = this(0) * this(3) - this(1) * this(2)
    if (math.abs(det) >= Double.MinPositiveValue)
      Some(Space(this(3) / det, -this(1) / det,
                -this(2) / det,  this(0) / det))
    else None
  }
  
  override def transpose: Transpose =
    Space.Transpose(this(0), this(2),  this(1), this(3))
  
  override def determinant: Real =
    new Real(this(0) * this(3) - this(1) * this(2))
}
