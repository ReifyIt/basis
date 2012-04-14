/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

trait MatrixR3x3 extends MatrixF3x3 with MatrixRMxN { self =>
  override type Matrix >: self.type <: MatrixR3x3 {
    type Matrix    = self.Matrix
    type RowVector = self.RowVector
  }
  
  override type RowVector <: VectorR3 {
    type Vector = self.RowVector
  }
  
  override def Space: R3x3 {
    type Matrix    = self.Matrix
    type RowVector = self.RowVector
  }
  
  override def row1: RowVector = Space.Row(this(0), this(1), this(2))
  
  override def row2: RowVector = Space.Row(this(3), this(4), this(5))
  
  override def row3: RowVector = Space.Row(this(6), this(7), this(8))
  
  override def column1: ColumnVector = Space.Column(this(0), this(3), this(6))
  
  override def column2: ColumnVector = Space.Column(this(1), this(4), this(7))
  
  override def column3: ColumnVector = Space.Column(this(2), this(5), this(8))
  
  override def + (that: Matrix): Matrix =
    Space(this(0) + that(0), this(1) + that(1), this(2) + that(2),
          this(3) + that(3), this(4) + that(4), this(5) + that(5),
          this(6) + that(6), this(7) + that(7), this(8) + that(8))
  
  override def unary_- : Matrix =
    Space(-this(0), -this(1), -this(2),
          -this(3), -this(4), -this(5),
          -this(6), -this(7), -this(8))
  
  override def - (that: Matrix): Matrix =
    Space(this(0) - that(0), this(1) - that(1), this(2) - that(2),
          this(3) - that(3), this(4) - that(4), this(5) - that(5),
          this(6) - that(6), this(7) - that(7), this(8) - that(8))
  
  override def :* (scalar: Double): Matrix =
    Space(this(0) * scalar, this(1) * scalar, this(2) * scalar,
          this(3) * scalar, this(4) * scalar, this(5) * scalar,
          this(6) * scalar, this(7) * scalar, this(8) * scalar)
  
  override def *: (scalar: Double): Matrix = this :* scalar
  
  override def :* (vector: RowVector): ColumnVector =
    Space.Column(this(0) * vector(0) + this(1) * vector(1) + this(2) * vector(2),
                 this(3) * vector(0) + this(4) * vector(1) + this(5) * vector(2),
                 this(6) * vector(0) + this(7) * vector(1) + this(8) * vector(2))
  
  override def *: (vector: ColumnVector): RowVector =
    Space.Row(vector(0) * this(0) + vector(1) * this(3) + vector(2) * this(6),
              vector(0) * this(1) + vector(1) * this(4) + vector(2) * this(7),
              vector(0) * this(2) + vector(1) * this(5) + vector(2) * this(8))
  
  override def * (that: Matrix): Matrix =
    Space(this(0) * that(0) + this(1) * that(3) + this(2) * that(6),
          this(0) * that(1) + this(1) * that(4) + this(2) * that(7),
          this(0) * that(2) + this(1) * that(5) + this(2) * that(8),
          this(3) * that(0) + this(4) * that(3) + this(5) * that(6),
          this(3) * that(1) + this(4) * that(4) + this(5) * that(7),
          this(3) * that(2) + this(4) * that(5) + this(5) * that(8),
          this(6) * that(0) + this(7) * that(3) + this(8) * that(6),
          this(6) * that(1) + this(7) * that(4) + this(8) * that(7),
          this(6) * that(2) + this(7) * that(5) + this(8) * that(8))
  
  override def inverse: Option[Matrix] = {
    // all 2x2 determinants minor_i_j with row i and column j blocked out.
    val minor_1_1 = this(4) * this(8) - this(5) * this(7)
    val minor_1_2 = this(3) * this(8) - this(5) * this(6)
    val minor_1_3 = this(3) * this(7) - this(4) * this(6)
    val minor_2_1 = this(1) * this(8) - this(2) * this(7)
    val minor_2_2 = this(0) * this(8) - this(2) * this(6)
    val minor_2_3 = this(0) * this(7) - this(1) * this(6)
    val minor_3_1 = this(1) * this(5) - this(2) * this(4)
    val minor_3_2 = this(0) * this(5) - this(2) * this(3)
    val minor_3_3 = this(0) * this(4) - this(1) * this(3)
    
    val det = this(0) * minor_1_1 - this(1) * minor_1_2 + this(2) * minor_1_3
    if (math.abs(det) >= Double.MinPositiveValue)
      Some(Space(minor_1_1 / det, -minor_2_1 / det,  minor_3_1 / det,
                -minor_1_2 / det,  minor_2_2 / det, -minor_3_2 / det,
                 minor_1_3 / det, -minor_2_3 / det,  minor_3_3 / det))
    else None
  }
  
  override def transpose: Transpose =
    Space(this(0), this(3), this(6),
          this(1), this(4), this(7),
          this(2), this(5), this(8))
  
  override def determinant: Real = {
    // 2x2 determinants minor_i_j with row i and column j blocked out.
    val minor_1_1 = this(4) * this(8) - this(5) * this(7)
    val minor_1_2 = this(3) * this(8) - this(5) * this(6)
    val minor_1_3 = this(3) * this(7) - this(4) * this(6)
    new Real(this(0) * minor_1_1 - this(1) * minor_1_2 + this(2) * minor_1_3)
  }
}
