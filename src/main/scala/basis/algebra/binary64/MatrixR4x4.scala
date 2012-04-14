/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

import generic._

trait MatrixR4x4 extends MatrixF4x4 with MatrixRMxN { self =>
  override type Matrix >: self.type <: MatrixR4x4 {
    type Matrix    = self.Matrix
    type RowVector = self.RowVector
  }
  
  override type RowVector <: VectorR4 {
    type Vector = self.RowVector
  }
  
  override def Space: R4x4 {
    type Matrix    = self.Matrix
    type RowVector = self.RowVector
  }
  
  override def row1: RowVector = Space.Row(this( 0), this( 1), this( 2), this( 3))
  
  override def row2: RowVector = Space.Row(this( 4), this( 5), this( 6), this( 7))
  
  override def row3: RowVector = Space.Row(this( 8), this( 9), this(10), this(11))
  
  override def row4: RowVector = Space.Row(this(12), this(13), this(14), this(15))
  
  override def column1: ColumnVector = Space.Column(this( 0), this( 4), this( 8), this(12))
  
  override def column2: ColumnVector = Space.Column(this( 1), this( 5), this( 9), this(13))
  
  override def column3: ColumnVector = Space.Column(this( 2), this( 6), this(10), this(14))
  
  override def column4: ColumnVector = Space.Column(this( 3), this( 7), this(11), this(15))
  
  override def + (that: Matrix): Matrix =
    Space(this( 0) + that( 0), this( 1) + that( 1), this( 2) + that( 2), this( 3) + that( 3),
          this( 4) + that( 4), this( 5) + that( 5), this( 6) + that( 6), this( 7) + that( 7),
          this( 8) + that( 8), this( 9) + that( 9), this(10) + that(10), this(11) + that(11),
          this(12) + that(12), this(13) + that(13), this(14) + that(14), this(15) + that(15))
  
  override def unary_- : Matrix =
    Space(-this( 0), -this( 1), -this( 2), -this( 3),
          -this( 4), -this( 5), -this( 6), -this( 7),
          -this( 8), -this( 9), -this(10), -this(11),
          -this(12), -this(13), -this(14), -this(15))
  
  override def - (that: Matrix): Matrix =
    Space(this( 0) - that( 0), this( 1) - that( 1), this( 2) - that( 2), this( 3) - that( 3),
          this( 4) - that( 4), this( 5) - that( 5), this( 6) - that( 6), this( 7) - that( 7),
          this( 8) - that( 8), this( 9) - that( 9), this(10) - that(10), this(11) - that(11),
          this(12) - that(12), this(13) - that(13), this(14) - that(14), this(15) - that(15))
  
  override def :* (scalar: Double): Matrix =
    Space(this( 0) * scalar, this( 1) * scalar, this( 2) * scalar, this( 3) * scalar,
          this( 4) * scalar, this( 5) * scalar, this( 6) * scalar, this( 7) * scalar,
          this( 8) * scalar, this( 9) * scalar, this(10) * scalar, this(11) * scalar,
          this(12) * scalar, this(13) * scalar, this(14) * scalar, this(15) * scalar)
  
  override def *: (scalar: Double): Matrix = this :* scalar
  
  override def :* (vector: RowVector): ColumnVector =
    Space.Column(this( 0) * vector(0) + this( 1) * vector(1) + this( 2) * vector(2) + this( 3) * vector(3),
                 this( 4) * vector(0) + this( 5) * vector(1) + this( 6) * vector(2) + this( 7) * vector(3),
                 this( 8) * vector(0) + this( 9) * vector(1) + this(10) * vector(2) + this(11) * vector(3),
                 this(12) * vector(0) + this(13) * vector(1) + this(14) * vector(2) + this(15) * vector(3))
  
  override def *: (vector: ColumnVector): RowVector =
    Space.Row(vector(0) * this( 0) + vector(1) * this( 4) + vector(2) * this( 8) + vector(3) * this(12),
              vector(0) * this( 1) + vector(1) * this( 5) + vector(2) * this( 9) + vector(3) * this(13),
              vector(0) * this( 2) + vector(1) * this( 6) + vector(2) * this(10) + vector(3) * this(14),
              vector(0) * this( 3) + vector(1) * this( 7) + vector(2) * this(11) + vector(3) * this(15))
  
  override def * (that: Matrix): Matrix =
    Space(this( 0) * that( 0) + this( 1) * that( 4) + this( 2) * that( 8) + this( 3) * that(12),
          this( 0) * that( 1) + this( 1) * that( 5) + this( 2) * that( 9) + this( 3) * that(13),
          this( 0) * that( 2) + this( 1) * that( 6) + this( 2) * that(10) + this( 3) * that(14),
          this( 0) * that( 3) + this( 1) * that( 7) + this( 2) * that(11) + this( 3) * that(15),
          this( 4) * that( 0) + this( 5) * that( 4) + this( 6) * that( 8) + this( 7) * that(12),
          this( 4) * that( 1) + this( 5) * that( 5) + this( 6) * that( 9) + this( 7) * that(13),
          this( 4) * that( 2) + this( 5) * that( 6) + this( 6) * that(10) + this( 7) * that(14),
          this( 4) * that( 3) + this( 5) * that( 7) + this( 6) * that(11) + this( 7) * that(15),
          this( 8) * that( 0) + this( 9) * that( 4) + this(10) * that( 8) + this(11) * that(12),
          this( 8) * that( 1) + this( 9) * that( 5) + this(10) * that( 9) + this(11) * that(13),
          this( 8) * that( 2) + this( 9) * that( 6) + this(10) * that(10) + this(11) * that(14),
          this( 8) * that( 3) + this( 9) * that( 7) + this(10) * that(11) + this(11) * that(15),
          this(12) * that( 0) + this(13) * that( 4) + this(14) * that( 8) + this(15) * that(12),
          this(12) * that( 1) + this(13) * that( 5) + this(14) * that( 9) + this(15) * that(13),
          this(12) * that( 2) + this(13) * that( 6) + this(14) * that(10) + this(15) * that(14),
          this(12) * that( 3) + this(13) * that( 7) + this(14) * that(11) + this(15) * that(15))
  
  override def inverse: Option[Matrix] = {
    // all 2x2 determinants minor_i1_i2__j1_j2 with
    // rows i1 and i2 and columns j1 and j2 blocked out.
    val minor_1_2__1_2 = this(10) * this(15) - this(11) * this(14)
    val minor_1_2__1_3 = this( 9) * this(15) - this(11) * this(13)
    val minor_1_2__1_4 = this( 9) * this(14) - this(10) * this(13)
    val minor_1_2__2_3 = this( 8) * this(15) - this(11) * this(12)
    val minor_1_2__2_4 = this( 8) * this(14) - this(10) * this(12)
    val minor_1_2__3_4 = this( 8) * this(13) - this( 9) * this(12)
    val minor_1_3__1_2 = this( 6) * this(15) - this( 7) * this(14)
    val minor_1_3__1_3 = this( 5) * this(15) - this( 7) * this(13)
    val minor_1_3__1_4 = this( 5) * this(14) - this( 6) * this(13)
    val minor_1_3__2_3 = this( 4) * this(15) - this( 7) * this(12)
    val minor_1_3__2_4 = this( 4) * this(14) - this( 6) * this(12)
    val minor_1_3__3_4 = this( 4) * this(13) - this( 5) * this(12)
    val minor_1_4__1_2 = this( 6) * this(11) - this( 7) * this(10)
    val minor_1_4__1_3 = this( 5) * this(11) - this( 7) * this( 9)
    val minor_1_4__1_4 = this( 5) * this(10) - this( 6) * this( 9)
    val minor_1_4__2_3 = this( 4) * this(11) - this( 7) * this( 8)
    val minor_1_4__2_4 = this( 4) * this(10) - this( 6) * this( 8)
    val minor_1_4__3_4 = this( 4) * this( 9) - this( 5) * this( 8)
    
    // all 3x3 determinants minor_i_j with row i and column j blocked out.
    val minor_1_1 = this( 5) * minor_1_2__1_2 - this( 6) * minor_1_2__1_3 + this( 7) * minor_1_2__1_4
    val minor_1_2 = this( 4) * minor_1_2__1_2 - this( 6) * minor_1_2__2_3 + this( 7) * minor_1_2__2_4
    val minor_1_3 = this( 4) * minor_1_2__1_3 - this( 5) * minor_1_2__2_3 + this( 7) * minor_1_2__3_4
    val minor_1_4 = this( 4) * minor_1_2__1_4 - this( 5) * minor_1_2__2_4 + this( 6) * minor_1_2__3_4
    val minor_2_1 = this( 1) * minor_1_2__1_2 - this( 2) * minor_1_2__1_3 + this( 3) * minor_1_2__1_4
    val minor_2_2 = this( 0) * minor_1_2__1_2 - this( 2) * minor_1_2__2_3 + this( 3) * minor_1_2__2_4
    val minor_2_3 = this( 0) * minor_1_2__1_3 - this( 1) * minor_1_2__2_3 + this( 3) * minor_1_2__3_4
    val minor_2_4 = this( 0) * minor_1_2__1_4 - this( 1) * minor_1_2__2_4 + this( 2) * minor_1_2__3_4
    val minor_3_1 = this( 1) * minor_1_3__1_2 - this( 2) * minor_1_3__1_3 + this( 3) * minor_1_3__1_4
    val minor_3_2 = this( 0) * minor_1_3__1_2 - this( 2) * minor_1_3__2_3 + this( 3) * minor_1_3__2_4
    val minor_3_3 = this( 0) * minor_1_3__1_3 - this( 1) * minor_1_3__2_3 + this( 3) * minor_1_3__3_4
    val minor_3_4 = this( 0) * minor_1_3__1_4 - this( 1) * minor_1_3__2_4 + this( 2) * minor_1_3__3_4
    val minor_4_1 = this( 1) * minor_1_4__1_2 - this( 2) * minor_1_4__1_3 + this( 3) * minor_1_4__1_4
    val minor_4_2 = this( 0) * minor_1_4__1_2 - this( 2) * minor_1_4__2_3 + this( 3) * minor_1_4__2_4
    val minor_4_3 = this( 0) * minor_1_4__1_3 - this( 1) * minor_1_4__2_3 + this( 3) * minor_1_4__3_4
    val minor_4_4 = this( 0) * minor_1_4__1_4 - this( 1) * minor_1_4__2_4 + this( 2) * minor_1_4__3_4
    
    val det = this( 0) * minor_1_1 - this( 1) * minor_1_2 + this( 2) * minor_1_3 - this( 3) * minor_1_4
    if (math.abs(det) >= Double.MinPositiveValue)
      Some(Space(minor_1_1 / det, -minor_2_1 / det,  minor_3_1 / det, -minor_4_1 / det,
                -minor_1_2 / det,  minor_2_2 / det, -minor_3_2 / det,  minor_4_2 / det,
                 minor_1_3 / det, -minor_2_3 / det,  minor_3_3 / det, -minor_4_3 / det,
                -minor_1_4 / det,  minor_2_4 / det, -minor_3_4 / det,  minor_4_4 / det))
    else None
  }
  
  override def transpose: Transpose =
    Space(this( 0), this( 4), this( 8), this(12),
          this( 1), this( 5), this( 9), this(13),
          this( 2), this( 6), this(10), this(14),
          this( 3), this( 7), this(11), this(15))
  
  override def determinant: Real = {
    // 2x2 determinants minor_i1_i2__j1_j2 with
    // rows i1 and i2 and columns j1 and j2 blocked out.
    val minor_1_2__1_2 = this(10) * this(15) - this(11) * this(14)
    val minor_1_2__1_3 = this( 9) * this(15) - this(11) * this(13)
    val minor_1_2__1_4 = this( 9) * this(14) - this(10) * this(13)
    val minor_1_2__2_3 = this( 8) * this(15) - this(11) * this(12)
    val minor_1_2__2_4 = this( 8) * this(14) - this(10) * this(12)
    val minor_1_2__3_4 = this( 8) * this(13) - this( 9) * this(12)
    
    // 3x3 determinants minor_i_j with row i and column j blocked out.
    val minor_1_1 = this( 5) * minor_1_2__1_2 - this( 6) * minor_1_2__1_3 + this( 7) * minor_1_2__1_4
    val minor_1_2 = this( 4) * minor_1_2__1_2 - this( 6) * minor_1_2__2_3 + this( 7) * minor_1_2__2_4
    val minor_1_3 = this( 4) * minor_1_2__1_3 - this( 5) * minor_1_2__2_3 + this( 7) * minor_1_2__3_4
    val minor_1_4 = this( 4) * minor_1_2__1_4 - this( 5) * minor_1_2__2_4 + this( 6) * minor_1_2__3_4
    
    new Real(this( 0) * minor_1_1 - this( 1) * minor_1_2 + this( 2) * minor_1_3 - this( 3) * minor_1_4)
  }
}
