/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

trait MatrixF3x3 extends MatrixFMxN { self =>
  override type Space <: F3x3 with Singleton {
    type Matrix    = self.Matrix
    type RowVector = self.RowVector
    type Scalar    = self.Scalar
  }
  
  override type Matrix >: self.type <: MatrixF3x3 {
    type Matrix    = self.Matrix
    type RowVector = self.RowVector
    type Scalar    = self.Scalar
  }
  
  override type Transpose = Matrix
  
  override type RowVector <: VectorF3 {
    type Vector = self.RowVector
    type Scalar = self.Scalar
  }
  
  override type ColumnVector = RowVector
  
  override type Scalar <: Field {
    type Scalar = self.Scalar
  }
  
  final override def M: Int = 3
  
  final override def N: Int = 3
  
  def row1: RowVector = Space.Row(entry(0), entry(1), entry(2))
  
  def row2: RowVector = Space.Row(entry(3), entry(4), entry(5))
  
  def row3: RowVector = Space.Row(entry(6), entry(7), entry(8))
  
  def column1: ColumnVector = Space.Column(entry(0), entry(3), entry(6))
  
  def column2: ColumnVector = Space.Column(entry(1), entry(4), entry(7))
  
  def column3: ColumnVector = Space.Column(entry(2), entry(5), entry(8))
  
  override def + (that: Matrix): Matrix =
    Space(entry(0) + that.entry(0), entry(1) + that.entry(1), entry(2) + that.entry(2),
          entry(3) + that.entry(3), entry(4) + that.entry(4), entry(5) + that.entry(5),
          entry(6) + that.entry(6), entry(7) + that.entry(7), entry(8) + that.entry(8))
  
  override def unary_- : Matrix =
    Space(-entry(0), -entry(1), -entry(2),
          -entry(3), -entry(4), -entry(5),
          -entry(6), -entry(7), -entry(8))
  
  override def - (that: Matrix): Matrix =
    Space(entry(0) - that.entry(0), entry(1) - that.entry(1), entry(2) - that.entry(2),
          entry(3) - that.entry(3), entry(4) - that.entry(4), entry(5) - that.entry(5),
          entry(6) - that.entry(6), entry(7) - that.entry(7), entry(8) - that.entry(8))
  
  override def :* (scalar: Scalar): Matrix =
    Space(entry(0) * scalar, entry(1) * scalar, entry(2) * scalar,
          entry(3) * scalar, entry(4) * scalar, entry(5) * scalar,
          entry(6) * scalar, entry(7) * scalar, entry(8) * scalar)
  
  override def *: (scalar: Scalar): Matrix =
    Space(scalar * entry(0), scalar * entry(1), scalar * entry(2),
          scalar * entry(3), scalar * entry(4), scalar * entry(5),
          scalar * entry(6), scalar * entry(7), scalar * entry(8))
  
  override def :* (vector: RowVector): ColumnVector =
    Space.Column(entry(0) * vector.coord(0) + entry(1) * vector.coord(1) + entry(2) * vector.coord(2),
                 entry(3) * vector.coord(0) + entry(4) * vector.coord(1) + entry(5) * vector.coord(2),
                 entry(6) * vector.coord(0) + entry(7) * vector.coord(1) + entry(8) * vector.coord(2))
  
  override def *: (vector: ColumnVector): RowVector =
    Space.Row(vector.coord(0) * entry(0) + vector.coord(1) * entry(3) + vector.coord(2) * entry(6),
              vector.coord(0) * entry(1) + vector.coord(1) * entry(4) + vector.coord(2) * entry(7),
              vector.coord(0) * entry(2) + vector.coord(1) * entry(5) + vector.coord(2) * entry(8))
  
  def * (that: Matrix): Matrix =
    Space(entry(0) * that.entry(0) + entry(1) * that.entry(3) + entry(2) * that.entry(6),
          entry(0) * that.entry(1) + entry(1) * that.entry(4) + entry(2) * that.entry(7),
          entry(0) * that.entry(2) + entry(1) * that.entry(5) + entry(2) * that.entry(8),
          entry(3) * that.entry(0) + entry(4) * that.entry(3) + entry(5) * that.entry(6),
          entry(3) * that.entry(1) + entry(4) * that.entry(4) + entry(5) * that.entry(7),
          entry(3) * that.entry(2) + entry(4) * that.entry(5) + entry(5) * that.entry(8),
          entry(6) * that.entry(0) + entry(7) * that.entry(3) + entry(8) * that.entry(6),
          entry(6) * that.entry(1) + entry(7) * that.entry(4) + entry(8) * that.entry(7),
          entry(6) * that.entry(2) + entry(7) * that.entry(5) + entry(8) * that.entry(8))
  
  def inverse: Option[Matrix] = {
    // all 2x2 determinants minor_i_j with row i and column j blocked out.
    val minor_1_1 = entry(4) * entry(8) - entry(5) * entry(7)
    val minor_1_2 = entry(3) * entry(8) - entry(5) * entry(6)
    val minor_1_3 = entry(3) * entry(7) - entry(4) * entry(6)
    val minor_2_1 = entry(1) * entry(8) - entry(2) * entry(7)
    val minor_2_2 = entry(0) * entry(8) - entry(2) * entry(6)
    val minor_2_3 = entry(0) * entry(7) - entry(1) * entry(6)
    val minor_3_1 = entry(1) * entry(5) - entry(2) * entry(4)
    val minor_3_2 = entry(0) * entry(5) - entry(2) * entry(3)
    val minor_3_3 = entry(0) * entry(4) - entry(1) * entry(3)
    
    val det = entry(0) * minor_1_1 - entry(1) * minor_1_2 + entry(2) * minor_1_3
    if (det != Space.Scalar.zero)
      Some(Space(minor_1_1 / det, -minor_2_1 / det,  minor_3_1 / det,
                -minor_1_2 / det,  minor_2_2 / det, -minor_3_2 / det,
                 minor_1_3 / det, -minor_2_3 / det,  minor_3_3 / det))
    else None
  }
  
  override def transpose: Transpose =
    Space(entry(0), entry(3), entry(6),
          entry(1), entry(4), entry(7),
          entry(2), entry(5), entry(8))
  
  def determinant: Scalar = {
    // 2x2 determinants minor_i_j with row i and column j blocked out.
    val minor_1_1 = entry(4) * entry(8) - entry(5) * entry(7)
    val minor_1_2 = entry(3) * entry(8) - entry(5) * entry(6)
    val minor_1_3 = entry(3) * entry(7) - entry(4) * entry(6)
    entry(0) * minor_1_1 - entry(1) * minor_1_2 + entry(2) * minor_1_3
  }
}
