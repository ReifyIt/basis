/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait MatrixF4x4[M <: MatrixF4x4[M, V, S],
                 V <: VectorF4[V, S],
                 S <: FieldElement[S]]
  extends GeneralMatrix[M, M, V, V, S] {
  
  def Space: F4x4 {
    type Matrix = M
    type RowVector = V
    type Scalar = S
  }
  
  final override def M: Int = 4
  
  final override def N: Int = 4
  
  def column1: V = Space.Column(entry( 0), entry( 4), entry( 8), entry(12))
  
  def column2: V = Space.Column(entry( 1), entry( 5), entry( 9), entry(13))
  
  def column3: V = Space.Column(entry( 2), entry( 6), entry(10), entry(14))
  
  def column4: V = Space.Column(entry( 3), entry( 7), entry(11), entry(15))
  
  def row1: V = Space.Row(entry( 0), entry( 1), entry( 2), entry( 3))
  
  def row2: V = Space.Row(entry( 4), entry( 5), entry( 6), entry( 7))
  
  def row3: V = Space.Row(entry( 8), entry( 9), entry(10), entry(11))
  
  def row4: V = Space.Row(entry(12), entry(13), entry(14), entry(15))
  
  override def + (that: M): M =
    Space(entry( 0) + that.entry( 0), entry( 1) + that.entry( 1), entry( 2) + that.entry( 2), entry( 3) + that.entry( 3),
          entry( 4) + that.entry( 4), entry( 5) + that.entry( 5), entry( 6) + that.entry( 6), entry( 7) + that.entry( 7),
          entry( 8) + that.entry( 8), entry( 9) + that.entry( 9), entry(10) + that.entry(10), entry(11) + that.entry(11),
          entry(12) + that.entry(12), entry(13) + that.entry(13), entry(14) + that.entry(14), entry(15) + that.entry(15))
  
  override def unary_- : M =
    Space(-entry( 0), -entry( 1), -entry( 2), -entry( 3),
          -entry( 4), -entry( 5), -entry( 6), -entry( 7),
          -entry( 8), -entry( 9), -entry(10), -entry(11),
          -entry(12), -entry(13), -entry(14), -entry(15))
  
  override def - (that: M): M =
    Space(entry( 0) - that.entry( 0), entry( 1) - that.entry( 1), entry( 2) - that.entry( 2), entry( 3) - that.entry( 3),
          entry( 4) - that.entry( 4), entry( 5) - that.entry( 5), entry( 6) - that.entry( 6), entry( 7) - that.entry( 7),
          entry( 8) - that.entry( 8), entry( 9) - that.entry( 9), entry(10) - that.entry(10), entry(11) - that.entry(11),
          entry(12) - that.entry(12), entry(13) - that.entry(13), entry(14) - that.entry(14), entry(15) - that.entry(15))
  
  override def :* (scalar: S): M =
    Space(entry( 0) * scalar, entry( 1) * scalar, entry( 2) * scalar, entry( 3) * scalar,
          entry( 4) * scalar, entry( 5) * scalar, entry( 6) * scalar, entry( 7) * scalar,
          entry( 8) * scalar, entry( 9) * scalar, entry(10) * scalar, entry(11) * scalar,
          entry(12) * scalar, entry(13) * scalar, entry(14) * scalar, entry(15) * scalar)
  
  override def *: (scalar: S): M =
    Space(scalar * entry( 0), scalar * entry( 1), scalar * entry( 2), scalar * entry( 3),
          scalar * entry( 4), scalar * entry( 5), scalar * entry( 6), scalar * entry( 7),
          scalar * entry( 8), scalar * entry( 9), scalar * entry(10), scalar * entry(11),
          scalar * entry(12), scalar * entry(13), scalar * entry(14), scalar * entry(15))
  
  override def :* (vector: V): V =
    Space.Column(entry( 0) * vector.coord(0) + entry( 1) * vector.coord(1) + entry( 2) * vector.coord(2) + entry( 3) * vector.coord(3),
                 entry( 4) * vector.coord(0) + entry( 5) * vector.coord(1) + entry( 6) * vector.coord(2) + entry( 7) * vector.coord(3),
                 entry( 8) * vector.coord(0) + entry( 9) * vector.coord(1) + entry(10) * vector.coord(2) + entry(11) * vector.coord(3),
                 entry(12) * vector.coord(0) + entry(13) * vector.coord(1) + entry(14) * vector.coord(2) + entry(15) * vector.coord(3))
  
  override def *: (vector: V): V =
    Space.Row(vector.coord(0) * entry( 0) + vector.coord(1) * entry( 4) + vector.coord(2) * entry( 8) + vector.coord(3) * entry(12),
              vector.coord(0) * entry( 1) + vector.coord(1) * entry( 5) + vector.coord(2) * entry( 9) + vector.coord(3) * entry(13),
              vector.coord(0) * entry( 2) + vector.coord(1) * entry( 6) + vector.coord(2) * entry(10) + vector.coord(3) * entry(14),
              vector.coord(0) * entry( 3) + vector.coord(1) * entry( 7) + vector.coord(2) * entry(11) + vector.coord(3) * entry(15))
  
  def * (that: M): M =
    Space(entry( 0) * that.entry( 0) + entry( 1) * that.entry( 4) + entry( 2) * that.entry( 8) + entry( 3) * that.entry(12),
          entry( 0) * that.entry( 1) + entry( 1) * that.entry( 5) + entry( 2) * that.entry( 9) + entry( 3) * that.entry(13),
          entry( 0) * that.entry( 2) + entry( 1) * that.entry( 6) + entry( 2) * that.entry(10) + entry( 3) * that.entry(14),
          entry( 0) * that.entry( 3) + entry( 1) * that.entry( 7) + entry( 2) * that.entry(11) + entry( 3) * that.entry(15),
          entry( 4) * that.entry( 0) + entry( 5) * that.entry( 4) + entry( 6) * that.entry( 8) + entry( 7) * that.entry(12),
          entry( 4) * that.entry( 1) + entry( 5) * that.entry( 5) + entry( 6) * that.entry( 9) + entry( 7) * that.entry(13),
          entry( 4) * that.entry( 2) + entry( 5) * that.entry( 6) + entry( 6) * that.entry(10) + entry( 7) * that.entry(14),
          entry( 4) * that.entry( 3) + entry( 5) * that.entry( 7) + entry( 6) * that.entry(11) + entry( 7) * that.entry(15),
          entry( 8) * that.entry( 0) + entry( 9) * that.entry( 4) + entry(10) * that.entry( 8) + entry(11) * that.entry(12),
          entry( 8) * that.entry( 1) + entry( 9) * that.entry( 5) + entry(10) * that.entry( 9) + entry(11) * that.entry(13),
          entry( 8) * that.entry( 2) + entry( 9) * that.entry( 6) + entry(10) * that.entry(10) + entry(11) * that.entry(14),
          entry( 8) * that.entry( 3) + entry( 9) * that.entry( 7) + entry(10) * that.entry(11) + entry(11) * that.entry(15),
          entry(12) * that.entry( 0) + entry(13) * that.entry( 4) + entry(14) * that.entry( 8) + entry(15) * that.entry(12),
          entry(12) * that.entry( 1) + entry(13) * that.entry( 5) + entry(14) * that.entry( 9) + entry(15) * that.entry(13),
          entry(12) * that.entry( 2) + entry(13) * that.entry( 6) + entry(14) * that.entry(10) + entry(15) * that.entry(14),
          entry(12) * that.entry( 3) + entry(13) * that.entry( 7) + entry(14) * that.entry(11) + entry(15) * that.entry(15))
  
  def inverse: Option[M] = {
    // all 2x2 determinants minor_i1_i2__j1_j2 with
    // rows i1 and i2 and columns j1 and j2 blocked out.
    val minor_1_2__1_2 = entry(10) * entry(15) - entry(11) * entry(14)
    val minor_1_2__1_3 = entry( 9) * entry(15) - entry(11) * entry(13)
    val minor_1_2__1_4 = entry( 9) * entry(14) - entry(10) * entry(13)
    val minor_1_2__2_3 = entry( 8) * entry(15) - entry(11) * entry(12)
    val minor_1_2__2_4 = entry( 8) * entry(14) - entry(10) * entry(12)
    val minor_1_2__3_4 = entry( 8) * entry(13) - entry( 9) * entry(12)
    val minor_1_3__1_2 = entry( 6) * entry(15) - entry( 7) * entry(14)
    val minor_1_3__1_3 = entry( 5) * entry(15) - entry( 7) * entry(13)
    val minor_1_3__1_4 = entry( 5) * entry(14) - entry( 6) * entry(13)
    val minor_1_3__2_3 = entry( 4) * entry(15) - entry( 7) * entry(12)
    val minor_1_3__2_4 = entry( 4) * entry(14) - entry( 6) * entry(12)
    val minor_1_3__3_4 = entry( 4) * entry(13) - entry( 5) * entry(12)
    val minor_1_4__1_2 = entry( 6) * entry(11) - entry( 7) * entry(10)
    val minor_1_4__1_3 = entry( 5) * entry(11) - entry( 7) * entry( 9)
    val minor_1_4__1_4 = entry( 5) * entry(10) - entry( 6) * entry( 9)
    val minor_1_4__2_3 = entry( 4) * entry(11) - entry( 7) * entry( 8)
    val minor_1_4__2_4 = entry( 4) * entry(10) - entry( 6) * entry( 8)
    val minor_1_4__3_4 = entry( 4) * entry( 9) - entry( 5) * entry( 8)
    
    // all 3x3 determinants minor_i_j with row i and column j blocked out.
    val minor_1_1 = entry( 5) * minor_1_2__1_2 - entry( 6) * minor_1_2__1_3 + entry( 7) * minor_1_2__1_4
    val minor_1_2 = entry( 4) * minor_1_2__1_2 - entry( 6) * minor_1_2__2_3 + entry( 7) * minor_1_2__2_4
    val minor_1_3 = entry( 4) * minor_1_2__1_3 - entry( 5) * minor_1_2__2_3 + entry( 7) * minor_1_2__3_4
    val minor_1_4 = entry( 4) * minor_1_2__1_4 - entry( 5) * minor_1_2__2_4 + entry( 6) * minor_1_2__3_4
    val minor_2_1 = entry( 1) * minor_1_2__1_2 - entry( 2) * minor_1_2__1_3 + entry( 3) * minor_1_2__1_4
    val minor_2_2 = entry( 0) * minor_1_2__1_2 - entry( 2) * minor_1_2__2_3 + entry( 3) * minor_1_2__2_4
    val minor_2_3 = entry( 0) * minor_1_2__1_3 - entry( 1) * minor_1_2__2_3 + entry( 3) * minor_1_2__3_4
    val minor_2_4 = entry( 0) * minor_1_2__1_4 - entry( 1) * minor_1_2__2_4 + entry( 2) * minor_1_2__3_4
    val minor_3_1 = entry( 1) * minor_1_3__1_2 - entry( 2) * minor_1_3__1_3 + entry( 3) * minor_1_3__1_4
    val minor_3_2 = entry( 0) * minor_1_3__1_2 - entry( 2) * minor_1_3__2_3 + entry( 3) * minor_1_3__2_4
    val minor_3_3 = entry( 0) * minor_1_3__1_3 - entry( 1) * minor_1_3__2_3 + entry( 3) * minor_1_3__3_4
    val minor_3_4 = entry( 0) * minor_1_3__1_4 - entry( 1) * minor_1_3__2_4 + entry( 2) * minor_1_3__3_4
    val minor_4_1 = entry( 1) * minor_1_4__1_2 - entry( 2) * minor_1_4__1_3 + entry( 3) * minor_1_4__1_4
    val minor_4_2 = entry( 0) * minor_1_4__1_2 - entry( 2) * minor_1_4__2_3 + entry( 3) * minor_1_4__2_4
    val minor_4_3 = entry( 0) * minor_1_4__1_3 - entry( 1) * minor_1_4__2_3 + entry( 3) * minor_1_4__3_4
    val minor_4_4 = entry( 0) * minor_1_4__1_4 - entry( 1) * minor_1_4__2_4 + entry( 2) * minor_1_4__3_4
    
    val det = entry( 0) * minor_1_1 - entry( 1) * minor_1_2 + entry( 2) * minor_1_3 - entry( 3) * minor_1_4
    if (det != Space.Scalar.zero)
      Some(Space(minor_1_1 / det, -minor_2_1 / det,  minor_3_1 / det, -minor_4_1 / det,
                -minor_1_2 / det,  minor_2_2 / det, -minor_3_2 / det,  minor_4_2 / det,
                 minor_1_3 / det, -minor_2_3 / det,  minor_3_3 / det, -minor_4_3 / det,
                -minor_1_4 / det,  minor_2_4 / det, -minor_3_4 / det,  minor_4_4 / det))
    else None
  }
  
  override def transpose: M =
    Space(entry( 0), entry( 4), entry( 8), entry(12),
          entry( 1), entry( 5), entry( 9), entry(13),
          entry( 2), entry( 6), entry(10), entry(14),
          entry( 3), entry( 7), entry(11), entry(15))
  
  def determinant: S = {
    // 2x2 determinants minor_i1_i2__j1_j2 with
    // rows i1 and i2 and columns j1 and j2 blocked out.
    val minor_1_2__1_2 = entry(10) * entry(15) - entry(11) * entry(14)
    val minor_1_2__1_3 = entry( 9) * entry(15) - entry(11) * entry(13)
    val minor_1_2__1_4 = entry( 9) * entry(14) - entry(10) * entry(13)
    val minor_1_2__2_3 = entry( 8) * entry(15) - entry(11) * entry(12)
    val minor_1_2__2_4 = entry( 8) * entry(14) - entry(10) * entry(12)
    val minor_1_2__3_4 = entry( 8) * entry(13) - entry( 9) * entry(12)
    
    // 3x3 determinants minor_i_j with row i and column j blocked out.
    val minor_1_1 = entry( 5) * minor_1_2__1_2 - entry( 6) * minor_1_2__1_3 + entry( 7) * minor_1_2__1_4
    val minor_1_2 = entry( 4) * minor_1_2__1_2 - entry( 6) * minor_1_2__2_3 + entry( 7) * minor_1_2__2_4
    val minor_1_3 = entry( 4) * minor_1_2__1_3 - entry( 5) * minor_1_2__2_3 + entry( 7) * minor_1_2__3_4
    val minor_1_4 = entry( 4) * minor_1_2__1_4 - entry( 5) * minor_1_2__2_4 + entry( 6) * minor_1_2__3_4
    
    entry( 0) * minor_1_1 - entry( 1) * minor_1_2 + entry( 2) * minor_1_3 - entry( 3) * minor_1_4
  }
}
