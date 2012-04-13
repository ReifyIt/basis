/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait MatrixF2x2 extends MatrixFMxN { self =>
  override type Matrix >: self.type <: MatrixF2x2 {
    type Matrix    = self.Matrix
    type RowVector = self.RowVector
    type Scalar    = self.Scalar
  }
  
  override type Transpose = Matrix
  
  override type RowVector <: VectorF2 {
    type Vector = self.RowVector
    type Scalar = self.Scalar
  }
  
  override type ColumnVector = RowVector
  
  override type Scalar <: Field {
    type Scalar = self.Scalar
  }
  
  override def Space: F2x2 {
    type Matrix    = self.Matrix
    type RowVector = self.RowVector
    type Scalar    = self.Scalar
  }
  
  final override def M: Int = 2
  
  final override def N: Int = 2
  
  def row1: RowVector = Space.Row(entry(0), entry(1))
  
  def row2: RowVector = Space.Row(entry(2), entry(3))
  
  def column1: ColumnVector = Space.Column(entry(0), entry(2))
  
  def column2: ColumnVector = Space.Column(entry(1), entry(3))
  
  override def + (that: Matrix): Matrix =
    Space(entry(0) + that.entry(0), entry(1) + that.entry(1),
          entry(2) + that.entry(2), entry(3) + that.entry(3))
  
  override def unary_- : Matrix =
    Space(-entry(0), -entry(1),
          -entry(2), -entry(3))
  
  override def - (that: Matrix): Matrix =
    Space(entry(0) - that.entry(0), entry(1) - that.entry(1),
          entry(2) - that.entry(2), entry(3) - that.entry(3))
  
  override def :* (scalar: Scalar): Matrix =
    Space(entry(0) * scalar, entry(1) * scalar,
          entry(2) * scalar, entry(3) * scalar)
  
  override def *: (scalar: Scalar): Matrix =
    Space(scalar * entry(0), scalar * entry(1),
          scalar * entry(2), scalar * entry(3))
  
  override def :* (vector: RowVector): ColumnVector =
    Space.Column(entry(0) * vector.coord(0) + entry(1) * vector.coord(1),
                 entry(2) * vector.coord(0) + entry(3) * vector.coord(1))
  
  override def *: (vector: ColumnVector): RowVector =
    Space.Row(vector.coord(0) * entry(0) + vector.coord(1) * entry(2),
              vector.coord(0) * entry(1) + vector.coord(1) * entry(3))
  
  def * (that: Matrix): Matrix =
    Space(entry(0) * that.entry(0) + entry(1) * that.entry(2),
          entry(0) * that.entry(1) + entry(1) * that.entry(3),
          entry(2) * that.entry(0) + entry(3) * that.entry(2),
          entry(2) * that.entry(1) + entry(3) * that.entry(3))
  
  def inverse: Option[Matrix] = {
    val det = determinant
    if (det != Space.Scalar.zero)
      Some(Space(entry(3) / det, -entry(1) / det,
                -entry(2) / det,  entry(0) / det))
    else None
  }
  
  override def transpose: Transpose =
    Space.Transpose(entry(0), entry(2),  entry(1), entry(3))
  
  def determinant: Scalar =
    entry(0) * entry(3) - entry(1) * entry(2)
}
