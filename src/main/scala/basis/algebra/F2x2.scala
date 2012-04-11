/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait F2x2 extends MatrixSpace { self =>
  type Matrix <: MatrixF2x2[Matrix, RowVector, Scalar]
  
  type Transpose = Matrix
  
  type ColumnVector = RowVector
  
  type RowVector <: VectorF2[RowVector, Scalar]
  
  def Transpose: this.type = this
  
  def Column: F2 {
    type Vector = self.ColumnVector
    type Scalar = self.Scalar
  } = Row
  
  def Row: F2 {
    type Vector = self.RowVector
    type Scalar = self.Scalar
  }
  
  final override def dimension: Int = 4
  
  override def zero: Matrix = {
    val z = Scalar.zero
    apply(z, z,
          z, z)
  }
  
  def identity: Matrix = {
    val z = Scalar.zero
    val u = Scalar.unit
    apply(u, z,
          z, u)
  }
  
  def apply(entries: Seq[Scalar]): Matrix = {
    if (entries.length != 4) throw new DimensionException
    apply(entries(0), entries(1),
          entries(2), entries(3))
  }
  
  def apply(_1_1: Scalar, _1_2: Scalar,
            _2_1: Scalar, _2_2: Scalar): Matrix
}
