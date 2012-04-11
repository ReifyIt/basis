/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait F3x3 extends VectorSpace with FMxN { self =>
  type Matrix <: MatrixF3x3[Matrix, RowVector, Scalar]
  
  type Transpose = Matrix
  
  type ColumnVector = RowVector
  
  type RowVector <: VectorF3[RowVector, Scalar]
  
  def Transpose: this.type = this
  
  def Column: F3 {
    type Vector = self.ColumnVector
    type Scalar = self.Scalar
  } = Row
  
  def Row: F3 {
    type Vector = self.RowVector
    type Scalar = self.Scalar
  }
  
  final override def dimension: Int = 9
  
  override def zero: Matrix = {
    val z = Scalar.zero
    apply(z, z, z,
          z, z, z,
          z, z, z)
  }
  
  def identity: Matrix = {
    val z = Scalar.zero
    val u = Scalar.unit
    apply(u, z, z,
          z, u, z,
          z, z, u)
  }
  
  def apply(entries: Seq[Scalar]): Matrix = {
    if (entries.length != 9) throw new DimensionException
    apply(entries(0), entries(1), entries(2),
          entries(3), entries(4), entries(5),
          entries(6), entries(7), entries(8))
  }
  
  def apply(_1_1: Scalar, _1_2: Scalar, _1_3: Scalar,
            _2_1: Scalar, _2_2: Scalar, _2_3: Scalar,
            _3_1: Scalar, _3_2: Scalar, _3_3: Scalar): Matrix
}
