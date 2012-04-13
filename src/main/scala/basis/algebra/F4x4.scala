/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait F4x4 extends LinearSpace with FMxN { self =>
  override type Matrix <: MatrixF4x4 {
    type Matrix    = self.Matrix
    type RowVector = self.RowVector
    type Scalar    = self.Scalar
  }
  
  override type Transpose = Matrix
  
  override type RowVector <: VectorF4 {
    type Vector = self.RowVector
    type Scalar = self.Scalar
  }
  
  override type ColumnVector = RowVector
  
  override def Transpose: this.type = this
  
  override def Row: F4 {
    type Vector = self.RowVector
    type Scalar = self.Scalar
  }
  
  override def Column: F4 {
    type Vector = self.ColumnVector
    type Scalar = self.Scalar
  } = Row
  
  final override def dimension: Int = 16
  
  override def zero: Matrix = {
    val z = Scalar.zero
    apply(z, z, z, z,
          z, z, z, z,
          z, z, z, z,
          z, z, z, z)
  }
  
  def identity: Matrix = {
    val z = Scalar.zero
    val u = Scalar.unit
    apply(u, z, z, z,
          z, u, z, z,
          z, z, u, z,
          z, z, z, u)
  }
  
  override def apply(entries: Seq[Scalar]): Matrix = {
    if (entries.length != 16) throw new DimensionException
    apply(entries( 0), entries( 1), entries( 2), entries( 3),
          entries( 4), entries( 5), entries( 6), entries( 7),
          entries( 8), entries( 9), entries(10), entries(11),
          entries(12), entries(13), entries(14), entries(15))
  }
  
  def apply(_1_1: Scalar, _1_2: Scalar, _1_3: Scalar, _1_4: Scalar,
            _2_1: Scalar, _2_2: Scalar, _2_3: Scalar, _2_4: Scalar,
            _3_1: Scalar, _3_2: Scalar, _3_3: Scalar, _3_4: Scalar,
            _4_1: Scalar, _4_2: Scalar, _4_3: Scalar, _4_4: Scalar): Matrix
}
