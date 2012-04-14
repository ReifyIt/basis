/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

trait F3x3 extends LinearSpace with FMxN { self =>
  override type Matrix <: MatrixF3x3 {
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
  
  override def Transpose: this.type = this
  
  override def Row: F3 {
    type Vector = self.RowVector
    type Scalar = self.Scalar
  }
  
  override def Column: F3 {
    type Vector = self.ColumnVector
    type Scalar = self.Scalar
  } = Row
  
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
  
  override def apply(entries: TraversableOnce[Scalar]): Matrix = {
    val xs = entries.toSeq
    if (xs.length != 9) throw new DimensionException
    apply(xs(0), xs(1), xs(2),
          xs(3), xs(4), xs(5),
          xs(6), xs(7), xs(8))
  }
  
  def apply(_1_1: Scalar, _1_2: Scalar, _1_3: Scalar,
            _2_1: Scalar, _2_2: Scalar, _2_3: Scalar,
            _3_1: Scalar, _3_2: Scalar, _3_3: Scalar): Matrix
}
