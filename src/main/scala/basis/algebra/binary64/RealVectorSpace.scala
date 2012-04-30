/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

trait RealVectorSpace extends VectorSpace { self =>
  override type Vector <: RealVector {
    type Vector = self.Vector
  }
  
  override type Scalar = Real
  
  override def N: Int
  
  override def apply(coords: TraversableOnce[Scalar]): Vector
  
  def apply(coords: Array[Double]): Vector
  
  override def map[W <: basis.algebra.Vector { type Vector = W; type Scalar = Real }]
      (that: VectorSpace { type Vector = W; type Scalar = Real })
    : MatrixSpace { type Row = self.Vector; type Col = W; type Scalar = Real } = {
    if (that.isInstanceOf[RealMatrix]) {
      type SomeRealVector[V] = RealVector { type Vector = V }
      this.map(that.asInstanceOf[RealVectorSpace { type Vector <: SomeRealVector[Vector] }]).
        asInstanceOf[MatrixSpace { type Row = self.Vector; type Col = W; type Scalar = Real}]
    }
    else super.map(that)
  }
  
  def map[W <: RealVector { type Vector = W }]
      (that: RealVectorSpace { type Vector = W })
    : RealMatrixSpace { type Row = self.Vector; type Col = W } =
    new RMxN(this, that)
}
