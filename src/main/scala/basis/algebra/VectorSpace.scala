/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait VectorSpace extends LinearSpace { self =>
  override type Vector <: basis.algebra.Vector {
    type Vector = self.Vector
    type Scalar = self.Scalar
  }
  
  override type Scalar <: Ring {
    type Vector = self.Scalar
  }
  
  def N: Int
  
  def apply(coords: TraversableOnce[Scalar]): Vector
  
  def map[W <: basis.algebra.Vector { type Vector = W; type Scalar = self.Scalar }]
      (that: VectorSpace { type Vector = W; type Scalar = self.Scalar })
    : MatrixSpace { type Row = self.Vector; type Col = W; type Scalar = self.Scalar } =
    new generic.FMxN(this, that)
}
