/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait AffineSpace extends AffineModule { self =>
  type Scalar <: FieldElement[Scalar]
  
  def Vector: VectorSpace {
    type Vector = self.Vector
    type Scalar = self.Scalar
  }
  
  def Scalar: Field {
    type Scalar = self.Scalar
  }
}
