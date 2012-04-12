/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait AffineModule { self =>
  type Point <: AffinePoint[Point, Vector, Scalar]
  
  type Vector <: VectorElement[Vector, Scalar]
  
  type Scalar <: RingElement[Scalar]
  
  def Vector: VectorModule {
    type Vector = self.Vector
    type Scalar = self.Scalar
  }
  
  def Scalar: Ring {
    type Scalar = self.Scalar
  }
}
