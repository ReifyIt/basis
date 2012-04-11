/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait FreeModule { self =>
  type Vector <: VectorElement[Vector, Scalar]
  
  type Scalar <: RingElement[Scalar]
  
  def Scalar: Ring {
    type Scalar = self.Scalar
  }
  
  def zero: Vector
}
