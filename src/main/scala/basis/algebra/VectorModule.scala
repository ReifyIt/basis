/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait VectorModule extends AffineModule { self =>
  type Point = Vector
  
  type Vector <: VectorElement[Vector, Scalar]
  
  type Scalar <: RingElement[Scalar]
  
  def Vector: this.type = this
  
  def zero: Vector
}
