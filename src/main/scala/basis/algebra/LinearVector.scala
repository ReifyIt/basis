/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait LinearVector extends AffinePoint { self =>
  override type Space <: LinearModule with Singleton {
    type Vector = self.Vector
    type Scalar = self.Scalar
  }
  
  override type Point = Vector
  
  override type Vector >: self.type <: LinearVector {
    type Vector = self.Vector
    type Scalar = self.Scalar
  }
  
  def + (that: Vector): Vector
  
  def unary_- : Vector
  
  def - (that: Vector): Vector
  
  def :* (scalar: Scalar): Vector
  
  def *: (scalar: Scalar): Vector
}