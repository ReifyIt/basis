/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Linear extends Any with Affine {
  override type Point = Vector
  
  override type Vector
  
  type Scalar
  
  override def + (that: Vector): Vector
  
  def unary_- : Vector
  
  override def - (that: Vector): Vector
  
  def :* (scalar: Scalar): Vector
  
  def *: (scalar: Scalar): Vector
}

object Linear {
  trait Space extends Affine.Space { self =>
    override type Point = Vector
    
    override type Vector <: Linear {
      type Vector = self.Vector
      type Scalar = self.Scalar
    }
    
    override type Scalar
  }
}
