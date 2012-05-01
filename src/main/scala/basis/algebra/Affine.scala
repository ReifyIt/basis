/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Affine extends Any {
  type Point
  
  type Vector
  
  def + (that: Vector): Point
  
  def - (that: Point): Vector
}

object Affine {
  trait Space { self =>
    type Point <: Affine {
      type Point  = self.Point
      type Vector = self.Vector
    }
    
    type Vector <: Linear {
      type Vector = self.Vector
      type Scalar = self.Scalar
    }
    
    type Scalar
  }
}
