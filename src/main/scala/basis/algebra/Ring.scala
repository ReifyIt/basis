/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Ring extends Any {
  type Vector
  
  def + (that: Vector): Vector
  
  def unary_- : Vector
  
  def - (that: Vector): Vector
  
  def * (that: Vector): Vector
}

object Ring {
  trait Space { self =>
    type Vector <: Ring {
      type Vector = self.Vector
    }
    
    def zero: Vector
    
    def unit: Vector
  }
  
  trait Scalar { self =>
    type Scalar <: Ring {
      type Vector = self.Scalar
    }
    
    def Scalar: Ring.Space {
      type Vector = self.Scalar
    }
  }
}
