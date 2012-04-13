/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait AffineModule { self =>
  type Point <: AffinePoint {
    type Point  = self.Point
    type Vector = self.Vector
    type Scalar = self.Scalar
  }
  
  type Vector <: LinearVector {
    type Vector = self.Vector
    type Scalar = self.Scalar
  }
  
  type Scalar <: Ring {
    type Scalar = self.Scalar
  }
  
  def Vector: LinearModule {
    type Vector = self.Vector
    type Scalar = self.Scalar
  }
  
  def Scalar: ScalarModule {
    type Scalar = self.Scalar
  }
}
