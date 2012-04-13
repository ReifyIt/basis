/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Ring extends LinearVector { self =>
  override type Point = Scalar
  
  override type Vector = Scalar
  
  override type Scalar >: self.type <: Ring {
    type Scalar = self.Scalar
  }
  
  override def Space: ScalarModule {
    type Scalar = self.Scalar
  }
  
  def + (that: Scalar): Scalar
  
  def unary_- : Scalar
  
  def - (that: Scalar): Scalar
  
  def * (that: Scalar): Scalar
  
  def :* (that: Scalar): Scalar = this * that
  
  def *: (that: Scalar): Scalar = that * (this: Scalar)
}
