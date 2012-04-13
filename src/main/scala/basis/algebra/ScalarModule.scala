/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait ScalarModule extends LinearModule { self =>
  override type Point = Scalar
  
  override type Vector = Scalar
  
  override type Scalar <: Ring {
    type Scalar = self.Scalar
  }
  
  override def Scalar: this.type = this
  
  override def zero: Scalar
  
  def unit: Scalar
  
  def apply(n: Long): Scalar
  
  def apply(n: Int): Scalar
}
