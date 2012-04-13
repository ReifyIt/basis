/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait ScalarModule extends VectorModule {
  type Vector = Scalar
  
  type Scalar <: Ring[Scalar]
  
  def Scalar: this.type = this
  
  def unit: Scalar
  
  def apply(n: Long): Scalar
  
  def apply(n: Int): Scalar
}
