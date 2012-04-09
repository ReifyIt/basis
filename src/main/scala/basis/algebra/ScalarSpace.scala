/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait ScalarSpace[F <: Field[F]] extends VectorSpace {
  type Scalar = F
  
  type Vector = F
  
  val Scalar: ScalarSpace[F] = this
  
  def zero: F
  
  def unit: F
}
