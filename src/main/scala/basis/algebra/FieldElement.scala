/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait FieldElement[F <: FieldElement[F]] extends RingElement[F] {
  def Space: Field {
    type Scalar = F
  }
  
  def inverse: F
  
  def / (that: F): F
}
