/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait OrderedRing extends Ring { self =>
  override type Scalar >: self.type <: OrderedRing {
    type Scalar = self.Scalar
  }
  
  def abs: Scalar
  
  def min(that: Scalar): Scalar
  
  def max(that: Scalar): Scalar
  
  def < (that: Scalar): Boolean
  
  def <= (that: Scalar): Boolean
  
  def >= (that: Scalar): Boolean
  
  def > (that: Scalar): Boolean
}
