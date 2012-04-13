/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Field extends Ring { self =>
  override type Scalar >: self.type <: Field {
    type Scalar = self.Scalar
  }
  
  override def Space: ScalarSpace {
    type Scalar = self.Scalar
  }
  
  def + (that: Scalar): Scalar
  
  def unary_- : Scalar
  
  def - (that: Scalar): Scalar
  
  def * (that: Scalar): Scalar
  
  def inverse: Scalar
  
  def / (that: Scalar): Scalar
}
