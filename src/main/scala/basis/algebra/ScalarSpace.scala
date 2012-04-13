/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait ScalarSpace extends ScalarModule with LinearSpace { self =>
  override type Scalar <: Field {
    type Scalar = self.Scalar
  }
  
  def apply(x: Double): Scalar
  
  def apply(x: Float): Scalar
}
