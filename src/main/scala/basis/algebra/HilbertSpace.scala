/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait HilbertSpace
  extends InnerProductSpace
    with NormedVectorSpace
    with MetricSpace {
  
  type Scalar <: CompleteField[Scalar]
  
  def norm(u: Vector): Scalar = innerProduct(u, u).sqrt
  
  def distance(u: Vector, v: Vector): Scalar = norm(u - v)
}