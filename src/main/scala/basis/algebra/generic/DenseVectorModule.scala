/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

class DenseVectorModule[S <: Ring { type Scalar = S }]
    (val Scalar: ScalarModule { type Scalar = S })
    (val dimension: Int)
  extends FN {
  
  type Vector = DenseVector[S]
  type Scalar = S
  
  def apply(coords: TraversableOnce[Scalar]): Vector =
    new DenseVector[S](this)(coords.toArray[AnyRef])
  
  def apply(coords: Scalar*): Vector =
    new DenseVector[S](this)(coords.toArray[AnyRef])
  
  override def toString: String =
    "FN"+"("+ Scalar +")"+"("+ dimension +")"
}
