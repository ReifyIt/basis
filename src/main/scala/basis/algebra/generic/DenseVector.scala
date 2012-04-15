/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

final class DenseVector[S <: Ring { type Scalar = S }] private[generic]
    (val Space: DenseVectorModule[S] with Singleton)
    (coords: Array[AnyRef])
  extends VectorFN {
  
  type Space  = DenseVectorModule[S] with Singleton
  type Vector = DenseVector[S]
  type Scalar = S
  
  def coord(i: Int): Scalar = coords(i).asInstanceOf[Scalar]
}
