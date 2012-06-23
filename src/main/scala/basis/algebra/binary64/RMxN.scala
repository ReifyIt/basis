/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

/** A space of ''M''x''N'' real matrices.
  * 
  * @author Chris Sachs
  * 
  * @tparam V   The row space of this $space.
  * @tparam W   The column space of this $space.
  */
class RMxN[V <: RealVectorSpace with Singleton, W <: RealVectorSpace with Singleton]
    (val Row: V, val Col: W)
  extends RealMatrixSpace[V, W] {
  
  final class Element private[RMxN] (entries: Array[Double]) extends super.Element {
    if (entries.length != M * N) throw new DimensionException
    
    override def apply(k: Int): Real = entries(k)
  }
  
  override type Matrix = Element
  
  override lazy val Transpose = new RMxN[W, V](Col, Row)
  
  override lazy val zero: Matrix = super.zero
  
  override def apply(entries: Array[Double]): Matrix = new Matrix(entries)
  
  override def toString: String = "("+ Col +"⨯"+ Row +")"
}
