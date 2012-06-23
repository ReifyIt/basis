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
  * @tparam V   The row space.
  * @tparam W   The column space.
  */
class RMxN[V <: RealVectorSpace with Singleton, W <: RealVectorSpace with Singleton]
    (val Row: V, val Col: W)
  extends RealMatrixSpace[V, W] {
  
  final class Element private[RMxN] (entries: Array[Double]) extends super.Element {
    if (entries.length != M * N) throw new DimensionException
    
    override def apply(k: Int): Real = entries(k)
  }
  
  override type Matrix = Element
  
  override type Transpose <: RMxN[W, V] with Singleton {
    type Transpose = RMxN.this.type
  }
  
  private var _Transpose: RMxN[W, V] = null
  
  override def Transpose: Transpose = synchronized {
    if (_Transpose == null) {
      _Transpose = new RMxN[W, V](Col, Row)
      _Transpose._Transpose = this
    }
    _Transpose.asInstanceOf[Transpose]
  }
  
  override lazy val zero: Matrix = super.zero
  
  override def apply(entries: Array[Double]): Matrix = new Matrix(entries)
  
  override def toString: String = "("+ Col +"⨯"+ Row +")"
}
