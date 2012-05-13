/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

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
  
  override def apply(entries: TraversableOnce[Real]): Matrix =
    new Matrix(entries.map(_.toDouble).toArray[Double])
  
  override def apply(entries: Array[Double]): Matrix =
    new Matrix(entries)
  
  def apply(entries: Double*): Matrix =
    new Matrix(entries.toArray[Double])
  
  def rows(vectors: Row*): Matrix = rows(vectors)
  
  def cols(vectors: Col*): Matrix = cols(vectors)
  
  override def toString: String = "RMxN"+"("+ Row +", "+ Col +")"
}
