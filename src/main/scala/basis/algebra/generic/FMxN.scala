/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

/** A generic space of ''M''x''N'' matrices over a ring.
  * 
  * @author Chris Sachs
  * 
  * @tparam V   The row space.
  * @tparam W   The column space.
  * @tparam S   The set of scalars.
  */
class FMxN[V <: VectorSpace[S] with Singleton, W <: VectorSpace[S] with Singleton, S <: Ring with Singleton]
    (val Scalar: S)(val Row: V, val Col: W)
  extends MatrixSpace[V, W, S] {
  
  final class Element private[FMxN] (entries: Array[AnyRef]) extends super.Element {
    if (entries.length != M * N) throw new DimensionException
    
    override def apply(k: Int): Scalar = entries(k).asInstanceOf[Scalar]
  }
  
  override type Matrix = Element
  
  override type Transpose <: FMxN[W, V, S] with Singleton {
    type Transpose = FMxN.this.type
  }
  
  private var _Transpose: FMxN[W, V, S] = null
  
  override def Transpose: Transpose = synchronized {
    if (_Transpose == null) {
      _Transpose = new FMxN[W, V, S](Scalar)(Col, Row)
      _Transpose._Transpose = this
    }
    _Transpose.asInstanceOf[Transpose]
  }
  
  override lazy val zero: Matrix = super.zero
  
  override def apply(entries: Scalar*): Matrix =
    new Matrix(entries.asInstanceOf[Seq[AnyRef]].toArray[AnyRef])
  
  override def toString: String = "FMxN"+"("+ Scalar +")"+"("+ Row +", "+ Col +")"
}
