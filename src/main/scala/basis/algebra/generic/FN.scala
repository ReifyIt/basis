/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

class FN[S <: Ring with Singleton](val Scalar: S)(val N: Int) extends VectorSpace[S] {
  final class Element private[FN] (coords: Array[AnyRef]) extends super.Element {
    if (coords.length != Vector.N) throw new DimensionException
    
    override def N: Int = coords.length
    
    override def apply(i: Int): Scalar = coords(i).asInstanceOf[Scalar]
  }
  
  override type Vector = Element
  
  override lazy val zero: Vector = super.zero
  
  override def apply(coords: TraversableOnce[Scalar]): Vector =
    new Vector(coords.asInstanceOf[TraversableOnce[AnyRef]].toArray[AnyRef])
  
  def apply(coords: Scalar*): Vector =
    new Vector(coords.asInstanceOf[Seq[AnyRef]].toArray[AnyRef])
  
  override def toString: String = "FN"+"("+ Scalar +")"+"("+ N +")"
}
