/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

class RN(val N: Int) extends RealVectorSpace {
  final class Element private[RN] (coords: Array[Double]) extends super.Element {
    if (coords.length != Vector.N) throw new DimensionException
    
    override def N: Int = coords.length
    
    override def apply(i: Int): Real = coords(i)
  }
  
  override type Vector = Element
  
  override lazy val zero: Vector = super.zero
  
  override def apply(coords: TraversableOnce[Real]): Vector =
    new Vector(coords.map(_.toDouble).toArray[Double])
  
  override def apply(coords: Array[Double]): Vector =
    new Vector(coords)
  
  def apply(coords: Double*): Vector =
    new Vector(coords.toArray[Double])
  
  override def toString: String = "RN"+"("+ N + ")"
}
