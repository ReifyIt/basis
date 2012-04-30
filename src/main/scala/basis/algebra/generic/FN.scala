/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

class FN[F <: Ring { type Vector = F }](val N: Int) extends VectorSpace {
  override type Vector = VectorFN[F]
  override type Scalar = F
  
  override def apply(coords: TraversableOnce[Scalar]): Vector =
    new Vector(this, coords.asInstanceOf[TraversableOnce[AnyRef]].toArray[AnyRef])
  
  def apply(coords: Scalar*): Vector =
    new Vector(this, coords.asInstanceOf[Seq[AnyRef]].toArray[AnyRef])
  
  override def toString: String = "F"+"("+ N + ")"
}
