/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** An element of a commutative ring with a total order.
  * 
  * $IntMorphismInfo
  * 
  * @author Chris Sachs
  * 
  * @tparam OrderedRing   the element type of the ordered ring.
  * 
  * @define Element   OrderedRing
  * @define element   `OrderedRing` value
  */
trait OrderedRing[OrderedRing] extends Ordered[OrderedRing] with Ring[OrderedRing] {
  /** Returns the absolute value of this $element. */
  def abs: OrderedRing
}
