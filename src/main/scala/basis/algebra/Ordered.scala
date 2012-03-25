/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** An element of a totally ordered set.
  * 
  * @author Chris Sachs
  * 
  * @tparam Ordered   the element type of the totally ordered set.
  * 
  * @define element   element
  */
trait Ordered[Ordered] {
  /** Returns `true` if this $element is less than the other $element. */
  def < (that: Ordered): Boolean
  
  /** Returns `true` if this $element is less than or equal to the other $element. */
  def <= (that: Ordered): Boolean
  
  /** Returns `true` if this $element is greater than or equal to the other $element. */
  def >= (that: Ordered): Boolean
  
  /** Returns `true` if this $element is greater than the other $element. */
  def > (that: Ordered): Boolean
  
  /** Returns the minimum of this $element and the other $element. */
  def min(that: Ordered): Ordered
  
  /** Returns the maximum of this $element and the other $element. */
  def max(that: Ordered): Ordered
}
