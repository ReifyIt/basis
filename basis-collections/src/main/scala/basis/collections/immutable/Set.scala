/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package immutable

import scala.annotation.unchecked.uncheckedVariance

/** An immutable set of elements.
  * 
  * @groupprio  Examining     -5
  * @groupprio  Iterating     -4
  * @groupprio  Traversing    -3
  * @groupprio  Updating      -2
  * @groupprio  Classifying   -1
  * 
  * @define collection  set
  */
trait Set[+A]
  extends Any
    with Immutable
    with Family[Set[A]]
    with Container[A]
    with general.Set[A] {
  
  /** Returns a copy of this $collection containing the given element.
    * @group Updating */
  def + [B >: A](elem: B): Set[B]
  
  /** Returns a copy of this $collection, excluding the given element.
    * @group Updating */
  def - (elem: A @uncheckedVariance): Set[A]
}
