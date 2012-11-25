/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package mutable

/** A mutable set of elements.
  * 
  * @groupprio  Examining     -5
  * @groupprio  Modifying     -4
  * @groupprio  Iterating     -3
  * @groupprio  Traversing    -2
  * @groupprio  Classifying   -1
  * 
  * @define collection  set
  */
trait Set[A]
  extends Mutable
    with Family[Set[A]]
    with Container[A]
    with traversable.Set[A] {
  
  /** Adds the given element to this $collection.
    * @group Modifying */
  def += (elem: A): this.type
  
  /** Removes the given element from this $collection.
    * @group Modifying */
  def -= (elem: A): this.type
}
