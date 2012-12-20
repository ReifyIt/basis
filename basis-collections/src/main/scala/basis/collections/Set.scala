/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

import scala.annotation.unchecked.uncheckedVariance

/** A unique set of elements.
  * 
  * @groupprio  Quantifying   -5
  * @groupprio  Querying      -4
  * @groupprio  Iterating     -3
  * @groupprio  Traversing    -2
  * @groupprio  Classifying   -1
  * 
  * @define collection  set
  */
trait Set[+A] extends Any with Family[Set[A]] with Container[A] {
  /** Returns `true` if this $collection doesn't contain any elements.
    * @group Quantifying */
  def isEmpty: Boolean = iterator.isEmpty
  
  /** Returns the number of elements in this $collection.
    * @group Quantifying */
  def size: Int = {
    var count = 0
    val these = iterator
    while (!these.isEmpty) {
      count += 1
      these.step()
    }
    count
  }
  
  /** Returns `true` if this $collection contains the given element.
    * @group Querying */
  def contains(element: A @uncheckedVariance): Boolean = {
    val these = iterator
    while (!these.isEmpty) {
      if (element == these.head) return true
      these.step()
    }
    false
  }
}
