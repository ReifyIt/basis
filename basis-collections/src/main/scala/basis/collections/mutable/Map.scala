/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package mutable

/** A mutable set of (key, value) pairs with unique keys.
  * 
  * @groupprio  Examining     -5
  * @groupprio  Modifying     -4
  * @groupprio  Iterating     -3
  * @groupprio  Traversing    -2
  * @groupprio  Classifying   -1
  * 
  * @define collection  map
  */
trait Map[A, T]
  extends Any
    with Mutable
    with Family[Map[A, T]]
    with Container[(A, T)]
    with traversable.Map[A, T] {
  
  /** Adds an association from the given value to the given key.
    * @group Modifying */
  def += (key: A, value: T): Unit
  
  /** Removes any association with the given key.
    * @group Modifying */
  def -= (key: A): Unit
}
