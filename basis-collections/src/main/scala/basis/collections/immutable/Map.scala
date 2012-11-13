/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package immutable

import scala.annotation.unchecked.uncheckedVariance

/** An immutable set of (key, value) pairs with unique keys.
  * 
  * @groupprio  Examining     -5
  * @groupprio  Iterating     -4
  * @groupprio  Traversing    -3
  * @groupprio  Updating      -2
  * @groupprio  Classifying   -1
  * 
  * @define collection  map
  */
trait Map[+A, +T]
  extends Any
    with Immutable
    with Family[Map[A, T]]
    with Container[(A, T)]
    with general.Map[A, T] {
  
  /** Returns a copy of this $collection with the given value associated with the given key.
    * @group Updating */
  def + [B >: A, U >: T](key: B, value: U): Map[B, U]
  
  /** Returns a copy of this $collection without any value associated with the given key.
    * @group Updating */
  def - (key: A @uncheckedVariance): Map[A, T]
}
