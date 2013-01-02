/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._

/** A list-convertible collection.
  * 
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  * 
  * @groupprio  Converting  1
  * 
  * @define collection  collection
  */
trait ListLike[+A] extends Any { this: Enumerator[A] =>
  /** Returns a list containing the elements of this $collection.
    * @group Converting */
  def toList: List[A]
}
