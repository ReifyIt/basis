/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.dispatch

import basis.control._

/** A future result sender.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * 
  * @groupprio  Evaluating  1
  */
trait Latch[-A] {
  /** Returns `true` if this latch is set with a value or exception,
    * otherwise returns `false` if this latch is not yet set.
    * @group Evaluating */
  def isSet: Boolean
  
  /** Tries to set this latch with a value or exception.
    * 
    * @return `true` if this latch was set with the result, otherwise returns
    *         `false` if this latch was already set.
    * @group  Evaluating */
  def set(result: Try[A]): Boolean
}
