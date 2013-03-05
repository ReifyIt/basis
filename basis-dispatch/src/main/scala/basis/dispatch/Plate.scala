/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.dispatch

import basis.control._

/** A future result aggregator.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * 
  * @groupprio  Evaluating  1
  */
trait Plate[-A] {
  /** Returns `true` if this plate does not need any more pieces,
    * otherwise returns `false` if this plate is not yet set.
    * @group Evaluating */
  def isSet: Boolean
  
  /** Contributes a piece to this plate.
    * @group Evaluating */
  def put(piece: Try[A]): Unit
  
  /** Contributes a successful piece to this plate.
    * @group Evaluating */
  def putBind(value: A): Unit
  
  /** Contributes a failed piece to this plate.
    * @group Evaluating */
  def putTrap(exception: Throwable): Unit
}
