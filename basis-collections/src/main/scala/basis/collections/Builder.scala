/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

import scala.annotation.implicitNotFound

/** A stateful element accumulator.
  * 
  * @groupprio  Examining   -3
  * @groupprio  Inserting   -2
  * @groupprio  Removing    -1
  * 
  * @define collection  builder
  */
@implicitNotFound("No builder available from ${From} for element type ${A}.")
trait Builder[-From, -A] extends Accumulator[A] {
  /** The type of state maintained by this $collection.
    * @group Examining */
  type State
  
  /** Returns the current state of this $collection.
    * @group Examining */
  def state: State
  
  /** Removes all elements from this $collection.
    * @group Removing */
  def clear(): Unit
}
