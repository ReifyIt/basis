/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

import scala.annotation.implicitNotFound

/** A collection builder.
  * 
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  * 
  * @groupprio  Examining   1
  * @groupprio  Inserting   2
  * @groupprio  Removing    3
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
