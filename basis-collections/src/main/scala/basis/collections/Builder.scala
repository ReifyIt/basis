/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

/** A stateful element accumulator.
  * 
  * @groupprio  Examining   -3
  * @groupprio  Inserting   -2
  * @groupprio  Removing    -1
  * 
  * @define collection  builder
  */
trait Builder[-From, -A] extends Accumulator[A] {
  /** The type of state maintained by this $collection.
    * @group Examining */
  type State
  
  /** Resets this $collection to its initial state.
    * @group Removing */
  def clear(): Unit
  
  /** Prepares this $collection to receive a certain number of elements.
    * @group Inserting */
  def expect(count: Int): this.type
  
  /** Returns the current state of this $collection.
    * @group Examining */
  def state: State
}
