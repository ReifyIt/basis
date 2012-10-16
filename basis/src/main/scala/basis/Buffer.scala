/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

/** A temporary buffer of elements.
  * 
  * @author Chris Sachs
  */
trait Buffer[-Source, -A] {
  /** The type of state maintained by this buffer. */
  type State
  
  /** Adds an element to this buffer. */
  def += (x: A): this.type
  
  /** Prepares this buffer to receive a certain number of elements. */
  def expect(count: Int): this.type
  
  /** Returns the current state of this buffer. */
  def state: State
  
  /** Resets this buffer to its initial state. */
  def clear(): Unit
}
