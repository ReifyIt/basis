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
  /** The type of state maintained by the buffer. */
  type State
  
  /** Adds an element to the buffer. */
  def += (x: A): this.type
  
  /** Prepares the buffer to receive a certain number of elements. */
  def expect(n: Int): this.type
  
  /** Returns the current state of the buffer. */
  def check: State
  
  /** Resets the buffer to its initial state. */
  def clear(): scala.Unit
}
