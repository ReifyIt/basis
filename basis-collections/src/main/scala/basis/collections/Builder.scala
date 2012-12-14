/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

/** An element accumulator.
  * 
  * @define builder   builder
  */
trait Builder[-From, @specialized(Specializable.Primitives) -A] {
  /** The type of state maintained by this $builder. */
  type State
  
  /** Adds a single element to this $builder. */
  def += (x: A): this.type
  
  /** Adds multiple elements to this $builder. */
  def ++= (xs: Enumerator[A]): this.type
  
  /** Resets this $builder to its initial state. */
  def clear(): Unit
  
  /** Prepares this $builder to receive a certain number of elements. */
  def expect(count: Int): this.type
  
  /** Returns the current state of this $builder. */
  def state: State
}
