/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package general

/** A stateful traverser of elements. An iterator steps through each element
  * of a collection, one element per `step()`, until `isEmpty` returns `true`.
  * Backtracking algorithms can `dup` an iterator's state and resume it after
  * mutating the original iterator.
  * 
  * == Iterator states ==
  *
  * Each `step()` forwards the iterator into one of three states:
  * ''buffered'', ''empty'', or ''done''.
  * 
  *  - In the ''buffered'' state, `head` returns the current element,
  *    and both `isEmpty` and `isDone` return `false`.
  *  - In the ''empty'' state, `head` is undefined, `isEmpty` returns `true`,
  *    and `isDone` returns `false`.
  *  - In the ''done'' state, like the ''empty'' state, `head` is undefined,
  *    and both `isEmpty` and `isDone` return `true`.
  * 
  * The distinct ''empty'' and ''done'' states facilitate low-overhead
  * "chunked" iterator applications such as iteratees.
  * 
  * @groupprio  Examining     -4
  * @groupprio  Iterating     -3
  * @groupprio  Traversing    -2
  * @groupprio  Classifying   -1
  * 
  * @define collection  iterator
  */
trait Iterator[@specialized(Byte, Short, Int, Long, Float, Double, Boolean) +A]
  extends Any with Family[Iterator[A]] with Enumerator[A] {
  
  /** Returns `true` when this $collection has reached a sentinel element.
    * @group Examining */
  def isDone: Boolean = false
  
  /** Returns `true` when this $collection has no more elements.
    * @group Examining */
  def isEmpty: Boolean
  
  /** Returns the currently buffered element.
    * @group Examining */
  def head: A
  
  /** Advances this $collection to its next state.
    * @group Iterating */
  def step(): Unit
  
  /** Returns a duplicate $collection with identical but independent state.
    * @group Iterating */
  def dup: Iterator[A]
  
  protected override def foreach[U](f: A => U): Unit =
    while (!isEmpty) { f(head); step() }
}

/** An empty iterator. */
object Done extends Iterator[Nothing] {
  override def isDone: Boolean = true
  
  override def isEmpty: Boolean = true
  
  override def head: Nothing =
    throw new NoSuchElementException("Head of empty iterator.")
  
  override def step(): Unit =
    throw new UnsupportedOperationException("Empty iterator step.")
  
  override def dup: Done.type = this
  
  protected override def foreach[U](f: Nothing => U): Unit = ()
}
