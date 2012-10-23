/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

/** A stateful traverser of elements. An iterator steps through each element
  * of a collection, one element per `step()` until `isEmpty` returns `true`.
  * Backtracking algorithms can `dup` an iterator's state and resume it after
  * mutating the original.
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
  * @groupprio  State       -4
  * @groupprio  Traversing  -3
  * @groupprio  Reducing    -2
  * @groupprio  Querying    -1
  * 
  * @define collection  iterator
  */
trait Iterator[+A] extends Any with Enumerator[A] {
  override type Self <: Iterator[A]
  
  /** Returns `true` when this $collection has reached a sentinel element.
    * @group State */
  def isDone: Boolean = false
  
  /** Returns `true` when this $collection has no more elements.
    * @group State */
  def isEmpty: Boolean
  
  /** Returns the currently buffered element.
    * @group State */
  def head: A
  
  /** Advances this $collection to its next state.
    * @group State */
  def step(): Unit
  
  /** Returns a duplicate $collection with identical but independent state.
    * @group State */
  def dup: Iterator[A]
  
  protected override def foreach[U](f: A => U): Unit =
    while (!isEmpty) { f(head); step() }
}

/** A terminated iterator. */
object Done extends Iterator[Nothing] {
  override def isDone: Boolean = true
  
  override def isEmpty: Boolean = true
  
  override def head: Nothing =
    throw new scala.NoSuchElementException("Head of empty iterator.")
  
  override def step(): Unit =
    throw new java.lang.UnsupportedOperationException("Empty iterator step.")
  
  override def dup: Done.type = this
  
  protected override def foreach[U](f: Nothing => U): Unit = ()
}
