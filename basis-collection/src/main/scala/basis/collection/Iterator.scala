/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

/** A stateful traverser of elements. An iterator steps through each element
  * of a collection, one element per `step()` until `isEmpty` returns `true`.
  * Backtracking algorithms can `dup` an iterator's state and resume it after
  * mutating the original.
  * 
  * Import [[basis.collection.IteratorOps]] to extend this interface with
  * a rich suite of optimized collection operations.
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
  * @author Chris Sachs
  * 
  * @define collection  iterator
  */
trait Iterator[+A] extends Any with Enumerator[A] {
  override type Self <: Iterator[A]
  
  /** Returns `true` when this $collection has reached a sentinel element. */
  def isDone: Boolean = false
  
  /** Returns `true` when this $collection has no more elements. */
  def isEmpty: Boolean
  
  /** Returns the currently buffered element. */
  def head: A
  
  /** Advances this $collection to its next state. */
  def step(): Unit
  
  /** Returns a duplicate $collection with identical but independent state. */
  def dup: Iterator[A]
  
  protected override def foreach[U](f: A => U): Unit =
    while (!isEmpty) { f(head); step() }
}

/** Singular iterators. */
object Iterator {
  object empty extends Iterator[Nothing] {
    override def isDone: Boolean = false
    
    override def isEmpty: Boolean = true
    
    override def head: Nothing =
      throw new scala.NoSuchElementException("Empty iterator has no element.")
    
    override def step(): Unit =
      throw new java.lang.UnsupportedOperationException("Can't advance empty iterator.")
    
    override def dup: empty.type = this
    
    protected override def foreach[U](f: Nothing => U): Unit = ()
    
    override def toString: String = "empty"
  }
  
  object done extends Iterator[Nothing] {
    override def isDone: Boolean = true
    
    override def isEmpty: Boolean = true
    
    override def head: Nothing =
      throw new scala.NoSuchElementException("Done iterator has no element.")
    
    override def step(): Unit =
      throw new java.lang.UnsupportedOperationException("Can't advance done iterator.")
    
    override def dup: done.type = this
    
    protected override def foreach[U](f: Nothing => U): Unit = ()
    
    override def toString: String = "done"
  }
}
