/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

/** A stateful traverser of elements. Import [[basis.collection.IteratorOps]] to
  * extend this interface with a rich suite of optimized collection operations.
  * 
  * @author Chris Sachs
  * 
  * @define collection  iterator
  */
trait Iterator[+A] extends Any with Enumerator[A] {
  override type Self <: Iterator[A]
  
  /** Returns `true` if this $collection has no more elements. */
  def isEmpty: Boolean
  
  /** Returns the current element of this $collection. */
  def head: A
  
  /** Advances this $collection to the next element. */
  def step(): Unit
  
  /** Returns a duplicate $collection with identical but independent state. */
  def dup: Iterator[A]
  
  protected override def foreach[U](f: A => U): Unit =
    while (!isEmpty) { f(head); step() }
}

/** Iterator utilities. */
object Iterator {
  object empty extends Iterator[Nothing] {
    override def isEmpty: Boolean = true
    
    override def head: Nothing =
      throw new scala.NoSuchElementException("head of empty iterator")
    
    override def step(): Unit =
      throw new java.lang.UnsupportedOperationException("empty iterator step")
    
    override def dup: Iterator.empty.type = this
    
    protected override def foreach[U](f: Nothing => U): Unit = ()
  }
}
