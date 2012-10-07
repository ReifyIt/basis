/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

/** A linear sequence of elements. Import [[basis.collection.ListOps]] to
  * extend this interface with a rich suite of optimized collection operations.
  * 
  * @author Chris Sachs
  * 
  * @define collection  list
  */
trait List[+A] extends Any with Seq[A] {
  override type Self <: List[A]
  
  /** Returns `true` if this $collection has no `head`. */
  def isEmpty: Boolean
  
  /** Returns the first element of this $collection. */
  def head: A
  
  /** Returns all but the first element of this $collection. */
  def tail: List[A]
  
  override def iterator: Iterator[A] = new ListIterator[A](this)
  
  override protected def foreach[U](f: A => U) {
    var xs = this
    while (!xs.isEmpty) {
      f(xs.head)
      xs = xs.tail
    }
  }
}

private[basis] final class ListIterator[+A](private[this] var xs: List[A]) extends Iterator[A] {
  override def hasNext: Boolean = !xs.isEmpty
  
  override def next(): A = {
    if (xs.isEmpty) throw new scala.NoSuchElementException("empty iterator")
    val x = xs.head
    xs = xs.tail
    x
  }
}
