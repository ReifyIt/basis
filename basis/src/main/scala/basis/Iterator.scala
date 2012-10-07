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
  
  /** Returns `true` if a subsequent call to `next()` will yield an element. */
  def hasNext: Boolean
  
  /** Returns the next element and advances the iterator's state. `next()`
    * calls should only succeed `true` tests of `hasNext`. */
  def next(): A
  
  protected override def foreach[U](f: A => U): Unit =
    while (hasNext) f(next())
}
