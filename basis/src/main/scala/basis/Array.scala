/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

/** An indexed sequence of elements. Import [[basis.collection.ArrayOps]] to
  * extend this interface with a rich suite of optimized collection operations.
  * 
  * @author Chris Sachs
  * 
  * @define collection array
  */
trait Array[+A] extends Any with Seq[A] {
  override type Self <: Array[A]
  
  /** Returns the number of elements in this $collection. */
  def length: Int
  
  /** Returns an indexed element of this $collection. */
  def apply(index: Int): A
  
  override def iterator: Iterator[A] =
    new ArrayIterator(this, 0, length)
  
  override protected def foreach[U](f: A => U) {
    var i = 0
    val n = length
    while (i < n) {
      f(this(i))
      i += 1
    }
  }
}

private[basis] final class ArrayIterator[+A](xs: Array[A], from: Int, until: Int) extends Iterator[A] {
  private[this] var lower: Int = scala.math.max(0, from)
  private[this] var upper: Int = scala.math.min(scala.math.max(lower, until), xs.length)
  private[this] var index: Int = lower
  
  override def isEmpty: Boolean = index >= upper
  
  override def head: A = {
    if (isEmpty) throw new scala.NoSuchElementException("head of empty iterator")
    else xs(index)
  }
  
  override def step() {
    if (isEmpty) throw new java.lang.UnsupportedOperationException("empty iterator step")
    else index += 1
  }
  
  override def dup: ArrayIterator[A] =
    new ArrayIterator[A](xs, index, upper)
}
