/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package traversable

import basis.util._

/** An indexed sequence of elements.
  * 
  * @groupprio  Examining     -4
  * @groupprio  Iterating     -3
  * @groupprio  Traversing    -2
  * @groupprio  Classifying   -1
  */
trait IndexedSeq[+A] extends Any with Family[IndexedSeq[A]] with Seq[A] {
  /** Returns the element at `index`.
    * @group Examining */
  def apply(index: Int): A
  
  override def isEmpty: Boolean = length == 0
  
  override def iterator: Iterator[A] = new IndexedSeqIterator(this)
  
  protected override def foreach[U](f: A => U) {
    var i = 0
    val n = length
    while (i < n) {
      f(this(i))
      i += 1
    }
  }
}

private[collections] final class IndexedSeqIterator[+A]
    (xs: IndexedSeq[A], private[this] var i: Int, n: Int)
  extends Iterator[A] {
  
  def this(xs: IndexedSeq[A]) = this(xs, 0, xs.length)
  
  override def isEmpty: Boolean = i >= n
  
  override def head: A = {
    if (i < n) xs(i)
    else Done.head
  }
  
  override def step() {
    if (i < n) i += 1
    else Done.step()
  }
  
  override def dup: Iterator[A] = new IndexedSeqIterator(xs, i, n)
}
