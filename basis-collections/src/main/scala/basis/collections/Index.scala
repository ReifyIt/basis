/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

/** An indexed sequence of elements.
  * 
  * @groupprio  Examining     -4
  * @groupprio  Iterating     -3
  * @groupprio  Traversing    -2
  * @groupprio  Classifying   -1
  */
trait Index[+A] extends Any with Family[Index[A]] with Seq[A] {
  /** Returns the element at the given index.
    * @group Examining */
  def apply(index: Int): A
  
  override def isEmpty: Boolean = length == 0
  
  override def iterator: Iterator[A] = new IndexIterator(this)
  
  protected override def foreach[U](f: A => U) {
    var i = 0
    val n = length
    while (i < n) {
      f(this(i))
      i += 1
    }
  }
}

private[collections] final class IndexIterator[+A]
    (xs: Index[A], private[this] var i: Int, n: Int)
  extends Iterator[A] {
  
  def this(xs: Index[A]) = this(xs, 0, xs.length)
  
  override def isEmpty: Boolean = i >= n
  
  override def head: A = {
    if (i < n) xs(i)
    else throw new NoSuchElementException("Head of empty iterator.")
  }
  
  override def step() {
    if (i < n) i += 1
    else throw new UnsupportedOperationException("Empty iterator step.")
  }
  
  override def dup: Iterator[A] = new IndexIterator(xs, i, n)
}
