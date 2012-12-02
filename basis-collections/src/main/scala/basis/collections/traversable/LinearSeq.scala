/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package traversable

/** A linear sequence of elements.
  * 
  * @groupprio  Examining     -4
  * @groupprio  Iterating     -3
  * @groupprio  Traversing    -2
  * @groupprio  Classifying   -1
  */
trait LinearSeq[+A] extends Any with Family[LinearSeq[A]] with Seq[A] {
  /** Returns the first element of this non-empty $collection.
    * @group Examining */
  def head: A
  
  /** Returns all elements except the first of this non-empty $collection.
    * @group Examining */
  def tail: LinearSeq[A]
  
  override def length: Int = {
    var xs = this
    var count = 0
    while (!xs.isEmpty) {
      count += 1
      xs = xs.tail
    }
    count
  }
  
  override def iterator: Iterator[A] = new LinearSeqIterator(this)
  
  protected override def foreach[U](f: A => U) {
    var xs = this
    while (!xs.isEmpty) {
      f(xs.head)
      xs = xs.tail
    }
  }
}

private[collections] final class LinearSeqIterator[+A]
    (private[this] var xs: LinearSeq[A])
  extends Iterator[A] {
  
  override def isEmpty: Boolean = xs.isEmpty
  
  override def head: A = {
    if (!xs.isEmpty) xs.head
    else throw new NoSuchElementException("Head of empty iterator.")
  }
  
  override def step() {
    if (!xs.isEmpty) xs = xs.tail
    else throw new UnsupportedOperationException("Empty iterator step.")
  }
  
  override def dup: Iterator[A] = new LinearSeqIterator(xs)
}
