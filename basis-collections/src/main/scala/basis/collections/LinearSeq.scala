/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

/** A linear sequence of elements.
  * 
  * @groupprio  Examining   -3
  * @groupprio  Iterating   -2
  * @groupprio  Traversing  -1
  */
trait LinearSeq[+A] extends Any with Seq[A] {
  override type Self <: LinearSeq[A]
  
  /** Returns the first element of this $collection.
    * @group Examining */
  def head: A
  
  /** Returns all except the first element of this $collection.
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
  
  override def iterator: Iterator[A] =
    new LinearSeqIterator(this)
  
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
  
  override def head: A = if (isEmpty) Done.head else xs.head
  
  override def step(): Unit = if (isEmpty) Done.step() else xs = xs.tail
  
  override def dup: Iterator[A] = new LinearSeqIterator(xs)
}
