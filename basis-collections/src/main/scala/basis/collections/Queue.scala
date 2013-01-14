/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

import scala.annotation.unspecialized

/** A linear sequence. Queues efficiently decompose into a `head` element and
  * a `tail` queue.
  * 
  * ==Extensions==
  * $Extensions
  * $SequentialOps
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Collections
  * 
  * @groupprio  Quantifying   1
  * @groupprio  Decomposing   2
  * @groupprio  Iterating     3
  * @groupprio  Traversing    4
  * @groupprio  Classifying   5
  * 
  * @define collection  queue
  * @define SequentialOps
  * The following classes implement the extensions to this interface:
  * 
  *  - [[basis.sequential.GeneralQueueOps GeneralQueueOps]]
  *    implements reductive operations (`foreach`, `fold`, `reduce`, etc.).
  *  - [[basis.sequential.StrictQueueOps StrictQueueOps]]
  *    implements eager transformations (`map`, `flatMap`, `filter`, etc.).
  *  - [[basis.sequential.NonStrictQueueOps NonStrictQueueOps]]
  *    implements lazy transformations (`map`, `flatMap`, `filter`, etc.).
  */
trait Queue[@specialized(Int, Long, Float, Double, Boolean) +A]
  extends Any with Family[Queue[A]] with Seq[A] {
  
  /** Returns `true` if this $collection doesn't contain any elements.
    * @group Quantifying */
  def isEmpty: Boolean
  
  /** Returns the first element of this non-empty $collection.
    * @group Decomposing */
  def head: A
  
  /** Returns all elements except the first of this non-empty $collection.
    * @group Decomposing */
  def tail: Family
  
  @unspecialized override def iterator: Iterator[A] = new QueueIterator(this)
  
  @unspecialized protected override def foreach[U](f: A => U) {
    var xs = this
    while (!xs.isEmpty) {
      f(xs.head)
      xs = xs.tail
    }
  }
}

private[collections] final class QueueIterator[+A]
    (private[this] var xs: Queue[A])
  extends Iterator[A] {
  
  override def isEmpty: Boolean = xs.isEmpty
  
  override def head: A = {
    if (xs.isEmpty) throw new NoSuchElementException("Head of empty iterator.")
    xs.head
  }
  
  override def step() {
    if (xs.isEmpty) throw new UnsupportedOperationException("Empty iterator step.")
    xs = xs.tail
  }
  
  override def dup: Iterator[A] = new QueueIterator(xs)
}
