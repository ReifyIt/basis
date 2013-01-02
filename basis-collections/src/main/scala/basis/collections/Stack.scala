/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

import scala.annotation.unspecialized

/** A linear sequence. Stacks decompose into a `head` element and a `tail` stack.
  * 
  * ==Extensions==
  * $Extensions
  * $SequentialOps
  * 
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  * 
  * @groupprio  Quantifying   1
  * @groupprio  Decomposing   2
  * @groupprio  Iterating     3
  * @groupprio  Traversing    4
  * @groupprio  Classifying   5
  * 
  * @define collection  stack
  * @define SequentialOps
  * The following classes implement the extensions to this interface:
  * 
  *  - [[basis.sequential.GeneralStackOps GeneralStackOps]]
  *    implements reductive operations (`foreach`, `fold`, `reduce`, etc.).
  *  - [[basis.sequential.StrictStackOps StrictStackOps]]
  *    implements eager transformations (`map`, `flatMap`, `filter`, etc.).
  *  - [[basis.sequential.NonStrictStackOps NonStrictStackOps]]
  *    implements lazy transformations (`map`, `flatMap`, `filter`, etc.).
  */
trait Stack[@specialized(Byte, Short, Int, Long, Float, Double, Boolean) +A]
  extends Any with Family[Stack[A]] with Seq[A] {
  
  /** Returns the first element of this non-empty $collection.
    * @group Decomposing */
  def head: A
  
  /** Returns all elements except the first of this non-empty $collection.
    * @group Decomposing */
  def tail: Family
  
  override def length: Int = {
    var xs = this
    var count = 0
    while (!xs.isEmpty) {
      count += 1
      xs = xs.tail
    }
    count
  }
  
  @unspecialized override def iterator: Iterator[A] = new StackIterator(this)
  
  @unspecialized protected override def foreach[U](f: A => U) {
    var xs = this
    while (!xs.isEmpty) {
      f(xs.head)
      xs = xs.tail
    }
  }
}

private[collections] final class StackIterator[+A]
    (private[this] var xs: Stack[A])
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
  
  override def dup: Iterator[A] = new StackIterator(xs)
}
