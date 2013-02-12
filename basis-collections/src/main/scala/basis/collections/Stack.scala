/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

/** A linear sequence. Stacks efficiently decompose into a `head` element and
  * a `tail` stack.
  * 
  * ==Extensions==
  * $Extensions
  * $SequentialOps
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    Collections
  * 
  * @groupprio  Measuring     1
  * @groupprio  Decomposing   2
  * @groupprio  Traversing    3
  * @groupprio  Classifying   4
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
trait Stack[@specialized(Int, Long, Float, Double, Boolean) +A]
  extends Any with Family[Stack[_]] with Seq[A] {
  
  /** Returns `true` if this $collection doesn't contain any elements.
    * @group Measuring */
  def isEmpty: Boolean
  
  /** Returns the first element of this non-empty $collection.
    * @group Decomposing */
  def head: A
  
  /** Returns all elements except the first of this non-empty $collection.
    * @group Decomposing */
  def tail: Stack[A]
  
  override def iterator: Iterator[A] = new StackIterator(this)
  
  override def traverse(f: A => Unit) {
    var xs = this
    while (!xs.isEmpty) {
      f(xs.head)
      xs = xs.tail
    }
  }
}

private[collections] final class StackIterator
    [@specialized(Int, Long, Float, Double, Boolean) +A]
    (protected[this] var xs: Stack[A])
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
