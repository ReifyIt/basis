/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

/** A linked sequence. A link efficiently decompose into its `head` element
  * and `tail` link.
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
  * @groupprio  Measuring     1
  * @groupprio  Decomposing   2
  * @groupprio  Traversing    3
  * @groupprio  Classifying   4
  * 
  * @define collection  link
  * @define SequentialOps
  * The following classes implement the extensions to this interface:
  * 
  *  - [[basis.sequential.GeneralLinkOps GeneralLinkOps]]
  *    implements reductive operations (`foreach`, `fold`, `reduce`, etc.).
  *  - [[basis.sequential.StrictLinkOps StrictLinkOps]]
  *    implements eager transformations (`map`, `flatMap`, `filter`, etc.).
  *  - [[basis.sequential.NonStrictLinkOps NonStricLinkOps]]
  *    implements lazy transformations (`map`, `flatMap`, `filter`, etc.).
  */
trait Link[@specialized(Int, Long, Float, Double, Boolean) +A]
  extends Any with Equals with Family[Link[_]] with Seq[A] {
  
  /** Returns `true` if this $collection doesn't contain any elements.
    * @group Measuring */
  def isEmpty: Boolean
  
  /** Returns the first element of this non-empty $collection.
    * @group Decomposing */
  def head: A
  
  /** Returns all elements except the first of this non-empty $collection.
    * @group Decomposing */
  def tail: Link[A]
  
  override def iterator: Iterator[A] = new LinkIterator(this)
  
  override def traverse(f: A => Unit) {
    var xs = this
    while (!xs.isEmpty) {
      f(xs.head)
      xs = xs.tail
    }
  }
}

private[collections] final class LinkIterator
    [@specialized(Int, Long, Float, Double, Boolean) +A]
    (protected[this] var xs: Link[A])
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
  
  override def dup: Iterator[A] = new LinkIterator(xs)
}
