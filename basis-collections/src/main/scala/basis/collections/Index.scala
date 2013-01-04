/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

import scala.annotation.unspecialized

/** An indexed sequence. Indexes can randomly access their elements.
  * 
  * ==Extensions==
  * $Extensions
  * $SequentialOps
  * 
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  * @group    Collections
  * 
  * @groupprio  Quantifying   1
  * @groupprio  Indexing      2
  * @groupprio  Iterating     3
  * @groupprio  Traversing    4
  * @groupprio  Classifying   5
  * 
  * @define collection  index
  * @define SequentialOps
  * The following classes implement the extensions to this interface:
  * 
  *  - [[basis.sequential.GeneralIndexOps GeneralIndexOps]]
  *    implements reductive operations (`foreach`, `fold`, `reduce`, etc.).
  *  - [[basis.sequential.StrictIndexOps StrictIndexOps]]
  *    implements eager transformations (`map`, `flatMap`, `filter`, etc.).
  *  - [[basis.sequential.NonStrictIndexOps NonStrictIndexOps]]
  *    implements lazy transformations (`map`, `flatMap`, `filter`, etc.).
  */
trait Index[@specialized(Byte, Short, Int, Long, Float, Double, Boolean) +A]
  extends Any with Family[Index[A]] with Seq[A] {
  
  /** Returns the number of elements in this $collection.
    * @group Quantifying */
  def length: Int
  
  /** Returns the element at the given index.
    * @group Indexing */
  def apply(index: Int): A
  
  @unspecialized override def iterator: Iterator[A] = new IndexIterator(this)
  
  @unspecialized protected override def foreach[U](f: A => U) {
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
