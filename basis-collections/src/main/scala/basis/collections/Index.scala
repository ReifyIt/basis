/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

/** An indexed sequence. Indexes can randomly access their elements.
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
  * @groupprio  Indexing      2
  * @groupprio  Traversing    3
  * @groupprio  Classifying   4
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
  extends Any with Family[Index[_]] with Seq[A] {
  
  /** Returns the number of elements in this $collection.
    * @group Measuring */
  def length: Int
  
  /** Returns the element at the given index.
    * @group Indexing */
  def apply(index: Int): A
  
  override def iterator: Iterator[A] = new IndexIterator(this)
  
  override def traverse(f: A => Unit) {
    var i = 0
    val n = length
    while (i < n) {
      f(this(i))
      i += 1
    }
  }
}

private[collections] final class IndexIterator
    [@specialized(Byte, Short, Int, Long, Float, Double, Boolean) +A]
    (protected[this] val xs: Index[A], protected[this] var i: Int, protected[this] val n: Int)
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
