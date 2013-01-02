/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

/** A mutable collection.
  * 
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  * 
  * @groupprio  Quantifying   1
  * @groupprio  Indexing      2
  * @groupprio  Inserting     3
  * @groupprio  Removing      4
  * @groupprio  Iterating     5
  * @groupprio  Traversing    6
  * @groupprio  Converting    7
  * @groupprio  Classifying   8
  * 
  * @define collection  buffer
  */
trait Buffer[@specialized(Byte, Short, Int, Long, Float, Double, Boolean) A]
  extends Equals with Family[Buffer[A]] with Seq[A] with Accumulator[A] {
  
  /** Returns the element at the given index.
    * @group Indexing */
  def apply(index: Int): A
  
  /** Replaces the element at `index` with the given element.
    * @group Indexing */
  def update(index: Int, elem: A): Unit
  
  /** Prepends a single element to this $collection.
    * @group Inserting */
  def prepend(elem: A): Unit
  
  /** Prepends multiple elements to this $collection.
    * @group Inserting */
  def prependAll(elems: Enumerator[A]) {
    traverse(elems)(new Buffer.Insert(this, 0))
  }
  
  /** Inserts a single element into this $collection at `index`.
    * @group Inserting */
  def insert(index: Int, elem: A): Unit
  
  /** Inserts multiple elements into this $collection, starting at `index`.
    * @group Inserting */
  def insertAll(index: Int, elems: Enumerator[A]) {
    if (index < 0 || index > length) throw new IndexOutOfBoundsException(index.toString)
    traverse(elems)(new Buffer.Insert(this, index))
  }
  
  /** Removes and returns the element of this $collection at `index`.
    * @group Removing */
  def remove(index: Int): A
  
  /** Removes multiple elements from this $collection, starting at `index`.
    * @group Removing */
  def remove(index: Int, count: Int) {
    if (count < 0) throw new IllegalArgumentException("negative count")
    if (index < 0) throw new IndexOutOfBoundsException(index.toString)
    var i = index + count - 1
    if (i >= length) throw new IndexOutOfBoundsException(i.toString)
    while (i >= index) {
      remove(i)
      i -= 1
    }
  }
  
  /** Removes all elements from this $collection.
    * @group Removing */
  def clear(): Unit
  
  /** Returns a distinct copy of this $collection.
    * @group Converting */
  def copy: Family
  
  /** Prepends a single element to this $collection.
    * @group Inserting */
  def +=: (elem: A): this.type = {
    prepend(elem)
    this
  }
  
  /** Prepends multiple elements to this $collection.
    * @group Inserting */
  def ++=: (elems: Enumerator[A]): this.type = {
    prependAll(elems)
    this
  }
}

private[collections] object Buffer {
  import scala.runtime.AbstractFunction1
  
  final class Insert[-A](b: Buffer[A], private[this] var i: Int) extends AbstractFunction1[A, Unit] {
    override def apply(x: A) {
      b.insert(i, x)
      i += 1
    }
  }
}
