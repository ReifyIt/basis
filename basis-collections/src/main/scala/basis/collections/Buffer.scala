/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

/** A mutable buffer of elements.
  * 
  * @groupprio  Examining     -7
  * @groupprio  Mutating      -6
  * @groupprio  Inserting     -5
  * @groupprio  Removing      -4
  * @groupprio  Iterating     -3
  * @groupprio  Traversing    -2
  * @groupprio  Classifying   -1
  * 
  * @define collection  buffer
  */
trait Buffer[@specialized(Specializable.Primitives) A]
  extends Equals
    with Family[Buffer[A]]
    with Seq[A]
    with Accumulator[A] {
  
  /** Returns the element at the given index.
    * @group Examining */
  def apply(index: Int): A
  
  /** Replaces the element at `index` with the given one.
    * @group Mutating */
  def update(index: Int, elem: A): Unit
  
  /** Prepends a single element to this $collection.
    * @group Inserting */
  def +=: (elem: A): this.type
  
  /** Prepends multiple elements to this $collection.
    * @group Inserting */
  def ++=: (elems: Enumerator[A]): this.type = {
    traverse(elems)(new Buffer.Insert(this, 0))
    this
  }
  
  /** Removes the first occurrence of an element from this $collection.
    * @group Removing */
  def -= (elem: A): this.type = {
    val xs = iterator
    var i = 0
    while (!xs.isEmpty) {
      if (elem == xs.head) {
        remove(i)
        return this
      }
      xs.step()
      i += 1
    }
    this
  }
  
  /** Inserts a single element into this $collection at `index`.
    * @group Inserting */
  def insert(index: Int, elem: A): Unit
  
  /** Inserts multiple elements into this $collection, starting at `index`.
    * @group Inserting */
  def insertAll(index: Int, elems: Enumerator[A]) {
    if (index < 0 || index > length)
      throw new IndexOutOfBoundsException(index.toString)
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
}

private[collections] object Buffer {
  import scala.runtime.AbstractFunction1
  
  final class Insert[-A](b: Buffer[A], private[this] var i: Int)
    extends AbstractFunction1[A, Unit] {
    override def apply(x: A) {
      b.insert(i, x)
      i += 1
    }
  }
}
