/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package mutable

/** A mutable buffer.
  * 
  * @groupprio  Examining     -7
  * @groupprio  Modifying     -6
  * @groupprio  Inserting     -5
  * @groupprio  Removing      -4
  * @groupprio  Iterating     -3
  * @groupprio  Traversing    -2
  * @groupprio  Classifying   -1
  * 
  * @define collection  buffer
  */
trait Buffer[A] extends Equals with Family[Buffer[A]] with Seq[A] {
  /** Returns the element at `index`.
    * @group Examining */
  def apply(index: Int): A = {
    var i = 0
    var xs = iterator
    while (i < index && !xs.isEmpty) {
      i += 1
      xs.step()
    }
    if (i == index) xs.head
    else throw new IndexOutOfBoundsException(index.toString)
  }
  
  /** Appends a single element to this $collection.
    * @group Inserting */
  def += (elem: A): this.type
  
  /** Prepends a single element to this $collection.
    * @group Inserting */
  def +=: (elem: A): this.type
  
  /** Removes the first occurrence of an element from this $collection.
    * @group Removing */
  def -= (elem: A): this.type
  
  /** Inserts a single element into this $collection at `index`.
    * @group Inserting */
  def insert(index: Int, elem: A): Unit
  
  /** Inserts multiple elements into this $collection, starting at `index`.
    * @group Inserting */
  def insert(index: Int, elems: A*): Unit
  
  /** Removes and returns the element of this $collection at `index`.
    * @group Removing */
  def remove(index: Int): A
  
  /** Removes multiple elements from this $collection, starting at `index`.
    * @group Removing */
  def remove(index: Int, count: Int): Unit
}
