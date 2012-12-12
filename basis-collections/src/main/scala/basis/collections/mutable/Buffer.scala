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
  def apply(index: Int): A
  
  /** Replaces the element at `index` with the given one.
    * @group Modifying */
  def update(index: Int, elem: A): Unit
  
  /** Appends a single element to this $collection.
    * @group Inserting */
  def += (elem: A): this.type
  
  /** Appends multiple elements to this $collection.
    * @group Inserting */
  def ++= (elems: Enumerator[A]): this.type
  
  /** Prepends a single element to this $collection.
    * @group Inserting */
  def +=: (elem: A): this.type
  
  /** Prepends multiple elements to this $collection.
    * @group Inserting */
  def ++=: (elems: Enumerator[A]): this.type
  
  /** Removes the first occurrence of an element from this $collection.
    * @group Removing */
  def -= (elem: A): this.type
  
  /** Inserts a single element into this $collection at `index`.
    * @group Inserting */
  def insert(index: Int, elem: A): Unit
  
  /** Inserts multiple elements into this $collection, starting at `index`.
    * @group Inserting */
  def insertAll(index: Int, elems: Enumerator[A]): Unit
  
  /** Removes and returns the element of this $collection at `index`.
    * @group Removing */
  def remove(index: Int): A
  
  /** Removes multiple elements from this $collection, starting at `index`.
    * @group Removing */
  def remove(index: Int, count: Int): Unit
}
