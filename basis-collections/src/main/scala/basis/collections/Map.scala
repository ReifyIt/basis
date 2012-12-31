/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

import scala.annotation.unchecked.uncheckedVariance

/** An associative map. Maps consist of (key, value) pairs with unique keys.
  * 
  * ==Extensions==
  * $Extensions
  * $SequentialOps
  * 
  * @groupprio  Quantifying   -5
  * @groupprio  Querying      -4
  * @groupprio  Iterating     -3
  * @groupprio  Traversing    -2
  * @groupprio  Classifying   -1
  * 
  * @define collection  map
  * @define SequentialOps
  * The following classes implement the extensions to this interface:
  * 
  *  - [[basis.sequential.GeneralMapOps GeneralMapOps]]
  *    implements reductive operations (`foreach`, `fold`, `reduce`, etc.).
  *  - [[basis.sequential.StrictMapOps StrictMapOps]]
  *    implements eager transformations (`map`, `flatMap`, `filter`, etc.).
  *  - [[basis.sequential.NonStrictMapOps NonStrictMapOps]]
  *    implements lazy transformations (`map`, `flatMap`, `filter`, etc.).
  */
trait Map[+A, +T] extends Any with Family[Map[A, T]] with Container[(A, T)] {
  /** Returns `true` if this $collection doesn't contain any associations.
    * @group Quantifying */
  def isEmpty: Boolean = iterator.isEmpty
  
  /** Returns the number of associations in this $collection.
    * @group Quantifying */
  def size: Int = {
    var count = 0
    val entries = iterator
    while (!entries.isEmpty) {
      count += 1
      entries.step()
    }
    count
  }
  
  /** Returns `true` if this $collection has a value associated with the given key.
    * @group Querying */
  def contains(key: A @uncheckedVariance): Boolean = {
    val entries = iterator
    while (!entries.isEmpty) {
      if (key == entries.head._1) return true
      entries.step()
    }
    false
  }
  
  /** Returns the value associated with the given key.
    * @group Querying */
  def apply(key: A @uncheckedVariance): T = {
    val entries = iterator
    while (!entries.isEmpty) {
      val entry = entries.head
      if (key == entry._1) return entry._2
      entries.step()
    }
    throw new NoSuchElementException(key.toString)
  }
  
  /** Returns some value associated with the given key, or none if no association exists.
    * @group Querying */
  def get(key: A @uncheckedVariance): Option[T] = {
    val entries = iterator
    while (!entries.isEmpty) {
      val entry = entries.head
      if (key == entry._1) return Some(entry._2)
      entries.step()
    }
    None
  }
  
  /** Returns a new iterator over the (key, value) pairs of this $collection.
    * @group Iterating */
  override def iterator: Iterator[(A, T)]
}
