/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

/** An associated set of (key, value) pairs with unique keys.
  * 
  * @groupprio  Examining     -4
  * @groupprio  Iterating     -3
  * @groupprio  Traversing    -2
  * @groupprio  Classifying   -1
  * 
  * @define collection  map
  */
trait Map[+A, +T] extends Any with Family[Map[A, T]] with Container[(A, T)] {
  import scala.annotation.unchecked.uncheckedVariance
  
  /** Returns `true` if this $collection doesn't contain any associations.
    * @group Examining */
  def isEmpty: Boolean = iterator.isEmpty
  
  /** Returns the number of associations in this $collection.
    * @group Examining */
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
    * @group Examining */
  def contains(key: A @uncheckedVariance): Boolean = {
    val entries = iterator
    while (!entries.isEmpty) {
      if (key == entries.head._1) return true
      entries.step()
    }
    false
  }
  
  /** Returns the value associated with the given key.
    * @group Examining */
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
    * @group Examining */
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
