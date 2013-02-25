/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

import basis.control._

import scala.annotation.unchecked.uncheckedVariance

/** An associative map. Maps consist of (key, value) pairs with unique keys.
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
  * @groupprio  Querying      2
  * @groupprio  Traversing    3
  * @groupprio  Classifying   4
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
trait Map[+A, +T] extends Any with Equals with Family[Map[_, _]] with Container[(A, T)] {
  /** Returns `true` if this $collection doesn't contain any associations.
    * @group Measuring */
  def isEmpty: Boolean = iterator.isEmpty
  
  /** Returns the number of associations in this $collection.
    * @group Measuring */
  def size: Int = {
    var count = 0
    val entries = iterator
    while (!entries.isEmpty) {
      count += 1
      entries.step()
    }
    count
  }
  
  /** Returns `true` if this $collection has an association with the given key.
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
  
  /** Returns the free value associated with the given key,
    * or a trap if no association exists.
    * @group Querying */
  def get(key: A @uncheckedVariance): Maybe[T] = {
    val entries = iterator
    while (!entries.isEmpty) {
      val entry = entries.head
      if (key == entry._1) return Bind(entry._2)
      entries.step()
    }
    Trap
  }
  
  /** Applies a side-effecting functiont or each key and value of this $collection.
    * @group Traversing */
  def traverse(f: (A, T) => Unit) {
    val xs = iterator
    while (!xs.isEmpty) {
      val x = xs.head
      f(x._1, x._2)
      xs.step()
    }
  }
  
  /** Applies a side-effecting function to each (key, value) pair of this $collection.
    * @group Traversing */
  override def traverse(f: ((A, T)) => Unit) {
    val xs = iterator
    while (!xs.isEmpty) { f(xs.head); xs.step() }
  }
  
  /** Returns a new iterator over the (key, value) pairs of this $collection.
    * @group Traversing */
  override def iterator: Iterator[(A, T)]
  
  /** Returns `true` if this $collection might equal another object, otherwise `false`.
    * @group Classifying */
  override def canEqual(other: Any): Boolean = other.isInstanceOf[Map[_, _]]
  
  /** Returns `true` if this $collection contains exactly the same keys as
    * another map, otherwise `false`.
    * @group Classifying */
  override def equals(other: Any): Boolean = other match {
    case that: Map[_, _] =>
      (this.asInstanceOf[AnyRef] eq that.asInstanceOf[AnyRef]) ||
      (that canEqual this) && 
      (this.size == that.size) && {
        val entries = that.asInstanceOf[Map[A, _]].iterator
        while (!entries.isEmpty) {
          if (!contains(entries.head._1)) return false
          entries.step()
        }
        true
      }
    case _ => false
  }
  
  /** Returns a hash of the keys in this $collection.
    * @group Classifying */
  override def hashCode: Int = {
    import basis.util.MurmurHash3._
    var a, b = 0
    var c = 1
    val these = iterator
    while (!these.isEmpty) {
      val h = hash(these.head._1)
      a ^= h
      b += h
      if (h != 0) c *= h
      these.step()
    }
    mash(mix(mix(mix(seed[Map[_, _]], a), b), c))
  }
}
