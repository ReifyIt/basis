/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

import scala.annotation.unchecked.uncheckedVariance

/** A unique set. Sets contain no more than one of each element.
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
  * @groupprio  Measuring     1
  * @groupprio  Querying      2
  * @groupprio  Iterating     3
  * @groupprio  Traversing    4
  * @groupprio  Classifying   5
  * 
  * @define collection  set
  * @define SequentialOps
  * The following classes implement the extensions to this interface:
  * 
  *  - [[basis.sequential.GeneralSetOps GeneralSetOps]]
  *    implements reductive operations (`foreach`, `fold`, `reduce`, etc.).
  *  - [[basis.sequential.StrictSetOps StrictSetOps]]
  *    implements eager transformations (`map`, `flatMap`, `filter`, etc.).
  *  - [[basis.sequential.NonStrictSetOps NonStrictSetOps]]
  *    implements lazy transformations (`map`, `flatMap`, `filter`, etc.).
  */
trait Set[+A] extends Any with Equals with Family[Set[_]] with Container[A] {
  /** Returns `true` if this $collection doesn't contain any elements.
    * @group Measuring */
  def isEmpty: Boolean = iterator.isEmpty
  
  /** Returns the number of elements in this $collection.
    * @group Measuring */
  def size: Int = {
    var count = 0
    val these = iterator
    while (!these.isEmpty) {
      count += 1
      these.step()
    }
    count
  }
  
  /** Returns `true` if this $collection contains the given element.
    * @group Querying */
  def contains(element: A @uncheckedVariance): Boolean = {
    val these = iterator
    while (!these.isEmpty) {
      if (element == these.head) return true
      these.step()
    }
    false
  }
  
  /** Returns `true` if this $collection might equal another object, otherwise `false`.
    * @group Classifying */
  override def canEqual(other: Any): Boolean = other.isInstanceOf[Set[_]]
  
  /** Returns `true` if this $collection contains exactly the same elements as
    * another set, otherwise `false`.
    * @group Classifying */
  override def equals(other: Any): Boolean = other match {
    case that: Set[_] =>
      (this.asInstanceOf[AnyRef] eq that.asInstanceOf[AnyRef]) ||
      (that canEqual this) && 
      (this.size == that.size) && {
        val those = that.asInstanceOf[Set[A]].iterator
        while (!those.isEmpty) {
          if (!contains(those.head)) return false
          those.step()
        }
        true
      }
    case _ => false
  }
  
  /** Returns a hash of the elements in this $collection.
    * @group Classifying */
  override def hashCode: Int = {
    import basis.util.MurmurHash3._
    var a, b = 0
    var c = 1
    val these = iterator
    while (!these.isEmpty) {
      val h = hash(these.head)
      a ^= h
      b += h
      if (h != 0) c *= h
      these.step()
    }
    mash(mix(mix(mix(seed[Set[_]], a), b), c))
  }
}
