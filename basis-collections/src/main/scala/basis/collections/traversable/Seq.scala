/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package traversable

import basis.util._

/** An iterable sequence of elements.
  * 
  * @groupprio  Examining     -4
  * @groupprio  Iterating     -3
  * @groupprio  Traversing    -2
  * @groupprio  Classifying   -1
  * 
  * @define collection  sequence
  */
trait Seq[+A] extends Any with Equals with Family[Seq[A]] with Container[A] {
  /** Returns `true` if this $collection doesn't contain any elements.
    * @group Examining */
  def isEmpty: Boolean = iterator.isEmpty
  
  /** Returns the number of elements in this $collection.
    * @group Examining */
  def length: Int = {
    var n = 0
    var xs = iterator
    while (!xs.isEmpty) {
      n += 1
      xs.step()
    }
    n
  }
  
  /** Returns `true` if this $collection might equal another object, otherwise `false`.
    * @group Classifying */
  override def canEqual(other: Any): Boolean = other.isInstanceOf[Seq[_]]
  
  /** Returns `true` if this $collection equals another object, otherwise `false`.
    * @group Classifying */
  override def equals(other: Any): Boolean = other match {
    case that: Seq[_] if that.canEqual(this) =>
      val xs = this.iterator
      val ys = that.iterator
      while (!xs.isEmpty && !ys.isEmpty) {
        if (xs.head != ys.head) return false
        xs.step()
        ys.step()
      }
      xs.isEmpty && ys.isEmpty
    case _ => false
  }
  
  /** Returns a hash of the elements in this $collection.
    * @group Classifying */
  override def hashCode: Int = {
    import MurmurHash3._
    var h = 63537721
    val xs = iterator
    while (!xs.isEmpty) {
      h = mix(h, xs.head.##)
      xs.step()
    }
    mash(h)
  }
}
