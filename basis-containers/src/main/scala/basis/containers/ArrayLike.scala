/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.util._

import scala.reflect.ClassTag

/** An array-convertible collection.
  * 
  * @groupprio  Quantifying   -3
  * @groupprio  Indexing      -2
  * @groupprio  Converting    -1
  * 
  * @define collection  collection
  */
trait ArrayLike[+A] extends Any { this: Enumerator[A] =>
  /** Returns the number of elements in this $collection.
    * @group Quantifying */
  def length: Int
  
  /** Returns the element at the given index.
    * @group Indexing */
  def apply(index: Int): A
  
  /** Copies elements from this $collection to an array slice.
    * 
    * @param  xs      the destination array.
    * @param  start   the offset to copy to in the destination array.
    * @param  count   the maximum number of elements to copy.
    * @group  Converting
    */
  def copyToArray[B >: A](xs: Array[B], start: Int, count: Int) {
    var i = 0
    var j = start
    val n = count min (xs.length - start) min length
    while (i < n) {
      xs(j) = this(i)
      i += 1
      j += 1
    }
  }
  
  /** Copies elements from this $collection to an array offset.
    * 
    * @param  xs      the destination array.
    * @param  start   the offset to copy to in the destination array.
    * @group  Converting
    */
  def copyToArray[B >: A](xs: Array[B], start: Int) {
    var i = 0
    var j = start
    val n = (xs.length - start) min length
    while (i < n) {
      xs(j) = this(i)
      i += 1
      j += 1
    }
  }
  
  /** Copies elements from this $collection to an array.
    * 
    * @param  xs  the destination array.
    * @group  Converting
    */
  def copyToArray[B >: A](xs: Array[B]) {
    var i = 0
    val n = xs.length min length
    while (i < n) {
      xs(i) = this(i)
      i += 1
    }
  }
  
  /** Returns an array containing the elements of this $collection.
    * 
    * @group  Converting
    * @usecase def toArray: Array[A]
    * @inheritdoc
    */
  def toArray[B >: A](implicit B: ClassTag[B]): Array[B] = {
    var i = 0
    val n = length
    val xs = B.newArray(n)
    while (i < n) {
      xs(i) = this(i)
      i += 1
    }
    xs
  }
}
