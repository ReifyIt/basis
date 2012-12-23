/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.util._

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
  
  /** Copies elements from this $collection to an array.
    * 
    * @param  index   the index to copy from in this $collection.
    * @param  to      the destination array.
    * @param  offset  the offset to copy to in the destination array.
    * @param  count   the number of elements to copy.
    * @group  Converting
    */
  def copyToArray[B >: A](index: Int, to: Array[B], offset: Int, count: Int) {
    var i = index
    var j = offset
    var k = 0
    while (k < count) {
      to(j) = this(i)
      i += 1
      j += 1
      k += 1
    }
  }
  
  /** Returns an array containing the elements of this $collection.
    * 
    * @group  Converting
    * @usecase def toArray: Array[A]
    * @inheritdoc
    */
  def toArray[B >: A](implicit B: scala.reflect.ClassTag[B]): Array[B] = {
    var i = 0
    val n = length
    val newArray = B.newArray(n)
    while (i < n) {
      newArray(i) = this(i)
      i += 1
    }
    newArray
  }
}
