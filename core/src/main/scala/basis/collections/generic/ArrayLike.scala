//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package generic

import scala.reflect._

trait ArrayLike[+A] extends Any {
  def erasure: ClassTag[_]

  def length: Int

  def apply(index: Int): A

  def copyToArray[B >: A](index: Int, to: Array[B], offset: Int, count: Int): Unit = {
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

  def toArray[B >: A](implicit B: ClassTag[B]): Array[B] = {
    var i = 0
    val n = length
    val array = B.newArray(n)
    while (i < n) {
      array(i) = this(i)
      i += 1
    }
    array
  }
}
