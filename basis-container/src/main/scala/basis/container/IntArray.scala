/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis
package container

final class IntArray(val array: scala.Array[Int]) extends AnyVal with Array[Int] {
  override def length: Int = array.length
  
  override def apply(index: Int): Int = array(index)
  
  def update(index: Int, value: Int): Unit = array(index) = value
  
  def copy(length: Int = this.length): IntArray = {
    val newArray = new scala.Array[Int](length)
    scala.Array.copy(array, 0, newArray, 0, scala.math.min(array.length, length))
    new IntArray(newArray)
  }
}

object IntArray {
  val Empty: IntArray = IntArray(0)
  
  def apply(length: Int): IntArray =
    new IntArray(new scala.Array[Int](length))
}
