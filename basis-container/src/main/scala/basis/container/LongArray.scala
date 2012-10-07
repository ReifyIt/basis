/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis
package container

final class LongArray(val array: scala.Array[Long]) extends AnyVal with Array[Long] {
  override def length: Int = array.length
  
  override def apply(index: Int): Long = array(index)
  
  def update(index: Int, value: Long): Unit = array(index) = value
  
  def copy(length: Int = this.length): LongArray = {
    val newArray = new scala.Array[Long](length)
    scala.Array.copy(array, 0, newArray, 0, scala.math.min(array.length, length))
    new LongArray(newArray)
  }
}

object LongArray {
  val Empty: LongArray = LongArray(0)
  
  def apply(length: Int): LongArray =
    new LongArray(new scala.Array[Long](length))
}
