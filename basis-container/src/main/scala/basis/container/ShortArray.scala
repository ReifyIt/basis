/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis
package container

final class ShortArray(val array: scala.Array[Short]) extends AnyVal with Array[Short] {
  override def length: Int = array.length
  
  override def apply(index: Int): Short = array(index)
  
  def update(index: Int, value: Short): Unit = array(index) = value
  
  def copy(length: Int = this.length): ShortArray = {
    val newArray = new scala.Array[Short](length)
    scala.Array.copy(array, 0, newArray, 0, scala.math.min(array.length, length))
    new ShortArray(newArray)
  }
}

object ShortArray {
  val empty: ShortArray = ShortArray(0)
  
  def apply(length: Int): ShortArray =
    new ShortArray(new scala.Array[Short](length))
}
