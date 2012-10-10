/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._

class ByteArray(val array: scala.Array[Byte]) extends AnyVal with Array[Byte] {
  override def length: Int = array.length
  
  override def apply(index: Int): Byte = array(index)
  
  def update(index: Int, value: Byte): Unit = array(index) = value
  
  def copy(length: Int = this.length): ByteArray = {
    val newArray = new scala.Array[Byte](length)
    scala.Array.copy(array, 0, newArray, 0, scala.math.min(array.length, length))
    new ByteArray(newArray)
  }
}

object ByteArray {
  val empty: ByteArray = ByteArray(0)
  
  def apply(length: Int): ByteArray =
    new ByteArray(new scala.Array[Byte](length))
}
