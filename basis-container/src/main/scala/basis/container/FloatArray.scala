/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._

final class FloatArray(val array: scala.Array[Float]) extends AnyVal with Array[Float] {
  override def length: Int = array.length
  
  override def apply(index: Int): Float = array(index)
  
  def update(index: Int, value: Float): Unit = array(index) = value
  
  def copy(length: Int = this.length): FloatArray = {
    val newArray = new scala.Array[Float](length)
    scala.Array.copy(array, 0, newArray, 0, scala.math.min(array.length, length))
    new FloatArray(newArray)
  }
}

object FloatArray {
  val empty: FloatArray = FloatArray(0)
  
  def apply(length: Int): FloatArray =
    new FloatArray(new scala.Array[Float](length))
}
