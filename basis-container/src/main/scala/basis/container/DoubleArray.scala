/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis
package container

final class DoubleArray(val array: scala.Array[Double]) extends AnyVal with Array[Double] {
  override def length: Int = array.length
  
  override def apply(index: Int): Double = array(index)
  
  def update(index: Int, value: Double): Unit = array(index) = value
  
  def copy(length: Int = this.length): DoubleArray = {
    val newArray = new scala.Array[Double](length)
    scala.Array.copy(array, 0, newArray, 0, scala.math.min(array.length, length))
    new DoubleArray(newArray)
  }
}

object DoubleArray {
  val empty: DoubleArray = DoubleArray(0)
  
  def apply(length: Int): DoubleArray =
    new DoubleArray(new scala.Array[Double](length))
}
