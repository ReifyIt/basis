/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._

class RefArray[A](val array: scala.Array[AnyRef]) extends AnyVal with Array[A] {
  override def length: Int = array.length
  
  override def apply(index: Int): A = array(index).asInstanceOf[A]
  
  def update(index: Int, value: A): Unit = array(index) = value.asInstanceOf[AnyRef]
  
  def copy(length: Int = this.length): RefArray[A] = {
    val newArray = new scala.Array[AnyRef](length)
    scala.Array.copy(array, 0, newArray, 0, scala.math.min(array.length, length))
    new RefArray[A](newArray)
  }
}

object RefArray {
  private[this] val Empty = RefArray[Nothing](0)
  
  def empty[T]: RefArray[T] = Empty.asInstanceOf[RefArray[T]]
  
  def apply[A](length: Int): RefArray[A] =
    new RefArray[A](new scala.Array[AnyRef](length))
}
