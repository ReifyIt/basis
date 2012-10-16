/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._

class RefArray[+A](val array: scala.Array[AnyRef]) extends AnyVal with Array[A] {
  import scala.annotation.unchecked.uncheckedVariance
  
  override def length: Int = array.length
  
  override def apply(index: Int): A = array(index).asInstanceOf[A]
  
  private[basis] def update(index: Int, value: A @uncheckedVariance): Unit =
    array(index) = value.asInstanceOf[AnyRef]
  
  private[basis] def copy(length: Int): RefArray[A] = {
    val newArray = new scala.Array[AnyRef](length)
    scala.Array.copy(array, 0, newArray, 0, array.length min length)
    new RefArray[A](newArray)
  }
}

private[basis] object RefArray {
  val empty: RefArray[Nothing] = RefArray[Nothing](0)
  
  def apply[A](length: Int): RefArray[A] =
    new RefArray[A](new scala.Array[AnyRef](length))
}

final class RefArrayBuffer[A] extends Buffer[Any, A] {
  override type State = RefArray[A]
  
  private[this] var array: RefArray[A] = RefArray.empty
  
  private[this] var aliased: Boolean = true
  
  private[this] var length: Int = 0
  
  private[this] def expand(base: Int, size: Int): Int = {
    var n = (base max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
  
  private[this] def prepare(size: Int) {
    if (aliased || size > array.length) {
      array = array.copy(expand(16, size))
      aliased = false
    }
  }
  
  override def += (value: A): this.type = {
    prepare(length + 1)
    array(length) = value
    length += 1
    this
  }
  
  override def expect(count: Int): this.type = {
    if (length + count > array.length) {
      array = array.copy(length + count)
      aliased = false
    }
    this
  }
  
  override def state: RefArray[A] = {
    if (length != array.length) array = array.copy(length)
    aliased = true
    array
  }
  
  override def clear() {
    array = RefArray.empty
    aliased = true
    length = 0
  }
}
