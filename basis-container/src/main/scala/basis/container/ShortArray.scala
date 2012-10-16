/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._

class ShortArray(val array: scala.Array[Short]) extends AnyVal with Array[Short] {
  override def length: Int = array.length
  
  override def apply(index: Int): Short = array(index)
  
  private[basis] def update(index: Int, value: Short): Unit =
    array(index) = value
  
  private[basis] def copy(length: Int): ShortArray = {
    val newArray = new scala.Array[Short](length)
    java.lang.System.arraycopy(array, 0, newArray, 0, array.length min length)
    new ShortArray(newArray)
  }
}

private[basis] object ShortArray {
  val empty: ShortArray = ShortArray(0)
  
  def apply(length: Int): ShortArray =
    new ShortArray(new scala.Array[Short](length))
}

final class ShortArrayBuffer extends Buffer[Any, Short] {
  override type State = ShortArray
  
  private[this] var array: ShortArray = ShortArray.empty
  
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
  
  override def += (value: Short): this.type = {
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
  
  override def state: ShortArray = {
    if (length != array.length) array = array.copy(length)
    aliased = true
    array
  }
  
  override def clear() {
    array = ShortArray.empty
    aliased = true
    length = 0
  }
}
