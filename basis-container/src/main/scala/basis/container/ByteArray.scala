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
  
  private[basis] def update(index: Int, value: Byte): Unit =
    array(index) = value
  
  private[basis] def copy(length: Int): ByteArray = {
    val newArray = new scala.Array[Byte](length)
    java.lang.System.arraycopy(array, 0, newArray, 0, array.length min length)
    new ByteArray(newArray)
  }
}

private[basis] object ByteArray {
  val empty: ByteArray = ByteArray(0)
  
  def apply(length: Int): ByteArray =
    new ByteArray(new scala.Array[Byte](length))
}

final class ByteArrayBuffer extends Buffer[Any, Byte] {
  override type State = ByteArray
  
  private[this] var array: ByteArray = ByteArray.empty
  
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
  
  override def += (value: Byte): this.type = {
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
  
  override def state: ByteArray = {
    if (length != array.length) array = array.copy(length)
    aliased = true
    array
  }
  
  override def clear() {
    array = ByteArray.empty
    aliased = true
    length = 0
  }
}
