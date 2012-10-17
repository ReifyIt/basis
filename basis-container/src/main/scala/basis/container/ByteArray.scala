/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._

class ByteArray(val array: scala.Array[Byte]) extends AnyVal with Array[Byte] {
  override def isEmpty: Boolean = array.length == 0
  
  override def length: Int = array.length
  
  override def apply(index: Int): Byte = array(index)
  
  /** Returns a copy of this array with a new `value` at `index`. */
  def update(index: Int, value: Byte): ByteArray = {
    val newArray = array.clone
    newArray(index) = value
    new ByteArray(newArray)
  }
  
  /** Returns a copy of this array with a new `value` inserted at `index`. */
  def insert(index: Int, value: Byte): ByteArray = {
    val newArray = new scala.Array[Byte](array.length + 1)
    java.lang.System.arraycopy(array, 0, newArray, 0, index)
    newArray(index) = value
    java.lang.System.arraycopy(array, index, newArray, index + 1, array.length - index)
    new ByteArray(newArray)
  }
  
  /** Returns a copy of this array with `index` removed. */
  def remove(index: Int): ByteArray = {
    val newArray = new scala.Array[Byte](array.length - 1)
    java.lang.System.arraycopy(array, 0, newArray, 0, index)
    java.lang.System.arraycopy(array, index + 1, newArray, index, newArray.length - index)
    new ByteArray(newArray)
  }
  
  /** Returns a copy of this array with `value` appended. */
  def :+ (value: Byte): ByteArray = {
    val newArray = new scala.Array[Byte](array.length + 1)
    java.lang.System.arraycopy(array, 0, newArray, 0, array.length)
    newArray(newArray.length) = value
    new ByteArray(newArray)
  }
  
  /** Returns a copy of this array with `value` prepended. */
  def +: (value: Byte): ByteArray = {
    val newArray = new scala.Array[Byte](array.length + 1)
    newArray(0) = value
    java.lang.System.arraycopy(array, 0, newArray, 1, array.length)
    new ByteArray(newArray)
  }
}

object ByteArray {
  val empty: ByteArray = new ByteArray(new scala.Array[Byte](0))
  
  def apply(xs: Byte*): ByteArray = macro ArrayMacros.literalByteArray
}

final class ByteArrayBuffer extends Buffer[Any, Byte] {
  override type State = ByteArray
  
  private[this] var array: scala.Array[Byte] = ByteArray.empty.array
  
  private[this] var aliased: Boolean = true
  
  private[this] var length: Int = 0
  
  private[this] def expand(base: Int, size: Int): Int = {
    var n = (base max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
  
  private[this] def resize(size: Int) {
    val newArray = new scala.Array[Byte](size)
    java.lang.System.arraycopy(array, 0, newArray, 0, array.length min size)
    array = newArray
  }
  
  private[this] def prepare(size: Int) {
    if (aliased || size > array.length) {
      resize(expand(16, size))
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
      resize(length + count)
      aliased = false
    }
    this
  }
  
  override def state: ByteArray = {
    if (length != array.length) resize(length)
    aliased = true
    new ByteArray(array)
  }
  
  override def clear() {
    array = ByteArray.empty.array
    aliased = true
    length = 0
  }
}
