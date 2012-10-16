/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._

class FloatArray(val array: scala.Array[Float]) extends AnyVal with Array[Float] {
  override def length: Int = array.length
  
  override def apply(index: Int): Float = array(index)
  
  private[basis] def update(index: Int, value: Float): Unit = array(index) = value
  
  private[basis] def copy(length: Int): FloatArray = {
    val newArray = new scala.Array[Float](length)
    java.lang.System.arraycopy(array, 0, newArray, 0, array.length min length)
    new FloatArray(newArray)
  }
}

private[basis] object FloatArray {
  val empty: FloatArray = FloatArray(0)
  
  def apply(length: Int): FloatArray =
    new FloatArray(new scala.Array[Float](length))
}

final class FloatArrayBuffer extends Buffer[Any, Float] {
  override type State = FloatArray
  
  private[this] var array: FloatArray = FloatArray.empty
  
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
  
  override def += (value: Float): this.type = {
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
  
  override def state: FloatArray = {
    if (length != array.length) array = array.copy(length)
    aliased = true
    array
  }
  
  override def clear() {
    array = FloatArray.empty
    aliased = true
    length = 0
  }
}
