/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._

class RefArray[+A](val array: scala.Array[AnyRef]) extends AnyVal with Array[A] {
  override def isEmpty: Boolean = array.length == 0
  
  override def length: Int = array.length
  
  override def apply(index: Int): A = array(index).asInstanceOf[A]
  
  /** Returns a copy of this array with a new `value` at `index`. */
  def update[B >: A](index: Int, value: B): RefArray[B] = {
    val newArray = array.clone
    newArray(index) = value.asInstanceOf[AnyRef]
    new RefArray[B](newArray)
  }
  
  /** Returns a copy of this array with a new `value` inserted at `index`. */
  def insert[B >: A](index: Int, value: B): RefArray[B] = {
    val newArray = new scala.Array[AnyRef](array.length + 1)
    java.lang.System.arraycopy(array, 0, newArray, 0, index)
    newArray(index) = value.asInstanceOf[AnyRef]
    java.lang.System.arraycopy(array, index, newArray, index + 1, array.length - index)
    new RefArray[B](newArray)
  }
  
  /** Returns a copy of this array with `index` removed. */
  def remove(index: Int): RefArray[A] = {
    val newArray = new scala.Array[AnyRef](array.length - 1)
    java.lang.System.arraycopy(array, 0, newArray, 0, index)
    java.lang.System.arraycopy(array, index + 1, newArray, index, newArray.length - index)
    new RefArray[A](newArray)
  }
  
  /** Returns a copy of this array with `value` appended. */
  def :+ [B >: A](value: B): RefArray[B] = {
    val newArray = new scala.Array[AnyRef](array.length + 1)
    java.lang.System.arraycopy(array, 0, newArray, 0, array.length)
    newArray(newArray.length) = value.asInstanceOf[AnyRef]
    new RefArray[B](newArray)
  }
  
  /** Returns a copy of this array with `value` prepended. */
  def +: [B >: A](value: B): RefArray[B] = {
    val newArray = new scala.Array[AnyRef](array.length + 1)
    newArray(0) = value.asInstanceOf[AnyRef]
    java.lang.System.arraycopy(array, 0, newArray, 1, array.length)
    new RefArray[B](newArray)
  }
}

object RefArray {
  val empty: RefArray[Nothing] = new RefArray[Nothing](new scala.Array[AnyRef](0))
  
  def apply[A](xs: A*): RefArray[A] = macro ArrayMacros.literalRefArray[A]
}

final class RefArrayBuffer[A] extends Buffer[Any, A] {
  override type State = RefArray[A]
  
  private[this] var array: scala.Array[AnyRef] = RefArray.empty.array
  
  private[this] var aliased: Boolean = true
  
  private[this] var length: Int = 0
  
  private[this] def expand(base: Int, size: Int): Int = {
    var n = (base max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
  
  private[this] def resize(size: Int) {
    val newArray = new scala.Array[AnyRef](size)
    java.lang.System.arraycopy(array, 0, newArray, 0, array.length min size)
    array = newArray
  }
  
  private[this] def prepare(size: Int) {
    if (aliased || size > array.length) {
      resize(expand(16, size))
      aliased = false
    }
  }
  
  override def += (value: A): this.type = {
    prepare(length + 1)
    array(length) = value.asInstanceOf[AnyRef]
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
  
  override def state: RefArray[A] = {
    if (length != array.length) resize(length)
    aliased = true
    new RefArray[A](array)
  }
  
  override def clear() {
    array = RefArray.empty.array
    aliased = true
    length = 0
  }
}
