/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers
package immutable

import basis.collections._
import basis.util._

private[containers] final class RefArraySeq[+A](array: Array[AnyRef]) extends ArraySeq[A] {
  override def isEmpty: Boolean = array.length == 0
  
  override def length: Int = array.length
  
  override def apply(index: Int): A = array(index).asInstanceOf[A]
  
  override def update[B >: A](index: Int, value: B): ArraySeq[B] = {
    val newArray = array.clone
    newArray(index) = value.asInstanceOf[AnyRef]
    new RefArraySeq(newArray)
  }
  
  override def insert[B >: A](index: Int, value: B): ArraySeq[B] = {
    val newArray = new Array[AnyRef](array.length + 1)
    java.lang.System.arraycopy(array, 0, newArray, 0, index)
    newArray(index) = value.asInstanceOf[AnyRef]
    java.lang.System.arraycopy(array, index, newArray, index + 1, array.length - index)
    new RefArraySeq(newArray)
  }
  
  override def remove(index: Int): ArraySeq[A] = {
    val newArray = new Array[AnyRef](array.length - 1)
    java.lang.System.arraycopy(array, 0, newArray, 0, index)
    java.lang.System.arraycopy(array, index + 1, newArray, index, newArray.length - index)
    new RefArraySeq(newArray)
  }
  
  override def :+ [B >: A](value: B): ArraySeq[B] = {
    val newArray = new Array[AnyRef](array.length + 1)
    java.lang.System.arraycopy(array, 0, newArray, 0, array.length)
    newArray(newArray.length) = value.asInstanceOf[AnyRef]
    new RefArraySeq(newArray)
  }
  
  override def +: [B >: A](value: B): ArraySeq[B] = {
    val newArray = new Array[AnyRef](array.length + 1)
    newArray(0) = value.asInstanceOf[AnyRef]
    java.lang.System.arraycopy(array, 0, newArray, 1, array.length)
    new RefArraySeq(newArray)
  }
}

private[containers] final class RefArraySeqBuilder[A] extends Builder[Any, A, ArraySeq[A]] {
  private[this] var array: Array[AnyRef] = _
  
  private[this] var aliased: Boolean = true
  
  private[this] var length: Int = 0
  
  private[this] def expand(base: Int, size: Int): Int = {
    var n = (base max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
  
  private[this] def resize(size: Int) {
    val newArray = new Array[AnyRef](size)
    if (array != null) java.lang.System.arraycopy(array, 0, newArray, 0, array.length min size)
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
    if (array == null || length + count > array.length) {
      resize(length + count)
      aliased = false
    }
    this
  }
  
  override def state: ArraySeq[A] = {
    if (array == null || length != array.length) resize(length)
    aliased = true
    new RefArraySeq[A](array)
  }
  
  override def clear() {
    array = null
    aliased = true
    length = 0
  }
}
