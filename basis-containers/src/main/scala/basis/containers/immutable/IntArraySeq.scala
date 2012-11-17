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

private[containers] final class IntArraySeq(array: Array[Int]) extends ArraySeq[Int] {
  override def isEmpty: Boolean = array.length == 0
  
  override def length: Int = array.length
  
  override def apply(index: Int): Int = array(index)
  
  override def update[B >: Int](index: Int, value: B): ArraySeq[B] = value match {
    case value: Int =>
      val newArray = array.clone
      newArray(index) = value
      new IntArraySeq(newArray)
    case _ => super.update(index, value)
  }
  
  override def insert[B >: Int](index: Int, value: B): ArraySeq[B] = value match {
    case value: Int =>
      val newArray = new Array[Int](array.length + 1)
      java.lang.System.arraycopy(array, 0, newArray, 0, index)
      newArray(index) = value
      java.lang.System.arraycopy(array, index, newArray, index + 1, array.length - index)
      new IntArraySeq(newArray)
    case _ => super.insert(index, value)
  }
  
  override def remove(index: Int): ArraySeq[Int] = {
    val newArray = new Array[Int](array.length - 1)
    java.lang.System.arraycopy(array, 0, newArray, 0, index)
    java.lang.System.arraycopy(array, index + 1, newArray, index, newArray.length - index)
    new IntArraySeq(newArray)
  }
  
  override def :+ [B >: Int](value: B): ArraySeq[B] = value match {
    case value: Int =>
      val newArray = new Array[Int](array.length + 1)
      java.lang.System.arraycopy(array, 0, newArray, 0, array.length)
      newArray(newArray.length) = value
      new IntArraySeq(newArray)
    case _ => super.:+(value)
  }
  
  override def +: [B >: Int](value: B): ArraySeq[B] = value match {
    case value: Int =>
      val newArray = new Array[Int](array.length + 1)
      newArray(0) = value
      java.lang.System.arraycopy(array, 0, newArray, 1, array.length)
      new IntArraySeq(newArray)
    case _ => super.+:(value)
  }
}

private[containers] final class IntArraySeqBuilder extends Builder[Any, Int, ArraySeq[Int]] {
  private[this] var array: Array[Int] = _
  
  private[this] var aliased: Boolean = true
  
  private[this] var length: Int = 0
  
  private[this] def expand(base: Int, size: Int): Int = {
    var n = (base max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
  
  private[this] def resize(size: Int) {
    val newArray = new Array[Int](size)
    if (array != null) java.lang.System.arraycopy(array, 0, newArray, 0, array.length min size)
    array = newArray
  }
  
  private[this] def prepare(size: Int) {
    if (aliased || size > array.length) {
      resize(expand(16, size))
      aliased = false
    }
  }
  
  override def += (value: Int): this.type = {
    prepare(length + 1)
    array(length) = value
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
  
  override def state: ArraySeq[Int] = {
    if (array == null || length != array.length) resize(length)
    aliased = true
    new IntArraySeq(array)
  }
  
  override def clear() {
    array = null
    aliased = true
    length = 0
  }
}
