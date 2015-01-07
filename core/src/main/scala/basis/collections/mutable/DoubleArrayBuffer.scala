//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package mutable

import basis._
import basis.collections.generic._
import basis.collections.immutable._
import basis.util._
import scala.reflect._

private[collections] class DoubleArrayBuffer private (
    private[this] var buffer: Array[Double],
    private[this] var size: Int,
    private[this] var aliased: Boolean)
  extends ArrayBuffer[Double] {

  def this() = this(null, 0, true)

  override def erasure: ClassTag[_] = ClassTag.Double

  override def length: Int = size

  override def apply(index: Int): Double = {
    if (index < 0 || index >= size) throw new IndexOutOfBoundsException(index.toString)
    buffer(index)
  }

  override def update(index: Int, elem: Double): Unit = {
    if (index < 0 || index >= size) throw new IndexOutOfBoundsException(index.toString)
    buffer(index) = elem
  }

  override def append(elem: Double): Unit = {
    var array = buffer
    if (aliased || size + 1 > array.length) {
      array = new Array[Double](expand(size + 1))
      if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, 0, size)
      buffer = array
      aliased = false
    }
    array(size) = elem
    size += 1
  }

  override def appendAll(elems: Traverser[Double]): Unit = {
    if (elems.isInstanceOf[ArrayLike[_]]) {
      val xs = elems.asInstanceOf[ArrayLike[Double]]
      val n = xs.length
      var array = buffer
      if (aliased || size + n > array.length) {
        array = new Array[Double](expand(size + n))
        if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, 0, size)
        buffer = array
        aliased = false
      }
      xs.copyToArray(0, array, size, n)
      size += n
    }
    else appendAll(ArrayBuffer.coerce(elems))
  }

  override def appendArray(elems: Array[Double]): Unit = {
    val n = elems.length
    var array = buffer
    if (aliased || size + n > array.length) {
      array = new Array[Double](expand(size + n))
      if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, 0, size)
      buffer = array
      aliased = false
    }
    java.lang.System.arraycopy(elems, 0, array, size, n)
    size += n
  }

  override def prepend(elem: Double): Unit = {
    var array = buffer
    if (aliased || size + 1 > array.length) array = new Array[Double](expand(1 + size))
    if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, 1, size)
    array(0) = elem
    buffer = array
    size += 1
    aliased = false
  }

  override def prependAll(elems: Traverser[Double]): Unit = {
    if (elems.isInstanceOf[ArrayLike[_]]) {
      val xs = elems.asInstanceOf[ArrayLike[Double]]
      val n = xs.length
      var array = buffer
      if (aliased || size + n > array.length) array = new Array[Double](expand(n + size))
      if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, n, size)
      xs.copyToArray(0, array, 0, n)
      buffer = array
      size += n
      aliased = false
    }
    else prependAll(ArrayBuffer.coerce(elems))
  }

  override def prependArray(elems: Array[Double]): Unit = {
    val n = elems.length
    var array = buffer
    if (aliased || size + n > array.length) array = new Array[Double](expand(n + size))
    if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, n, size)
    java.lang.System.arraycopy(elems, 0, array, 0, n)
    buffer = array
    size += n
    aliased = false
  }

  override def insert(index: Int, elem: Double): Unit = {
    if (index == size) append(elem)
    else if (index == 0) prepend(elem)
    else if (index < 0 || index > size) throw new IndexOutOfBoundsException(index.toString)
    else {
      var array = buffer
      if (aliased || size + 1 > array.length) {
        array = new Array[Double](expand(size + 1))
        java.lang.System.arraycopy(buffer, 0, array, 0, index)
      }
      java.lang.System.arraycopy(buffer, index, array, index + 1, size - index)
      array(index) = elem
      buffer = array
      size += 1
      aliased = false
    }
  }

  override def insertAll(index: Int, elems: Traverser[Double]): Unit = {
    if (index == size) appendAll(elems)
    else if (index == 0) prependAll(elems)
    else if (index < 0 || index > size) throw new IndexOutOfBoundsException(index.toString)
    else if (elems.isInstanceOf[ArrayLike[_]]) {
      val xs = elems.asInstanceOf[ArrayLike[Double]]
      val n = xs.length
      var array = buffer
      if (aliased || size + n > array.length) {
        array = new Array[Double](expand(size + n))
        java.lang.System.arraycopy(buffer, 0, array, 0, index)
      }
      java.lang.System.arraycopy(buffer, index, array, index + n, size - index)
      xs.copyToArray(0, array, index, n)
      buffer = array
      size += n
      aliased = false
    }
    else insertAll(index, ArrayBuffer.coerce(elems))
  }

  override def insertArray(index: Int, elems: Array[Double]): Unit = {
    if (index == size) appendArray(elems)
    else if (index == 0) prependArray(elems)
    else if (index < 0 || index > size) throw new IndexOutOfBoundsException(index.toString)
    else {
      val n = elems.length
      var array = buffer
      if (aliased || size + n > array.length) {
        array = new Array[Double](expand(size + n))
        java.lang.System.arraycopy(buffer, 0, array, 0, index)
      }
      java.lang.System.arraycopy(buffer, index, array, index + n, size - index)
      java.lang.System.arraycopy(elems, 0, array, index, n)
      buffer = array
      size += n
      aliased = false
    }
  }

  override def remove(index: Int): Double = {
    if (index < 0 || index >= size) throw new IndexOutOfBoundsException(index.toString)
    var array = buffer
    val x = array(index)
    if (size == 1) clear()
    else {
      if (aliased) {
        array = new Array[Double](expand(size - 1))
        java.lang.System.arraycopy(buffer, 0, array, 0, index)
      }
      java.lang.System.arraycopy(buffer, index + 1, array, index, size - index - 1)
      if (buffer eq array) array(size - 1) = 0.0
      size -= 1
      buffer = array
      aliased = false
    }
    x
  }

  override def remove(index: Int, count: Int): Unit = {
    if (count < 0) throw new IllegalArgumentException("negative count")
    if (index < 0) throw new IndexOutOfBoundsException(index.toString)
    if (index + count > size) throw new IndexOutOfBoundsException((index + count).toString)
    if (size == count) clear()
    else {
      var array = buffer
      if (aliased) {
        array = new Array[Double](expand(size - count))
        java.lang.System.arraycopy(buffer, 0, array, 0, index)
      }
      java.lang.System.arraycopy(buffer, index + count, array, index, size - index - count)
      if (buffer eq array) java.util.Arrays.fill(array, size - count, size, 0.0)
      size -= count
      buffer = array
      aliased = false
    }
  }

  override def clear(): Unit = {
    aliased = true
    size = 0
    buffer = null
  }

  override def state: State = throw new UnsupportedOperationException

  override def copy: ArrayBuffer[Double] = {
    aliased = true
    new DoubleArrayBuffer(buffer, size, aliased)
  }

  override def copyToArray[B >: Double](index: Int, to: Array[B], offset: Int, count: Int): Unit = {
    if (to.isInstanceOf[Array[Double]]) java.lang.System.arraycopy(buffer, index, to, offset, count)
    else super.copyToArray(index, to, offset, count)
  }

  override def toArray[B >: Double](implicit B: ClassTag[B]): Array[B] = {
    if (B == ClassTag.Double) {
      val array = new Array[Double](size)
      java.lang.System.arraycopy(buffer, 0, array, 0, size)
      array.asInstanceOf[Array[B]]
    }
    else super.toArray
  }

  override def toArraySeq: ArraySeq[Double] = {
    if (buffer == null || size != buffer.length) {
      val array = new Array[Double](size)
      if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, 0, size)
      buffer = array
    }
    aliased = true
    new DoubleArraySeq(buffer)
  }

  override def expect(count: Int): this.type = {
    if (buffer == null || size + count > buffer.length) {
      val array = new Array[Double](size + count)
      if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, 0, size)
      buffer = array
      aliased = false
    }
    this
  }

  override def iterator: Iterator[Double] = new DoubleArrayBufferIterator(this)

  protected def defaultSize: Int = 16

  private[this] def expand(size: Int): Int = {
    var n = (defaultSize max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
}

private[collections] final class DoubleArrayBufferIterator private (
    private[this] val b: DoubleArrayBuffer,
    private[this] var i: Int,
    private[this] var n: Int,
    private[this] var x: Double)
  extends Iterator[Double] {

  def this(b: DoubleArrayBuffer) = this(b, 0, b.length, if (b.length > 0) b(0) else 0.0)

  override def isEmpty: Boolean = i >= n

  override def head: Double = {
    if (i >= n) Iterator.empty.head
    x
  }

  override def step(): Unit = {
    if (i >= n) Iterator.empty.step()
    i += 1
    n = b.length
    x = if (i < n) b(i) else 0.0
  }

  override def dup: Iterator[Double] = new DoubleArrayBufferIterator(b, i, n, x)
}

private[collections] final class DoubleArrayBufferBuilder extends DoubleArrayBuffer with State[ArrayBuffer[Double]] {
  override def state: ArrayBuffer[Double] = copy
  override def toString: String = "ArrayBuffer"+"."+"Builder"+"["+"Double"+"]"+"()"
}
