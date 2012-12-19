/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.util._

import scala.reflect.ClassTag

private[containers] class ByteArrayBuffer private (
    private[this] var buffer: Array[Byte],
    private[this] var size: Int,
    private[this] var aliased: Boolean)
  extends ArrayBuffer[Byte] {
  
  def this() = this(null, 0, true)
  
  final override def isEmpty: Boolean = size == 0
  
  final override def length: Int = size
  
  final override def apply(index: Int): Byte = {
    if (index < 0 || index >= size) throw new IndexOutOfBoundsException(index.toString)
    buffer(index)
  }
  
  final override def update(index: Int, elem: Byte) {
    if (index < 0 || index >= size) throw new IndexOutOfBoundsException(index.toString)
    buffer(index) = elem
  }
  
  final override def append(elem: Byte) {
    var array = buffer
    if (aliased || size + 1 > array.length) {
      array = new Array[Byte](expand(size + 1))
      if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, 0, size)
      buffer = array
      aliased = false
    }
    array(size) = elem
    size += 1
  }
  
  final override def appendAll(elems: Enumerator[Byte]) {
    if (elems.isInstanceOf[ArrayLike[_]]) {
      val xs = elems.asInstanceOf[ArrayLike[Byte]]
      val n = xs.length
      var array = buffer
      if (aliased || size + n > array.length) {
        array = new Array[Byte](expand(size + n))
        if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, 0, size)
        buffer = array
        aliased = false
      }
      xs.copyToArray(array, size)
      size += n
    }
    else super.appendAll(elems)
  }
  
  final override def prepend(elem: Byte) {
    var array = buffer
    if (aliased || size + 1 > array.length)
      array = new Array[Byte](expand(size + 1))
    if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, 1, size)
    array(0) = elem
    buffer = array
    size += 1
    aliased = false
  }
  
  final override def prependAll(elems: Enumerator[Byte]) {
    if (elems.isInstanceOf[ArrayLike[_]]) {
      val xs = elems.asInstanceOf[ArrayLike[Byte]]
      val n = xs.length
      var array = buffer
      if (aliased || size + n > array.length)
        array = new Array[Byte](expand(size + n))
      if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, n, size)
      xs.copyToArray(array, 0)
      buffer = array
      size += n
      aliased = false
    }
    else super.prependAll(elems)
  }
  
  final override def insert(index: Int, elem: Byte) {
    if (index < 0 || index > size) throw new IndexOutOfBoundsException(index.toString)
    if (index == size) append(elem)
    else if (index == 0) prepend(elem)
    else {
      var array = buffer
      if (aliased || size + 1 > array.length) {
        array = new Array[Byte](expand(size + 1))
        java.lang.System.arraycopy(buffer, 0, array, 0, index)
      }
      java.lang.System.arraycopy(buffer, index, array, index + 1, size - index)
      array(index) = elem
      buffer = array
      size += 1
      aliased = false
    }
  }
  
  final override def insertAll(index: Int, elems: Enumerator[Byte]) {
    if (index < 0 || index > size) throw new IndexOutOfBoundsException(index.toString)
    if (index == size) appendAll(elems)
    else if (index == 0) prependAll(elems)
    else if (elems.isInstanceOf[ArrayLike[_]]) {
      val xs = elems.asInstanceOf[ArrayLike[Byte]]
      val n = xs.length
      var array = buffer
      if (aliased || size + n > array.length) {
        array = new Array[Byte](expand(size + n))
        java.lang.System.arraycopy(buffer, 0, array, 0, index)
      }
      java.lang.System.arraycopy(buffer, index, array, index + n, size - index)
      xs.copyToArray(array, index)
      buffer = array
      size += n
      aliased = false
    }
    else super.insertAll(index, elems)
  }
  
  final override def remove(index: Int): Byte = {
    if (index < 0 || index >= size) throw new IndexOutOfBoundsException(index.toString)
    var array = buffer
    val x = array(index)
    if (size == 1) clear()
    else {
      if (aliased) {
        array = new Array[Byte](expand(size - 1))
        java.lang.System.arraycopy(buffer, 0, array, 0, index)
      }
      java.lang.System.arraycopy(buffer, index + 1, array, index, size - index - 1)
      if (buffer eq array) array(size - 1) = 0.toByte
      size -= 1
      buffer = array
      aliased = false
    }
    x
  }
  
  final override def remove(index: Int, count: Int) {
    if (count < 0) throw new IllegalArgumentException("negative count")
    if (index < 0) throw new IndexOutOfBoundsException(index.toString)
    if (index + count > size) throw new IndexOutOfBoundsException((index + count).toString)
    if (size == count) clear()
    else {
      var array = buffer
      if (aliased) {
        array = new Array[Byte](expand(size - count))
        java.lang.System.arraycopy(buffer, 0, array, 0, index)
      }
      java.lang.System.arraycopy(buffer, index + count, array, index, size - index - count)
      if (buffer eq array) java.util.Arrays.fill(array, size - count, size, 0.toByte)
      size -= count
      buffer = array
      aliased = false
    }
  }
  
  final override def clear() {
    aliased = true
    size = 0
    buffer = null
  }
  
  final override def copy: ArrayBuffer[Byte] = {
    aliased = true
    new ByteArrayBuffer(buffer, size, aliased)
  }
  
  final override def copyToArray[B >: Byte](xs: Array[B], start: Int, count: Int) {
    if (xs.isInstanceOf[Array[Byte]])
      java.lang.System.arraycopy(buffer, 0, xs, start, count min (xs.length - start) min size)
    else super.copyToArray(xs, start, count)
  }
  
  final override def copyToArray[B >: Byte](xs: Array[B], start: Int) {
    if (xs.isInstanceOf[Array[Byte]])
      java.lang.System.arraycopy(buffer, 0, xs, start, (xs.length - start) min size)
    else super.copyToArray(xs, start)
  }
  
  final override def copyToArray[B >: Byte](xs: Array[B]) {
    if (xs.isInstanceOf[Array[Byte]])
      java.lang.System.arraycopy(buffer, 0, xs, 0, xs.length min size)
    else super.copyToArray(xs)
  }
  
  final override def toArray[B >: Byte](implicit B: ClassTag[B]): Array[B] = {
    if (B == ClassTag.Byte) {
      val xs = new Array[Byte](size)
      java.lang.System.arraycopy(buffer, 0, xs, 0, size)
      xs.asInstanceOf[Array[B]]
    }
    else super.toArray[B]
  }
  
  final override def toArraySeq: ArraySeq[Byte] = {
    if (buffer == null || size != buffer.length) {
      var array = new Array[Byte](size)
      if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, 0, size)
      buffer = array
    }
    aliased = true
    new ByteArraySeq(buffer)
  }
  
  override def expect(count: Int): this.type = {
    if (buffer == null || size + count > buffer.length) {
      var array = new Array[Byte](size + count)
      if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, 0, size)
      buffer = array
    }
    this
  }
  
  protected def defaultSize: Int = 16
  
  final override def iterator: Iterator[Byte] = new ByteArrayBufferIterator(this)
  
  private[this] def expand(size: Int): Int = {
    var n = (defaultSize max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
}

private[containers] final class ByteArrayBufferIterator private (
    private[this] val b: ByteArrayBuffer,
    private[this] var i: Int,
    private[this] var n: Int,
    private[this] var x: Byte)
  extends Iterator[Byte] {
  
  def this(b: ByteArrayBuffer) = this(b, 0, b.length, if (!b.isEmpty) b(0) else 0.toByte)
  
  override def isEmpty: Boolean = i >= n
  
  override def head: Byte = {
    if (i >= n) throw new NoSuchElementException("Head of empty iterator.")
    x
  }
  
  override def step() {
    if (i >= n) throw new UnsupportedOperationException("Empty iterator step.")
    i += 1
    n = b.length
    x = if (i < n) b(i) else 0.toByte
  }
  
  override def dup: Iterator[Byte] = new ByteArrayBufferIterator(b, i, n, x)
}

private[containers] final class ByteArrayBufferBuilder
  extends ByteArrayBuffer with Builder[Any, Byte] {
  override type State = ArrayBuffer[Byte]
  override def state: ArrayBuffer[Byte] = copy
}
