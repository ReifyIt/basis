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

private[containers] class IntArrayBuffer private (
    private[this] var buffer: Array[Int],
    private[this] var size: Int,
    private[this] var aliased: Boolean)
  extends ArrayBuffer[Int] {
  
  def this() = this(null, 0, true)
  
  final override def isEmpty: Boolean = size == 0
  
  final override def length: Int = size
  
  final override def apply(index: Int): Int = {
    if (index < 0 || index >= size) throw new IndexOutOfBoundsException(index.toString)
    buffer(index)
  }
  
  final override def update(index: Int, elem: Int) {
    if (index < 0 || index >= size) throw new IndexOutOfBoundsException(index.toString)
    buffer(index) = elem
  }
  
  final override def += (elem: Int): this.type = {
    var array = buffer
    if (aliased || size + 1 > array.length) {
      array = new Array[Int](expand(16, size + 1))
      if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, 0, size)
      buffer = array
      aliased = false
    }
    array(size) = elem
    size += 1
    this
  }
  
  final override def ++= (elems: Enumerator[Int]): this.type = {
    if (elems.isInstanceOf[ArrayLike[_]]) {
      val xs = elems.asInstanceOf[ArrayLike[Int]]
      val n = xs.length
      var array = buffer
      if (aliased || size + n > array.length) {
        array = new Array[Int](expand(16, size + n))
        if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, 0, size)
        buffer = array
        aliased = false
      }
      xs.copyToArray(array, size)
      size += n
    }
    else super.++=(elems)
    this
  }
  
  final override def +=: (elem: Int): this.type = {
    var array = buffer
    if (aliased || size + 1 > array.length)
      array = new Array[Int](expand(16, size + 1))
    if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, 1, size)
    array(0) = elem
    buffer = array
    size += 1
    aliased = false
    this
  }
  
  final override def ++=: (elems: Enumerator[Int]): this.type = {
    if (elems.isInstanceOf[ArrayLike[_]]) {
      val xs = elems.asInstanceOf[ArrayLike[Int]]
      val n = xs.length
      var array = buffer
      if (aliased || size + n > array.length)
        array = new Array[Int](expand(16, size + n))
      if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, n, size)
      xs.copyToArray(array, 0)
      buffer = array
      size += n
      aliased = false
    }
    else super.++=:(elems)
    this
  }
  
  final override def -= (elem: Int): this.type = {
    var i = 0
    while (i < size) {
      if (elem == this(i)) {
        remove(i)
        return this
      }
      i += 1
    }
    this
  }
  
  final override def insert(index: Int, elem: Int) {
    if (index < 0 || index > size) throw new IndexOutOfBoundsException(index.toString)
    if (index == size) this += elem
    else if (index == 0) elem +=: this
    else {
      var array = buffer
      if (aliased || size + 1 > array.length) {
        array = new Array[Int](expand(16, size + 1))
        java.lang.System.arraycopy(buffer, 0, array, 0, index)
      }
      java.lang.System.arraycopy(buffer, index, array, index + 1, size - index)
      array(index) = elem
      buffer = array
      size += 1
      aliased = false
    }
  }
  
  final override def insertAll(index: Int, elems: Enumerator[Int]) {
    if (index < 0 || index > size) throw new IndexOutOfBoundsException(index.toString)
    if (index == size) this ++= elems
    else if (index == 0) elems ++=: this
    else if (elems.isInstanceOf[ArrayLike[_]]) {
      val xs = elems.asInstanceOf[ArrayLike[Int]]
      val n = xs.length
      var array = buffer
      if (aliased || size + n > array.length) {
        array = new Array[Int](expand(16, size + n))
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
  
  final override def remove(index: Int): Int = {
    if (index < 0 || index >= size) throw new IndexOutOfBoundsException(index.toString)
    var array = buffer
    val x = array(index)
    if (aliased) {
      array = new Array[Int](expand(16, size - 1))
      java.lang.System.arraycopy(buffer, 0, array, 0, index)
    }
    java.lang.System.arraycopy(buffer, index + 1, array, index, size - index - 1)
    if (buffer eq array) array(size - 1) = 0
    size -= 1
    buffer = array
    aliased = false
    x
  }
  
  final override def remove(index: Int, count: Int) {
    if (count < 0) throw new IllegalArgumentException("negative count")
    if (index < 0) throw new IndexOutOfBoundsException(index.toString)
    if (index + count > size) throw new IndexOutOfBoundsException((index + count).toString)
    var array = buffer
    if (aliased) {
      array = new Array[Int](expand(16, size - count))
      java.lang.System.arraycopy(buffer, 0, array, 0, index)
    }
    java.lang.System.arraycopy(buffer, index + count, array, index, size - index - count)
    if (buffer eq array) java.util.Arrays.fill(array, size - count, size, 0)
    size -= count
    buffer = array
    aliased = false
  }
  
  final override def clear() {
    aliased = true
    size = 0
    buffer = null
  }
  
  final override def copyToArray[B >: Int](xs: Array[B], start: Int, count: Int) {
    if (xs.isInstanceOf[Array[Int]])
      java.lang.System.arraycopy(buffer, 0, xs, start, count min (xs.length - start) min size)
    else super.copyToArray(xs, start, count)
  }
  
  final override def copyToArray[B >: Int](xs: Array[B], start: Int) {
    if (xs.isInstanceOf[Array[Int]])
      java.lang.System.arraycopy(buffer, 0, xs, start, (xs.length - start) min size)
    else super.copyToArray(xs, start)
  }
  
  final override def copyToArray[B >: Int](xs: Array[B]) {
    if (xs.isInstanceOf[Array[Int]])
      java.lang.System.arraycopy(buffer, 0, xs, 0, xs.length min size)
    else super.copyToArray(xs)
  }
  
  final override def toArray[B >: Int](implicit B: ClassTag[B]): Array[B] = {
    if (B == ClassTag.Int) {
      val xs = new Array[Int](size)
      java.lang.System.arraycopy(buffer, 0, xs, 0, size)
      xs.asInstanceOf[Array[B]]
    }
    else super.toArray[B]
  }
  
  private[containers] final def toArraySeq: ArraySeq[Int] = {
    if (buffer == null || buffer.length != size) {
      val array = new Array[Int](size)
      if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, 0, size)
      buffer = array
    }
    aliased = true
    new IntArraySeq(buffer)
  }
  
  private[containers] final def copy: ArrayBuffer[Int] = {
    aliased = true
    new IntArrayBuffer(buffer, size, aliased)
  }
  
  final def expect(count: Int): this.type = {
    if (buffer == null || size + count > buffer.length) {
      val array = new Array[Int](size + count)
      if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, 0, size)
      buffer = array
    }
    this
  }
  
  final override def iterator: Iterator[Int] = new IntArrayBufferIterator(this)
  
  protected override def stringPrefix: String = "ArrayBuffer[Int]"
  
  private[this] def expand(base: Int, size: Int): Int = {
    var n = (base max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
}

private[containers] final class IntArrayBufferIterator private (
    private[this] val xs: IntArrayBuffer,
    private[this] var i: Int,
    private[this] var n: Int,
    private[this] var elem: Int)
  extends Iterator[Int] {
  
  def this(xs: IntArrayBuffer) = this(xs, 0, xs.length, if (!xs.isEmpty) xs(0) else 0)
  
  override def isEmpty: Boolean = i >= n
  
  override def head: Int = {
    if (i >= n) throw new NoSuchElementException("Head of empty iterator.")
    elem
  }
  
  override def step() {
    if (i >= n) throw new UnsupportedOperationException("Empty iterator step.")
    i += 1
    n = xs.length
    elem = if (i < n) xs(i) else 0
  }
  
  override def dup: Iterator[Int] = new IntArrayBufferIterator(xs, i, n, elem)
}

private[containers] final class IntArrayBufferBuilder
  extends IntArrayBuffer with Builder[Any, Int] {
  
  override type State = ArrayBuffer[Int]
  
  override def state: ArrayBuffer[Int] = copy
  
  protected override def stringPrefix: String = "ArrayBuffer.Builder[Int]"
}
