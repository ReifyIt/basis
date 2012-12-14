/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.util._

private[containers] class IntArrayBuffer private (
    private[this] var array: Array[Int],
    private[this] var size: Int,
    private[this] var aliased: Boolean)
  extends ArrayBuffer[Int] {
  
  def this() = this(null, 0, true)
  
  final override def isEmpty: Boolean = array.length == 0
  
  final override def length: Int = size
  
  final override def apply(index: Int): Int = {
    if (index < 0 || index >= size)
      throw new IndexOutOfBoundsException(index.toString)
    array(index)
  }
  
  final override def update(index: Int, elem: Int) {
    if (index < 0 || index >= size)
      throw new IndexOutOfBoundsException(index.toString)
    array(index) = elem
  }
  
  final override def += (elem: Int): this.type = {
    var newArray = array
    if (aliased || size + 1 > newArray.length) {
      newArray = new Array[Int](expand(16, size + 1))
      if (array != null) java.lang.System.arraycopy(array, 0, newArray, 0, size)
      aliased = false
    }
    newArray(size) = elem
    array = newArray
    size += 1
    this
  }
  
  final override def ++= (elems: Enumerator[Int]): this.type = {
    if (elems.isInstanceOf[IntArraySeq]) {
      val xs = elems.asInstanceOf[IntArraySeq]
      var newArray = array
      if (aliased || size + xs.length > newArray.length) {
        newArray = new Array[Int](expand(16, size + xs.length))
        if (array != null) java.lang.System.arraycopy(array, 0, newArray, 0, size)
        aliased = false
      }
      xs.copyToArray(newArray, size)
      array = newArray
      size += xs.length
    }
    else super.++=(elems)
    this
  }
  
  final override def +=: (elem: Int): this.type = {
    var newArray = array
    if (aliased || size + 1 > newArray.length) {
      newArray = new Array[Int](expand(16, size + 1))
      aliased = false
    }
    if (array != null) java.lang.System.arraycopy(array, 0, newArray, 1, size)
    newArray(0) = elem
    array = newArray
    size += 1
    this
  }
  
  final override def ++=: (elems: Enumerator[Int]): this.type = {
    if (elems.isInstanceOf[IntArraySeq]) {
      val xs = elems.asInstanceOf[IntArraySeq]
      var newArray = array
      if (aliased || size + xs.length > newArray.length) {
        newArray = new Array[Int](expand(16, size + xs.length))
        aliased = false
      }
      if (array != null) java.lang.System.arraycopy(array, 0, newArray, xs.length, size)
      xs.copyToArray(newArray, 0)
      array = newArray
      size += xs.length
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
      var newArray = array
      if (aliased || size + 1 > newArray.length) {
        newArray = new Array[Int](expand(16, size + 1))
        aliased = false
      }
      if (array ne newArray) java.lang.System.arraycopy(array, 0, newArray, 0, index)
      java.lang.System.arraycopy(array, index, newArray, index + 1, size - index)
      newArray(index) = elem
      array = newArray
      size += 1
    }
  }
  
  final override def insertAll(index: Int, elems: Enumerator[Int]) {
    if (index < 0 || index > size) throw new IndexOutOfBoundsException(index.toString)
    if (index == size) this ++= elems
    else if (index == 0) elems ++=: this
    else if (elems.isInstanceOf[IntArraySeq]) {
      val xs = elems.asInstanceOf[IntArraySeq]
      var newArray = array
      if (aliased || size + xs.length > newArray.length) {
        newArray = new Array[Int](expand(16, size + xs.length))
        aliased = false
      }
      if (array ne newArray) java.lang.System.arraycopy(array, 0, newArray, 0, index)
      java.lang.System.arraycopy(array, index, newArray, index + xs.length, size - index)
      xs.copyToArray(newArray, index)
      array = newArray
      size += xs.length
    }
    else super.insertAll(index, elems)
  }
  
  final override def remove(index: Int): Int = {
    if (index < 0 || index >= size)
      throw new IndexOutOfBoundsException(index.toString)
    var newArray = array
    val x = newArray(index)
    if (aliased) {
      newArray = new Array[Int](expand(16, size - 1))
      aliased = false
    }
    if (array ne newArray) java.lang.System.arraycopy(array, 0, newArray, 0, index)
    java.lang.System.arraycopy(array, index + 1, newArray, index, size - index - 1)
    if (array eq newArray) newArray(size - 1) = 0
    size -= 1
    array = newArray
    x
  }
  
  final override def remove(index: Int, count: Int) {
    if (count < 0) throw new IllegalArgumentException("negative count")
    if (index < 0) throw new IndexOutOfBoundsException(index.toString)
    if (index + count > size) throw new IndexOutOfBoundsException((index + count).toString)
    var newArray = array
    if (aliased) {
      newArray = new Array[Int](expand(16, size - count))
      aliased = false
    }
    if (array ne newArray) java.lang.System.arraycopy(array, 0, newArray, 0, index)
    java.lang.System.arraycopy(array, index + count, newArray, index, size - index - count)
    if (array eq newArray) java.util.Arrays.fill(newArray, size - count, size, 0)
    size -= count
    array = newArray
  }
  
  final override def clear() {
    aliased = true
    size = 0
    array = null
  }
  
  final override def iterator: Iterator[Int] = new IntArrayBufferIterator(this)
  
  protected override def stringPrefix: String = "ArrayBuffer[Int]"
  
  private[containers] final def copy: ArrayBuffer[Int] = {
    aliased = true
    new IntArrayBuffer(array, size, aliased)
  }
  
  private[containers] final def toArraySeq: ArraySeq[Int] = {
    if (array == null || size != array.length) {
      val newArray = new Array[Int](size)
      if (array != null) java.lang.System.arraycopy(array, 0, newArray, 0, size)
      array = newArray
    }
    aliased = true
    new IntArraySeq(array)
  }
  
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
  
  override def dup: Iterator[Int] = new IntArrayBufferIterator(xs, elem, i, n)
}

private[containers] final class IntArrayBufferBuilder
  extends IntArrayBuffer with Builder[Any, Int] {
  
  override type State = ArrayBuffer[Int]
  
  override def expect(count: Int): this.type = this
  
  override def state: ArrayBuffer[Int] = copy
  
  protected override def stringPrefix: String = "ArrayBuffer.Builder[Int]"
}
