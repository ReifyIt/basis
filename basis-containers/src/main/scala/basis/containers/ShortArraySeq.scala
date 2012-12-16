/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.util._

private[containers] final class ShortArraySeq(array: Array[Short]) extends ArraySeq[Short] {
  override def isEmpty: Boolean = array.length == 0
  
  override def length: Int = array.length
  
  override def apply(index: Int): Short = array(index)
  
  override def copyToArray[B >: Short](xs: Array[B], start: Int, count: Int) {
    if (xs.isInstanceOf[Array[Short]])
      java.lang.System.arraycopy(array, 0, xs, start, count min (xs.length - start) min length)
    else super.copyToArray(xs, start, count)
  }
  
  override def copyToArray[B >: Short](xs: Array[B], start: Int) {
    if (xs.isInstanceOf[Array[Short]])
      java.lang.System.arraycopy(array, 0, xs, start, (xs.length - start) min length)
    else super.copyToArray(xs, start)
  }
  
  override def copyToArray[B >: Short](xs: Array[B]) {
    if (xs.isInstanceOf[Array[Short]])
      java.lang.System.arraycopy(array, 0, xs, 0, xs.length min length)
    else super.copyToArray(xs)
  }
  
  override def iterator: Iterator[Short] = new ShortArraySeqIterator(array)
}

private[containers] final class ShortArraySeqIterator
    (array: Array[Short], private[this] var i: Int, n: Int)
  extends Iterator[Short] {
  
  def this(array: Array[Short]) = this(array, 0, array.length)
  
  override def isEmpty: Boolean = i >= n
  
  override def head: Short = {
    if (i < n) array(i)
    else throw new NoSuchElementException("Head of empty iterator.")
  }
  
  override def step() {
    if (i < n) i += 1
    else throw new UnsupportedOperationException("Empty iterator step.")
  }
  
  override def dup: Iterator[Short] = new ShortArraySeqIterator(array, i, n)
}

private[containers] final class ShortArraySeqBuilder extends Builder[Any, Short] {
  override type State = ArraySeq[Short]
  
  private[this] var array: Array[Short] = _
  
  private[this] var aliased: Boolean = true
  
  private[this] var length: Int = 0
  
  private[this] def expand(base: Int, size: Int): Int = {
    var n = (base max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
  
  private[this] def resize(size: Int) {
    val newArray = new Array[Short](size)
    if (array != null) java.lang.System.arraycopy(array, 0, newArray, 0, array.length min size)
    array = newArray
  }
  
  private[this] def prepare(size: Int) {
    if (aliased || size > array.length) {
      resize(expand(16, size))
      aliased = false
    }
  }
  
  override def += (value: Short): this.type = {
    prepare(length + 1)
    array(length) = value
    length += 1
    this
  }
  
  override def ++= (xs: Enumerator[Short]): this.type = xs match {
    case xs: ShortArraySeq =>
      prepare(length + xs.length)
      xs.copyToArray(array, length)
      length += xs.length
      this
    case _ => super.++=(xs)
  }
  
  override def expect(count: Int): this.type = {
    if (array == null || length + count > array.length) {
      resize(length + count)
      aliased = false
    }
    this
  }
  
  override def state: ArraySeq[Short] = {
    if (array == null || length != array.length) resize(length)
    aliased = true
    new ShortArraySeq(array)
  }
  
  override def clear() {
    array = null
    aliased = true
    length = 0
  }
}
