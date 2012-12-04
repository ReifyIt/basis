/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers
package mutable

import basis.collections._
import basis.util._

private[containers] final class RefArraySeq[A](array: Array[AnyRef]) extends ArraySeq[A] {
  override def isEmpty: Boolean = array.length == 0
  
  override def length: Int = array.length
  
  override def apply(index: Int): A = array(index).asInstanceOf[A]
  
  override def update(index: Int, value: A): Unit = array(index) = value.asInstanceOf[AnyRef]
  
  override def copyToArray(xs: Array[A], start: Int, count: Int) {
    if (xs.isInstanceOf[Array[AnyRef]])
      java.lang.System.arraycopy(array, 0, xs, start, count min (xs.length - start) min length)
    else super.copyToArray(xs, start, count)
  }
  
  override def copyToArray(xs: Array[A], start: Int) {
    if (xs.isInstanceOf[Array[AnyRef]])
      java.lang.System.arraycopy(array, 0, xs, start, (xs.length - start) min length)
    else super.copyToArray(xs, start)
  }
  
  override def copyToArray(xs: Array[A]) {
    if (xs.isInstanceOf[Array[AnyRef]])
      java.lang.System.arraycopy(array, 0, xs, 0, xs.length min length)
    else super.copyToArray(xs)
  }
  
  override def iterator: Iterator[A] = new RefArraySeqIterator(array)
}

private[containers] final class RefArraySeqIterator[A]
    (array: Array[AnyRef], private[this] var i: Int, n: Int)
  extends Iterator[A] {
  
  def this(array: Array[AnyRef]) = this(array, 0, array.length)
  
  override def isEmpty: Boolean = i >= n
  
  override def head: A = {
    if (i < n) array(i).asInstanceOf[A]
    else throw new NoSuchElementException("Head of empty iterator.")
  }
  
  override def step() {
    if (i < n) i += 1
    else throw new UnsupportedOperationException("Empty iterator step.")
  }
  
  override def dup: Iterator[A] = new RefArraySeqIterator(array, i, n)
}

private[containers] final class RefArraySeqBuilder[A] extends Builder[Any, A] {
  override type State = ArraySeq[A]
  
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
  
  override def ++= (xs: Enumerator[A]): this.type = xs match {
    case xs: RefArraySeq[A] =>
      prepare(length + xs.length)
      xs.copyToArray(array.asInstanceOf[Array[A]], length)
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
  
  override def state: ArraySeq[A] = {
    if (array == null || length != array.length) resize(length)
    aliased = true
    new RefArraySeq(array)
  }
  
  override def clear() {
    array = null
    aliased = true
    length = 0
  }
}
