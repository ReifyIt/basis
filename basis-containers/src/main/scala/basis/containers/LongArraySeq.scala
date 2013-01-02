/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.runtime._

/** An immutable contiguous `Long` array.
  * 
  * @author Chris Sachs
  * @since  0.0
  */
private[containers] final class LongArraySeq(array: Array[Long]) extends ArraySeq[Long] with Reified {
  protected override def T: TypeHint[Long] = TypeHint.Long
  
  override def isEmpty: Boolean = array.length == 0
  
  override def length: Int = array.length
  
  override def apply(index: Int): Long = array(index)
  
  override def update[B >: Long](index: Int, elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Long]) {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      val newArray = new Array[Long](length)
      java.lang.System.arraycopy(array, 0, newArray, 0, newArray.length)
      newArray(index) = elem.asInstanceOf[Long]
      new LongArraySeq(newArray).asInstanceOf[ArraySeq[B]]
    }
    else super.update(index, elem)
  }
  
  override def append[B >: Long](elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Long]) {
      val newArray = new Array[Long](length + 1)
      java.lang.System.arraycopy(array, 0, newArray, 0, length)
      newArray(newArray.length) = elem.asInstanceOf[Long]
      new LongArraySeq(newArray)
    }
    else super.append(elem)
  }
  
  override def appendAll[B >: Long](elems: Enumerator[B]): ArraySeq[B] = {
    if (elems.isInstanceOf[ArrayLike[_]] && Reified[Long](elems)) {
      val xs = elems.asInstanceOf[ArrayLike[Long]]
      val n = xs.length
      val newArray = new Array[Long](length + n)
      java.lang.System.arraycopy(array, 0, newArray, 0, length)
      xs.copyToArray(0, newArray, length, n)
      new LongArraySeq(newArray)
    }
    else super.appendAll(elems)
  }
  
  override def prepend[B >: Long](elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Long]) {
      val newArray = new Array[Long](1 + length)
      newArray(0) = elem.asInstanceOf[Long]
      java.lang.System.arraycopy(array, 0, newArray, 1, length)
      new LongArraySeq(newArray)
    }
    else super.prepend(elem)
  }
  
  override def prependAll[B >: Long](elems: Enumerator[B]): ArraySeq[B] = {
    if (elems.isInstanceOf[ArrayLike[_]] && Reified[Long](elems)) {
      val xs = elems.asInstanceOf[ArrayLike[Long]]
      val n = xs.length
      val newArray = new Array[Long](n + length)
      xs.copyToArray(0, newArray, 0, n)
      java.lang.System.arraycopy(array, 0, newArray, n, length)
      new LongArraySeq(newArray)
    }
    else super.prependAll(elems)
  }
  
  override def insert[B >: Long](index: Int, elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Long]) {
      if (index < 0 || index > length) throw new IndexOutOfBoundsException(index.toString)
      val newArray = new Array[Long](length + 1)
      java.lang.System.arraycopy(array, 0, newArray, 0, index)
      newArray(index) = elem.asInstanceOf[Long]
      java.lang.System.arraycopy(array, index, newArray, index + 1, length - index)
      new LongArraySeq(newArray)
    }
    else super.insert(index, elem)
  }
  
  override def insertAll[B >: Long](index: Int, elems: Enumerator[B]): ArraySeq[B] = {
    if (elems.isInstanceOf[ArrayLike[_]] && Reified[Long](elems)) {
      val xs = elems.asInstanceOf[ArrayLike[Long]]
      val n = xs.length
      if (index < 0 || index > length) throw new IndexOutOfBoundsException(index.toString)
      val newArray = new Array[Long](length + n)
      java.lang.System.arraycopy(array, 0, newArray, 0, index)
      xs.copyToArray(0, newArray, index, n)
      java.lang.System.arraycopy(array, index, newArray, index + n, length - index)
      new LongArraySeq(newArray)
    }
    else super.insertAll(index, elems)
  }
  
  override def remove(index: Int): ArraySeq[Long] = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    val newArray = new Array[Long](length - 1)
    java.lang.System.arraycopy(array, 0, newArray, 0, index)
    java.lang.System.arraycopy(array, index + 1, newArray, index, newArray.length - index)
    new LongArraySeq(newArray)
  }
  
  override def remove(index: Int, count: Int): ArraySeq[Long] = {
    if (count < 0) throw new IllegalArgumentException("negative count")
    if (index < 0) throw new IndexOutOfBoundsException(index.toString)
    if (index + count > length) throw new IndexOutOfBoundsException((index + count).toString)
    val newArray = new Array[Long](length - count)
    java.lang.System.arraycopy(array, 0, newArray, 0, index)
    java.lang.System.arraycopy(array, index + count, newArray, index, newArray.length - index)
    new LongArraySeq(newArray)
  }
  
  override def copyToArray[B >: Long](index: Int, to: Array[B], offset: Int, count: Int) {
    if (to.isInstanceOf[Array[Long]]) java.lang.System.arraycopy(array, index, to, offset, count)
    else super.copyToArray(index, to, offset, count)
  }
  
  override def toArray[B >: Long](implicit B: scala.reflect.ClassTag[B]): Array[B] = {
    if (B == scala.reflect.ClassTag.Long) {
      val newArray = new Array[Long](length)
      java.lang.System.arraycopy(array, 0, newArray, 0, newArray.length)
      newArray.asInstanceOf[Array[B]]
    }
    else super.toArray
  }
  
  override def iterator: Iterator[Long] = new LongArraySeqIterator(array)
}

private[containers] final class LongArraySeqIterator
    (array: Array[Long], private[this] var i: Int, n: Int)
  extends Iterator[Long] {
  
  def this(array: Array[Long]) = this(array, 0, array.length)
  
  override def isEmpty: Boolean = i >= n
  
  override def head: Long = {
    if (i >= n) throw new NoSuchElementException("Head of empty iterator.")
    array(i)
  }
  
  override def step() {
    if (i >= n) throw new UnsupportedOperationException("Empty iterator step.")
    i += 1
  }
  
  override def dup: Iterator[Long] = new LongArraySeqIterator(array, i, n)
}

private[containers] final class LongArraySeqBuilder
  extends LongArrayBuffer with Builder[Any, Long] {
  override type State = ArraySeq[Long]
  override def state: ArraySeq[Long] = toArraySeq
}
