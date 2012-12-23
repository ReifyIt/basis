/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.runtime._

private[containers] final class ShortArraySeq(array: Array[Short]) extends ArraySeq[Short] with Reified {
  protected override def T: TypeHint[Short] = TypeHint.Short
  
  override def isEmpty: Boolean = array.length == 0
  
  override def length: Int = array.length
  
  override def apply(index: Int): Short = array(index)
  
  override def update[B >: Short](index: Int, elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Short]) {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      val newArray = new Array[Short](length)
      java.lang.System.arraycopy(array, 0, newArray, 0, newArray.length)
      newArray(index) = elem.asInstanceOf[Short]
      new ShortArraySeq(newArray).asInstanceOf[ArraySeq[B]]
    }
    else super.update(index, elem)
  }
  
  override def append[B >: Short](elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Short]) {
      val newArray = new Array[Short](length + 1)
      java.lang.System.arraycopy(array, 0, newArray, 0, length)
      newArray(newArray.length) = elem.asInstanceOf[Short]
      new ShortArraySeq(newArray)
    }
    else super.append(elem)
  }
  
  override def appendAll[B >: Short](elems: Enumerator[B]): ArraySeq[B] = {
    if (elems.isInstanceOf[ArrayLike[_]] && Reified[Short](elems)) {
      val xs = elems.asInstanceOf[ArrayLike[Short]]
      val n = xs.length
      val newArray = new Array[Short](length + n)
      java.lang.System.arraycopy(array, 0, newArray, 0, length)
      xs.copyToArray(0, newArray, length, n)
      new ShortArraySeq(newArray)
    }
    else super.appendAll(elems)
  }
  
  override def prepend[B >: Short](elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Short]) {
      val newArray = new Array[Short](1 + length)
      newArray(0) = elem.asInstanceOf[Short]
      java.lang.System.arraycopy(array, 0, newArray, 1, length)
      new ShortArraySeq(newArray)
    }
    else super.prepend(elem)
  }
  
  override def prependAll[B >: Short](elems: Enumerator[B]): ArraySeq[B] = {
    if (elems.isInstanceOf[ArrayLike[_]] && Reified[Short](elems)) {
      val xs = elems.asInstanceOf[ArrayLike[Short]]
      val n = xs.length
      val newArray = new Array[Short](n + length)
      xs.copyToArray(0, newArray, 0, n)
      java.lang.System.arraycopy(array, 0, newArray, n, length)
      new ShortArraySeq(newArray)
    }
    else super.prependAll(elems)
  }
  
  override def insert[B >: Short](index: Int, elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Short]) {
      if (index < 0 || index > length) throw new IndexOutOfBoundsException(index.toString)
      val newArray = new Array[Short](length + 1)
      java.lang.System.arraycopy(array, 0, newArray, 0, index)
      newArray(index) = elem.asInstanceOf[Short]
      java.lang.System.arraycopy(array, index, newArray, index + 1, length - index)
      new ShortArraySeq(newArray)
    }
    else super.insert(index, elem)
  }
  
  override def insertAll[B >: Short](index: Int, elems: Enumerator[B]): ArraySeq[B] = {
    if (elems.isInstanceOf[ArrayLike[_]] && Reified[Short](elems)) {
      val xs = elems.asInstanceOf[ArrayLike[Short]]
      val n = xs.length
      if (index < 0 || index > length) throw new IndexOutOfBoundsException(index.toString)
      val newArray = new Array[Short](length + n)
      java.lang.System.arraycopy(array, 0, newArray, 0, index)
      xs.copyToArray(0, newArray, index, n)
      java.lang.System.arraycopy(array, index, newArray, index + n, length - index)
      new ShortArraySeq(newArray)
    }
    else super.insertAll(index, elems)
  }
  
  override def remove(index: Int): ArraySeq[Short] = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    val newArray = new Array[Short](length - 1)
    java.lang.System.arraycopy(array, 0, newArray, 0, index)
    java.lang.System.arraycopy(array, index + 1, newArray, index, newArray.length - index)
    new ShortArraySeq(newArray)
  }
  
  override def remove(index: Int, count: Int): ArraySeq[Short] = {
    if (count < 0) throw new IllegalArgumentException("negative count")
    if (index < 0) throw new IndexOutOfBoundsException(index.toString)
    if (index + count > length) throw new IndexOutOfBoundsException((index + count).toString)
    val newArray = new Array[Short](length - count)
    java.lang.System.arraycopy(array, 0, newArray, 0, index)
    java.lang.System.arraycopy(array, index + count, newArray, index, newArray.length - index)
    new ShortArraySeq(newArray)
  }
  
  override def copyToArray[B >: Short](index: Int, to: Array[B], offset: Int, count: Int) {
    if (to.isInstanceOf[Array[Short]]) java.lang.System.arraycopy(array, index, to, offset, count)
    else super.copyToArray(index, to, offset, count)
  }
  
  override def toArray[B >: Short](implicit B: scala.reflect.ClassTag[B]): Array[B] = {
    if (B == scala.reflect.ClassTag.Short) {
      val newArray = new Array[Short](length)
      java.lang.System.arraycopy(array, 0, newArray, 0, newArray.length)
      newArray.asInstanceOf[Array[B]]
    }
    else super.toArray
  }
  
  override def iterator: Iterator[Short] = new ShortArraySeqIterator(array)
}

private[containers] final class ShortArraySeqIterator
    (array: Array[Short], private[this] var i: Int, n: Int)
  extends Iterator[Short] {
  
  def this(array: Array[Short]) = this(array, 0, array.length)
  
  override def isEmpty: Boolean = i >= n
  
  override def head: Short = {
    if (i >= n) throw new NoSuchElementException("Head of empty iterator.")
    array(i)
  }
  
  override def step() {
    if (i >= n) throw new UnsupportedOperationException("Empty iterator step.")
    i += 1
  }
  
  override def dup: Iterator[Short] = new ShortArraySeqIterator(array, i, n)
}

private[containers] final class ShortArraySeqBuilder
  extends ShortArrayBuffer with Builder[Any, Short] {
  override type State = ArraySeq[Short]
  override def state: ArraySeq[Short] = toArraySeq
}
