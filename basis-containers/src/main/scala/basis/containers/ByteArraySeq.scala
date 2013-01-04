/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.runtime._

/** An immutable contiguous `Byte` array.
  * 
  * @author Chris Sachs
  * @since  0.0
  */
private[containers] final class ByteArraySeq(array: Array[Byte]) extends ArraySeq[Byte] with Reified {
  protected override def T: TypeHint[Byte] = TypeHint.Byte
  
  override def length: Int = array.length
  
  override def apply(index: Int): Byte = array(index)
  
  override def update[B >: Byte](index: Int, elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Byte]) {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      val newArray = new Array[Byte](length)
      java.lang.System.arraycopy(array, 0, newArray, 0, newArray.length)
      newArray(index) = elem.asInstanceOf[Byte]
      new ByteArraySeq(newArray).asInstanceOf[ArraySeq[B]]
    }
    else super.update(index, elem)
  }
  
  override def append[B >: Byte](elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Byte]) {
      val newArray = new Array[Byte](length + 1)
      java.lang.System.arraycopy(array, 0, newArray, 0, length)
      newArray(newArray.length) = elem.asInstanceOf[Byte]
      new ByteArraySeq(newArray)
    }
    else super.append(elem)
  }
  
  override def appendAll[B >: Byte](elems: Enumerator[B]): ArraySeq[B] = {
    if (elems.isInstanceOf[ArrayLike[_]] && Reified[Byte](elems)) {
      val xs = elems.asInstanceOf[ArrayLike[Byte]]
      val n = xs.length
      val newArray = new Array[Byte](length + n)
      java.lang.System.arraycopy(array, 0, newArray, 0, length)
      xs.copyToArray(0, newArray, length, n)
      new ByteArraySeq(newArray)
    }
    else super.appendAll(elems)
  }
  
  override def prepend[B >: Byte](elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Byte]) {
      val newArray = new Array[Byte](1 + length)
      newArray(0) = elem.asInstanceOf[Byte]
      java.lang.System.arraycopy(array, 0, newArray, 1, length)
      new ByteArraySeq(newArray)
    }
    else super.prepend(elem)
  }
  
  override def prependAll[B >: Byte](elems: Enumerator[B]): ArraySeq[B] = {
    if (elems.isInstanceOf[ArrayLike[_]] && Reified[Byte](elems)) {
      val xs = elems.asInstanceOf[ArrayLike[Byte]]
      val n = xs.length
      val newArray = new Array[Byte](n + length)
      xs.copyToArray(0, newArray, 0, n)
      java.lang.System.arraycopy(array, 0, newArray, n, length)
      new ByteArraySeq(newArray)
    }
    else super.prependAll(elems)
  }
  
  override def insert[B >: Byte](index: Int, elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Byte]) {
      if (index < 0 || index > length) throw new IndexOutOfBoundsException(index.toString)
      val newArray = new Array[Byte](length + 1)
      java.lang.System.arraycopy(array, 0, newArray, 0, index)
      newArray(index) = elem.asInstanceOf[Byte]
      java.lang.System.arraycopy(array, index, newArray, index + 1, length - index)
      new ByteArraySeq(newArray)
    }
    else super.insert(index, elem)
  }
  
  override def insertAll[B >: Byte](index: Int, elems: Enumerator[B]): ArraySeq[B] = {
    if (elems.isInstanceOf[ArrayLike[_]] && Reified[Byte](elems)) {
      val xs = elems.asInstanceOf[ArrayLike[Byte]]
      val n = xs.length
      if (index < 0 || index > length) throw new IndexOutOfBoundsException(index.toString)
      val newArray = new Array[Byte](length + n)
      java.lang.System.arraycopy(array, 0, newArray, 0, index)
      xs.copyToArray(0, newArray, index, n)
      java.lang.System.arraycopy(array, index, newArray, index + n, length - index)
      new ByteArraySeq(newArray)
    }
    else super.insertAll(index, elems)
  }
  
  override def remove(index: Int): ArraySeq[Byte] = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    val newArray = new Array[Byte](length - 1)
    java.lang.System.arraycopy(array, 0, newArray, 0, index)
    java.lang.System.arraycopy(array, index + 1, newArray, index, newArray.length - index)
    new ByteArraySeq(newArray)
  }
  
  override def remove(index: Int, count: Int): ArraySeq[Byte] = {
    if (count < 0) throw new IllegalArgumentException("negative count")
    if (index < 0) throw new IndexOutOfBoundsException(index.toString)
    if (index + count > length) throw new IndexOutOfBoundsException((index + count).toString)
    val newArray = new Array[Byte](length - count)
    java.lang.System.arraycopy(array, 0, newArray, 0, index)
    java.lang.System.arraycopy(array, index + count, newArray, index, newArray.length - index)
    new ByteArraySeq(newArray)
  }
  
  override def copyToArray[B >: Byte](index: Int, to: Array[B], offset: Int, count: Int) {
    if (to.isInstanceOf[Array[Byte]]) java.lang.System.arraycopy(array, index, to, offset, count)
    else super.copyToArray(index, to, offset, count)
  }
  
  override def toArray[B >: Byte](implicit B: scala.reflect.ClassTag[B]): Array[B] = {
    if (B == scala.reflect.ClassTag.Byte) {
      val newArray = new Array[Byte](length)
      java.lang.System.arraycopy(array, 0, newArray, 0, newArray.length)
      newArray.asInstanceOf[Array[B]]
    }
    else super.toArray
  }
  
  override def iterator: Iterator[Byte] = new ByteArraySeqIterator(array)
}

private[containers] final class ByteArraySeqIterator
    (array: Array[Byte], private[this] var i: Int, n: Int)
  extends Iterator[Byte] {
  
  def this(array: Array[Byte]) = this(array, 0, array.length)
  
  override def isEmpty: Boolean = i >= n
  
  override def head: Byte = {
    if (i >= n) throw new NoSuchElementException("Head of empty iterator.")
    array(i)
  }
  
  override def step() {
    if (i >= n) throw new UnsupportedOperationException("Empty iterator step.")
    i += 1
  }
  
  override def dup: Iterator[Byte] = new ByteArraySeqIterator(array, i, n)
}

private[containers] final class ByteArraySeqBuilder
  extends ByteArrayBuffer with Builder[Any, Byte] {
  override type State = ArraySeq[Byte]
  override def state: ArraySeq[Byte] = toArraySeq
}
