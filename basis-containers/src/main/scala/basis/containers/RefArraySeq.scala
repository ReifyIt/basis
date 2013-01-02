/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._

/** An immutable contiguous reference array.
  * 
  * @author Chris Sachs
  * @since  0.0
  */
private[containers] final class RefArraySeq[+A](array: Array[AnyRef]) extends ArraySeq[A] {
  override def isEmpty: Boolean = array.length == 0
  
  override def length: Int = array.length
  
  override def apply(index: Int): A = array(index).asInstanceOf[A]
  
  override def update[B >: A](index: Int, elem: B): ArraySeq[B] = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    val newArray = new Array[AnyRef](length)
    java.lang.System.arraycopy(array, 0, newArray, 0, newArray.length)
    newArray(index) = elem.asInstanceOf[AnyRef]
    new RefArraySeq(newArray)
  }
  
  override def append[B >: A](elem: B): ArraySeq[B] = {
    val newArray = new Array[AnyRef](length + 1)
    java.lang.System.arraycopy(array, 0, newArray, 0, length)
    newArray(newArray.length) = elem.asInstanceOf[AnyRef]
    new RefArraySeq(newArray)
  }
  
  override def appendAll[B >: A](elems: Enumerator[B]): ArraySeq[B] = {
    if (elems.isInstanceOf[ArrayLike[_]]) {
      val xs = elems.asInstanceOf[ArrayLike[B]]
      val n = xs.length
      val newArray = new Array[AnyRef](length + n)
      java.lang.System.arraycopy(array, 0, newArray, 0, length)
      xs.copyToArray(0, newArray.asInstanceOf[Array[Any]], length, n)
      new RefArraySeq(newArray)
    }
    else appendAll(ArrayBuffer.coerce(elems))
  }
  
  override def prepend[B >: A](elem: B): ArraySeq[B] = {
    val newArray = new Array[AnyRef](1 + length)
    newArray(0) = elem.asInstanceOf[AnyRef]
    java.lang.System.arraycopy(array, 0, newArray, 1, length)
    new RefArraySeq(newArray)
  }
  
  override def prependAll[B >: A](elems: Enumerator[B]): ArraySeq[B] = {
    if (elems.isInstanceOf[ArrayLike[_]]) {
      val xs = elems.asInstanceOf[ArrayLike[B]]
      val n = xs.length
      val newArray = new Array[AnyRef](n + length)
      xs.copyToArray(0, newArray.asInstanceOf[Array[Any]], 0, n)
      java.lang.System.arraycopy(array, 0, newArray, n, length)
      new RefArraySeq(newArray)
    }
    else prependAll(ArrayBuffer.coerce(elems))
  }
  
  override def insert[B >: A](index: Int, elem: B): ArraySeq[B] = {
    if (index < 0 || index > length) throw new IndexOutOfBoundsException(index.toString)
    val newArray = new Array[AnyRef](length + 1)
    java.lang.System.arraycopy(array, 0, newArray, 0, index)
    newArray(index) = elem.asInstanceOf[AnyRef]
    java.lang.System.arraycopy(array, index, newArray, index + 1, length - index)
    new RefArraySeq(newArray)
  }
  
  override def insertAll[B >: A](index: Int, elems: Enumerator[B]): ArraySeq[B] = {
    if (elems.isInstanceOf[ArrayLike[_]]) {
      val xs = elems.asInstanceOf[ArrayLike[B]]
      val n = xs.length
      if (index < 0 || index > length) throw new IndexOutOfBoundsException(index.toString)
      val newArray = new Array[AnyRef](length + n)
      java.lang.System.arraycopy(array, 0, newArray, 0, index)
      xs.copyToArray(0, newArray.asInstanceOf[Array[Any]], index, n)
      java.lang.System.arraycopy(array, index, newArray, index + n, length - index)
      new RefArraySeq(newArray)
    }
    else insertAll(index, ArrayBuffer.coerce(elems))
  }
  
  override def remove(index: Int): ArraySeq[A] = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    val newArray = new Array[AnyRef](length - 1)
    java.lang.System.arraycopy(array, 0, newArray, 0, index)
    java.lang.System.arraycopy(array, index + 1, newArray, index, newArray.length - index)
    new RefArraySeq(newArray)
  }
  
  override def remove(index: Int, count: Int): ArraySeq[A] = {
    if (count < 0) throw new IllegalArgumentException("negative count")
    if (index < 0) throw new IndexOutOfBoundsException(index.toString)
    if (index + count > length) throw new IndexOutOfBoundsException((index + count).toString)
    val newArray = new Array[AnyRef](length - count)
    java.lang.System.arraycopy(array, 0, newArray, 0, index)
    java.lang.System.arraycopy(array, index + count, newArray, index, newArray.length - index)
    new RefArraySeq(newArray)
  }
  
  override def copyToArray[B >: A](index: Int, to: Array[B], offset: Int, count: Int) {
    if (to.isInstanceOf[Array[AnyRef]]) java.lang.System.arraycopy(array, index, to, offset, count)
    else super.copyToArray(index, to, offset, count)
  }
  
  override def toArray[B >: A](implicit B: scala.reflect.ClassTag[B]): Array[B] = {
    if (!B.runtimeClass.isPrimitive) {
      val newArray = B.newArray(length)
      java.lang.System.arraycopy(array, 0, newArray, 0, newArray.length)
      newArray
    }
    else super.toArray
  }
  
  override def iterator: Iterator[A] = new RefArraySeqIterator(array)
}

private[containers] final class RefArraySeqIterator[A]
    (array: Array[AnyRef], private[this] var i: Int, n: Int)
  extends Iterator[A] {
  
  def this(array: Array[AnyRef]) = this(array, 0, array.length)
  
  override def isEmpty: Boolean = i >= n
  
  override def head: A = {
    if (i >= n) throw new NoSuchElementException("Head of empty iterator.")
    array(i).asInstanceOf[A]
  }
  
  override def step() {
    if (i >= n) throw new UnsupportedOperationException("Empty iterator step.")
    i += 1
  }
  
  override def dup: Iterator[A] = new RefArraySeqIterator(array, i, n)
}

private[containers] final class RefArraySeqBuilder[A]
  extends RefArrayBuffer[A] with Builder[Any, A] {
  override type State = ArraySeq[A]
  override def state: ArraySeq[A] = toArraySeq
}
