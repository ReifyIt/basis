/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._

private[containers] final class LongArraySeq(array: Array[Long]) extends ArraySeq[Long] {
  override def isEmpty: Boolean = array.length == 0
  
  override def length: Int = array.length
  
  override def apply(index: Int): Long = array(index)
  
  override def update[B >: Long](index: Int, elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Long]) {
      val newArray = new Array[Long](array.length)
      java.lang.System.arraycopy(array, 0, newArray, 0, newArray.length)
      newArray(index) = elem.asInstanceOf[Long]
      new LongArraySeq(newArray).asInstanceOf[ArraySeq[B]]
    }
    else super.update(index, elem)
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
