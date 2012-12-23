/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._

private[containers] final class ByteArraySeq(array: Array[Byte]) extends ArraySeq[Byte] {
  override def isEmpty: Boolean = array.length == 0
  
  override def length: Int = array.length
  
  override def apply(index: Int): Byte = array(index)
  
  override def update[B >: Byte](index: Int, elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Byte]) {
      val n = array.length
      val newArray = new Array[Byte](n)
      java.lang.System.arraycopy(array, 0, newArray, 0, n)
      newArray(index) = elem.asInstanceOf[Byte]
      new ByteArraySeq(newArray).asInstanceOf[ArraySeq[B]]
    }
    else super.update(index, elem)
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
