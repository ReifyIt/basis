/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._

private[containers] final class FloatArraySeq(array: Array[Float]) extends ArraySeq[Float] {
  override def isEmpty: Boolean = array.length == 0
  
  override def length: Int = array.length
  
  override def apply(index: Int): Float = array(index)
  
  override def update[B >: Float](index: Int, elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Float]) {
      val newArray = new Array[Float](array.length)
      java.lang.System.arraycopy(array, 0, newArray, 0, newArray.length)
      newArray(index) = elem.asInstanceOf[Float]
      new FloatArraySeq(newArray).asInstanceOf[ArraySeq[B]]
    }
    else super.update(index, elem)
  }
  
  override def copyToArray[B >: Float](index: Int, to: Array[B], offset: Int, count: Int) {
    if (to.isInstanceOf[Array[Float]]) java.lang.System.arraycopy(array, index, to, offset, count)
    else super.copyToArray(index, to, offset, count)
  }
  
  override def toArray[B >: Float](implicit B: scala.reflect.ClassTag[B]): Array[B] = {
    if (B == scala.reflect.ClassTag.Float) {
      val newArray = new Array[Float](length)
      java.lang.System.arraycopy(array, 0, newArray, 0, newArray.length)
      newArray.asInstanceOf[Array[B]]
    }
    else super.toArray
  }
  
  override def iterator: Iterator[Float] = new FloatArraySeqIterator(array)
}

private[containers] final class FloatArraySeqIterator
    (array: Array[Float], private[this] var i: Int, n: Int)
  extends Iterator[Float] {
  
  def this(array: Array[Float]) = this(array, 0, array.length)
  
  override def isEmpty: Boolean = i >= n
  
  override def head: Float = {
    if (i >= n) throw new NoSuchElementException("Head of empty iterator.")
    array(i)
  }
  
  override def step() {
    if (i >= n) throw new UnsupportedOperationException("Empty iterator step.")
    i += 1
  }
  
  override def dup: Iterator[Float] = new FloatArraySeqIterator(array, i, n)
}

private[containers] final class FloatArraySeqBuilder
  extends FloatArrayBuffer with Builder[Any, Float] {
  override type State = ArraySeq[Float]
  override def state: ArraySeq[Float] = toArraySeq
}
