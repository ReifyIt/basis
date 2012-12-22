/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._

import scala.reflect.ClassTag

private[containers] final class ShortArraySeq(array: Array[Short]) extends ArraySeq[Short] {
  override def isEmpty: Boolean = array.length == 0
  
  override def length: Int = array.length
  
  override def apply(index: Int): Short = array(index)
  
  override def update[B >: Short](index: Int, elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Short]) {
      val newArray = new Array[Short](array.length)
      java.lang.System.arraycopy(array, 0, newArray, 0, newArray.length)
      newArray(index) = elem.asInstanceOf[Short]
      new ShortArraySeq(newArray).asInstanceOf[ArraySeq[B]]
    }
    else super.update(index, elem)
  }
  
  override def copyToArray[B >: Short](index: Int, to: Array[B], offset: Int, count: Int) {
    if (to.isInstanceOf[Array[Short]]) java.lang.System.arraycopy(array, index, to, offset, count)
    else super.copyToArray(index, to, offset, count)
  }
  
  override def toArray[B >: Short](implicit B: ClassTag[B]): Array[B] = {
    if (B == ClassTag.Short) {
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
