/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.util._

import scala.reflect.ClassTag

private[containers] final class LongArraySeq(array: Array[Long]) extends ArraySeq[Long] {
  override def isEmpty: Boolean = array.length == 0
  
  override def length: Int = array.length
  
  override def apply(index: Int): Long = array(index)
  
  override def copyToArray[B >: Long](xs: Array[B], start: Int, count: Int) {
    if (xs.isInstanceOf[Array[Long]])
      java.lang.System.arraycopy(array, 0, xs, start, count min (xs.length - start) min length)
    else super.copyToArray(xs, start, count)
  }
  
  override def copyToArray[B >: Long](xs: Array[B], start: Int) {
    if (xs.isInstanceOf[Array[Long]])
      java.lang.System.arraycopy(array, 0, xs, start, (xs.length - start) min length)
    else super.copyToArray(xs, start)
  }
  
  override def copyToArray[B >: Long](xs: Array[B]) {
    if (xs.isInstanceOf[Array[Long]])
      java.lang.System.arraycopy(array, 0, xs, 0, xs.length min length)
    else super.copyToArray(xs)
  }
  
  override def toArray[B >: Long](implicit B: ClassTag[B]): Array[B] = {
    if (B == ClassTag.Long) {
      val xs = new Array[Long](length)
      java.lang.System.arraycopy(array, 0, xs, 0, length)
      xs.asInstanceOf[Array[B]]
    }
    else super.toArray[B]
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
