/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.util._

import scala.reflect.ClassTag

private[containers] final class FloatArraySeq(array: Array[Float]) extends ArraySeq[Float] {
  override def isEmpty: Boolean = array.length == 0
  
  override def length: Int = array.length
  
  override def apply(index: Int): Float = array(index)
  
  override def copyToArray[B >: Float](xs: Array[B], start: Int, count: Int) {
    if (xs.isInstanceOf[Array[Float]])
      java.lang.System.arraycopy(array, 0, xs, start, count min (xs.length - start) min length)
    else super.copyToArray(xs, start, count)
  }
  
  override def copyToArray[B >: Float](xs: Array[B], start: Int) {
    if (xs.isInstanceOf[Array[Float]])
      java.lang.System.arraycopy(array, 0, xs, start, (xs.length - start) min length)
    else super.copyToArray(xs, start)
  }
  
  override def copyToArray[B >: Float](xs: Array[B]) {
    if (xs.isInstanceOf[Array[Float]])
      java.lang.System.arraycopy(array, 0, xs, 0, xs.length min length)
    else super.copyToArray(xs)
  }
  
  override def toArray[B >: Float](implicit B: ClassTag[B]): Array[B] = {
    if (B == ClassTag.Float) {
      val xs = new Array[Float](length)
      java.lang.System.arraycopy(array, 0, xs, 0, length)
      xs.asInstanceOf[Array[B]]
    }
    else super.toArray[B]
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
