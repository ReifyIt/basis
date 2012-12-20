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

private[containers] final class ShortArraySeq(array: Array[Short]) extends ArraySeq[Short] {
  override def isEmpty: Boolean = array.length == 0
  
  override def length: Int = array.length
  
  override def apply(index: Int): Short = array(index)
  
  override def copyToArray[B >: Short](xs: Array[B], start: Int, count: Int) {
    if (xs.isInstanceOf[Array[Short]])
      java.lang.System.arraycopy(array, 0, xs, start, count min (xs.length - start) min length)
    else super.copyToArray(xs, start, count)
  }
  
  override def copyToArray[B >: Short](xs: Array[B], start: Int) {
    if (xs.isInstanceOf[Array[Short]])
      java.lang.System.arraycopy(array, 0, xs, start, (xs.length - start) min length)
    else super.copyToArray(xs, start)
  }
  
  override def copyToArray[B >: Short](xs: Array[B]) {
    if (xs.isInstanceOf[Array[Short]])
      java.lang.System.arraycopy(array, 0, xs, 0, xs.length min length)
    else super.copyToArray(xs)
  }
  
  override def toArray[B >: Short](implicit B: ClassTag[B]): Array[B] = {
    if (B == ClassTag.Short) {
      val xs = new Array[Short](length)
      java.lang.System.arraycopy(array, 0, xs, 0, length)
      xs.asInstanceOf[Array[B]]
    }
    else super.toArray[B]
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
