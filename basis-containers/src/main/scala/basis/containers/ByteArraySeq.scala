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

private[containers] final class ByteArraySeq(array: Array[Byte]) extends ArraySeq[Byte] {
  override def isEmpty: Boolean = array.length == 0
  
  override def length: Int = array.length
  
  override def apply(index: Int): Byte = array(index)
  
  override def copyToArray[B >: Byte](xs: Array[B], start: Int, count: Int) {
    if (xs.isInstanceOf[Array[Byte]])
      java.lang.System.arraycopy(array, 0, xs, start, count min (xs.length - start) min length)
    else super.copyToArray(xs, start, count)
  }
  
  override def copyToArray[B >: Byte](xs: Array[B], start: Int) {
    if (xs.isInstanceOf[Array[Byte]])
      java.lang.System.arraycopy(array, 0, xs, start, (xs.length - start) min length)
    else super.copyToArray(xs, start)
  }
  
  override def copyToArray[B >: Byte](xs: Array[B]) {
    if (xs.isInstanceOf[Array[Byte]])
      java.lang.System.arraycopy(array, 0, xs, 0, xs.length min length)
    else super.copyToArray(xs)
  }
  
  override def toArray[B >: Byte](implicit B: ClassTag[B]): Array[B] = {
    if (B == ClassTag.Byte) {
      val xs = new Array[Byte](length)
      java.lang.System.arraycopy(array, 0, xs, 0, length)
      xs.asInstanceOf[Array[B]]
    }
    else super.toArray[B]
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
