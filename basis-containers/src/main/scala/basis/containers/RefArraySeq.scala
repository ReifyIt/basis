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

private[containers] final class RefArraySeq[+A](array: Array[AnyRef]) extends ArraySeq[A] {
  override def isEmpty: Boolean = array.length == 0
  
  override def length: Int = array.length
  
  override def apply(index: Int): A = array(index).asInstanceOf[A]
  
  override def copyToArray[B >: A](xs: Array[B], start: Int, count: Int) {
    if (xs.isInstanceOf[Array[AnyRef]])
      java.lang.System.arraycopy(array, 0, xs, start, count min (xs.length - start) min length)
    else super.copyToArray(xs, start, count)
  }
  
  override def copyToArray[B >: A](xs: Array[B], start: Int) {
    if (xs.isInstanceOf[Array[AnyRef]])
      java.lang.System.arraycopy(array, 0, xs, start, (xs.length - start) min length)
    else super.copyToArray(xs, start)
  }
  
  override def copyToArray[B >: A](xs: Array[B]) {
    if (xs.isInstanceOf[Array[AnyRef]])
      java.lang.System.arraycopy(array, 0, xs, 0, xs.length min length)
    else super.copyToArray(xs)
  }
  
  override def toArray[B >: A](implicit B: ClassTag[B]): Array[B] = {
    val xs = B.newArray(length)
    if (xs.isInstanceOf[Array[AnyRef]])
      java.lang.System.arraycopy(array, 0, xs, 0, length)
    else super.copyToArray(xs)
    xs
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
  
  protected override def stringPrefix: String = "ArraySeq.Builder"
}
