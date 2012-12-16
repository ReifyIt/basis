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

private[containers] final class IntArraySeq(array: Array[Int]) extends ArraySeq[Int] {
  override def isEmpty: Boolean = array.length == 0
  
  override def length: Int = array.length
  
  override def apply(index: Int): Int = array(index)
  
  override def copyToArray[B >: Int](xs: Array[B], start: Int, count: Int) {
    if (xs.isInstanceOf[Array[Int]])
      java.lang.System.arraycopy(array, 0, xs, start, count min (xs.length - start) min length)
    else super.copyToArray(xs, start, count)
  }
  
  override def copyToArray[B >: Int](xs: Array[B], start: Int) {
    if (xs.isInstanceOf[Array[Int]])
      java.lang.System.arraycopy(array, 0, xs, start, (xs.length - start) min length)
    else super.copyToArray(xs, start)
  }
  
  override def copyToArray[B >: Int](xs: Array[B]) {
    if (xs.isInstanceOf[Array[Int]])
      java.lang.System.arraycopy(array, 0, xs, 0, xs.length min length)
    else super.copyToArray(xs)
  }
  
  override def toArray[B >: Int](implicit B: ClassTag[B]): Array[B] = {
    if (B == ClassTag.Int) {
      val xs = new Array[Int](length)
      java.lang.System.arraycopy(array, 0, xs, 0, length)
      xs.asInstanceOf[Array[B]]
    }
    else super.toArray[B]
  }
  
  override def iterator: Iterator[Int] = new IntArraySeqIterator(array)
  
  protected override def stringPrefix: String = "ArraySeq[Int]"
}

private[containers] final class IntArraySeqIterator
    (array: Array[Int], private[this] var i: Int, n: Int)
  extends Iterator[Int] {
  
  def this(array: Array[Int]) = this(array, 0, array.length)
  
  override def isEmpty: Boolean = i >= n
  
  override def head: Int = {
    if (i >= n) throw new NoSuchElementException("Head of empty iterator.")
    array(i)
  }
  
  override def step() {
    if (i >= n) throw new UnsupportedOperationException("Empty iterator step.")
    i += 1
  }
  
  override def dup: Iterator[Int] = new IntArraySeqIterator(array, i, n)
}

private[containers] final class IntArraySeqBuilder
  extends IntArrayBuffer with Builder[Any, Int] {
  
  override type State = ArraySeq[Int]
  
  override def state: ArraySeq[Int] = toArraySeq
  
  protected override def stringPrefix: String = "ArraySeq.Builder[Int]"
}
