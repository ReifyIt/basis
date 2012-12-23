/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._

private[containers] final class IntArraySeq(array: Array[Int]) extends ArraySeq[Int] {
  override def isEmpty: Boolean = array.length == 0
  
  override def length: Int = array.length
  
  override def apply(index: Int): Int = array(index)
  
  override def update[B >: Int](index: Int, elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Int]) {
      val newArray = new Array[Int](array.length)
      java.lang.System.arraycopy(array, 0, newArray, 0, newArray.length)
      newArray(index) = elem.asInstanceOf[Int]
      new IntArraySeq(newArray).asInstanceOf[ArraySeq[B]]
    }
    else super.update(index, elem)
  }
  
  override def copyToArray[B >: Int](index: Int, to: Array[B], offset: Int, count: Int) {
    if (to.isInstanceOf[Array[Int]]) java.lang.System.arraycopy(array, index, to, offset, count)
    else super.copyToArray(index, to, offset, count)
  }
  
  override def toArray[B >: Int](implicit B: scala.reflect.ClassTag[B]): Array[B] = {
    if (B == scala.reflect.ClassTag.Int) {
      val newArray = new Array[Int](length)
      java.lang.System.arraycopy(array, 0, newArray, 0, newArray.length)
      newArray.asInstanceOf[Array[B]]
    }
    else super.toArray
  }
  
  override def iterator: Iterator[Int] = new IntArraySeqIterator(array)
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
}
