/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._

import scala.reflect.ClassTag

private[containers] final class DoubleArraySeq(array: Array[Double]) extends ArraySeq[Double] {
  override def isEmpty: Boolean = array.length == 0
  
  override def length: Int = array.length
  
  override def apply(index: Int): Double = array(index)
  
  override def update[B >: Double](index: Int, elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Double]) {
      val newArray = new Array[Double](array.length)
      java.lang.System.arraycopy(array, 0, newArray, 0, newArray.length)
      newArray(index) = elem.asInstanceOf[Double]
      new DoubleArraySeq(newArray).asInstanceOf[ArraySeq[B]]
    }
    else super.update(index, elem)
  }
  
  override def copyToArray[B >: Double](index: Int, to: Array[B], offset: Int, count: Int) {
    if (to.isInstanceOf[Array[Double]]) java.lang.System.arraycopy(array, index, to, offset, count)
    else super.copyToArray(index, to, offset, count)
  }
  
  override def toArray[B >: Double](implicit B: ClassTag[B]): Array[B] = {
    if (B == ClassTag.Double) {
      val newArray = new Array[Double](length)
      java.lang.System.arraycopy(array, 0, newArray, 0, newArray.length)
      newArray.asInstanceOf[Array[B]]
    }
    else super.toArray
  }
  
  override def iterator: Iterator[Double] = new DoubleArraySeqIterator(array)
}

private[containers] final class DoubleArraySeqIterator
    (array: Array[Double], private[this] var i: Int, n: Int)
  extends Iterator[Double] {
  
  def this(array: Array[Double]) = this(array, 0, array.length)
  
  override def isEmpty: Boolean = i >= n
  
  override def head: Double = {
    if (i >= n) throw new NoSuchElementException("Head of empty iterator.")
    array(i)
  }
  
  override def step() {
    if (i >= n) throw new UnsupportedOperationException("Empty iterator step.")
    i += 1
  }
  
  override def dup: Iterator[Double] = new DoubleArraySeqIterator(array, i, n)
}

private[containers] final class DoubleArraySeqBuilder
  extends DoubleArrayBuffer with Builder[Any, Double] {
  override type State = ArraySeq[Double]
  override def state: ArraySeq[Double] = toArraySeq
}
