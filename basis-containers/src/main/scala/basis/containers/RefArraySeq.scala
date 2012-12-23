/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._

private[containers] final class RefArraySeq[+A](array: Array[AnyRef]) extends ArraySeq[A] {
  override def isEmpty: Boolean = array.length == 0
  
  override def length: Int = array.length
  
  override def apply(index: Int): A = array(index).asInstanceOf[A]
  
  override def update[B >: A](index: Int, elem: B): ArraySeq[B] = {
    val newArray = new Array[AnyRef](array.length)
    java.lang.System.arraycopy(array, 0, newArray, 0, newArray.length)
    newArray(index) = elem.asInstanceOf[AnyRef]
    new RefArraySeq(newArray)
  }
  
  override def copyToArray[B >: A](index: Int, to: Array[B], offset: Int, count: Int) {
    if (to.isInstanceOf[Array[AnyRef]]) java.lang.System.arraycopy(array, index, to, offset, count)
    else super.copyToArray(index, to, offset, count)
  }
  
  override def toArray[B >: A](implicit B: scala.reflect.ClassTag[B]): Array[B] = {
    if (!B.runtimeClass.isPrimitive) {
      val newArray = B.newArray(length)
      java.lang.System.arraycopy(array, 0, newArray, 0, newArray.length)
      newArray
    }
    else super.toArray
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
}
