/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.runtime._

/** An immutable contiguous `Int` array.
  * 
  * @author Chris Sachs
  * @since  0.0
  */
private[containers] final class IntArraySeq(array: Array[Int]) extends ArraySeq[Int] with Reified {
  protected override def T: TypeHint[Int] = TypeHint.Int
  
  override def length: Int = array.length
  
  override def apply(index: Int): Int = array(index)
  
  override def update[B >: Int](index: Int, elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Int]) {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      val newArray = new Array[Int](length)
      java.lang.System.arraycopy(array, 0, newArray, 0, newArray.length)
      newArray(index) = elem.asInstanceOf[Int]
      new IntArraySeq(newArray).asInstanceOf[ArraySeq[B]]
    }
    else super.update(index, elem)
  }
  
  override def append[B >: Int](elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Int]) {
      val newArray = new Array[Int](length + 1)
      java.lang.System.arraycopy(array, 0, newArray, 0, length)
      newArray(newArray.length) = elem.asInstanceOf[Int]
      new IntArraySeq(newArray)
    }
    else super.append(elem)
  }
  
  override def appendAll[B >: Int](elems: Enumerator[B]): ArraySeq[B] = {
    if (elems.isInstanceOf[ArrayLike[_]] && Reified[Int](elems)) {
      val xs = elems.asInstanceOf[ArrayLike[Int]]
      val n = xs.length
      val newArray = new Array[Int](length + n)
      java.lang.System.arraycopy(array, 0, newArray, 0, length)
      xs.copyToArray(0, newArray, length, n)
      new IntArraySeq(newArray)
    }
    else super.appendAll(elems)
  }
  
  override def prepend[B >: Int](elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Int]) {
      val newArray = new Array[Int](1 + length)
      newArray(0) = elem.asInstanceOf[Int]
      java.lang.System.arraycopy(array, 0, newArray, 1, length)
      new IntArraySeq(newArray)
    }
    else super.prepend(elem)
  }
  
  override def prependAll[B >: Int](elems: Enumerator[B]): ArraySeq[B] = {
    if (elems.isInstanceOf[ArrayLike[_]] && Reified[Int](elems)) {
      val xs = elems.asInstanceOf[ArrayLike[Int]]
      val n = xs.length
      val newArray = new Array[Int](n + length)
      xs.copyToArray(0, newArray, 0, n)
      java.lang.System.arraycopy(array, 0, newArray, n, length)
      new IntArraySeq(newArray)
    }
    else super.prependAll(elems)
  }
  
  override def insert[B >: Int](index: Int, elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Int]) {
      if (index < 0 || index > length) throw new IndexOutOfBoundsException(index.toString)
      val newArray = new Array[Int](length + 1)
      java.lang.System.arraycopy(array, 0, newArray, 0, index)
      newArray(index) = elem.asInstanceOf[Int]
      java.lang.System.arraycopy(array, index, newArray, index + 1, length - index)
      new IntArraySeq(newArray)
    }
    else super.insert(index, elem)
  }
  
  override def insertAll[B >: Int](index: Int, elems: Enumerator[B]): ArraySeq[B] = {
    if (elems.isInstanceOf[ArrayLike[_]] && Reified[Int](elems)) {
      val xs = elems.asInstanceOf[ArrayLike[Int]]
      val n = xs.length
      if (index < 0 || index > length) throw new IndexOutOfBoundsException(index.toString)
      val newArray = new Array[Int](length + n)
      java.lang.System.arraycopy(array, 0, newArray, 0, index)
      xs.copyToArray(0, newArray, index, n)
      java.lang.System.arraycopy(array, index, newArray, index + n, length - index)
      new IntArraySeq(newArray)
    }
    else super.insertAll(index, elems)
  }
  
  override def remove(index: Int): ArraySeq[Int] = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    val newArray = new Array[Int](length - 1)
    java.lang.System.arraycopy(array, 0, newArray, 0, index)
    java.lang.System.arraycopy(array, index + 1, newArray, index, newArray.length - index)
    new IntArraySeq(newArray)
  }
  
  override def remove(index: Int, count: Int): ArraySeq[Int] = {
    if (count < 0) throw new IllegalArgumentException("negative count")
    if (index < 0) throw new IndexOutOfBoundsException(index.toString)
    if (index + count > length) throw new IndexOutOfBoundsException((index + count).toString)
    val newArray = new Array[Int](length - count)
    java.lang.System.arraycopy(array, 0, newArray, 0, index)
    java.lang.System.arraycopy(array, index + count, newArray, index, newArray.length - index)
    new IntArraySeq(newArray)
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
