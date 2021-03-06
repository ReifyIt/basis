//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import basis._
import basis.collections.generic._
import basis.collections.mutable._
import scala.reflect._

private[collections] final class FloatArraySeq(array: Array[Float]) extends ArraySeq[Float] {
  override def erasure: ClassTag[_] = ClassTag.Float

  override def length: Int = array.length

  override def apply(index: Int): Float = array(index)

  override def update[B >: Float](index: Int, elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Float]) {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      val newArray = new Array[Float](length)
      java.lang.System.arraycopy(array, 0, newArray, 0, newArray.length)
      newArray(index) = elem.asInstanceOf[Float]
      new FloatArraySeq(newArray).asInstanceOf[ArraySeq[B]]
    }
    else super.update(index, elem)
  }

  override def append[B >: Float](elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Float]) {
      val newArray = new Array[Float](length + 1)
      java.lang.System.arraycopy(array, 0, newArray, 0, length)
      newArray(newArray.length) = elem.asInstanceOf[Float]
      new FloatArraySeq(newArray)
    }
    else super.append(elem)
  }

  override def appendAll[B >: Float](elems: Traverser[B]): ArraySeq[B] = {
    if (elems.isInstanceOf[ArrayLike[_]] && elems.asInstanceOf[ArrayLike[_]].erasure == ClassTag.Float) {
      val xs = elems.asInstanceOf[ArrayLike[Float]]
      val n = xs.length
      val newArray = new Array[Float](length + n)
      java.lang.System.arraycopy(array, 0, newArray, 0, length)
      xs.copyToArray(0, newArray, length, n)
      new FloatArraySeq(newArray)
    }
    else super.appendAll(elems)
  }

  override def prepend[B >: Float](elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Float]) {
      val newArray = new Array[Float](1 + length)
      newArray(0) = elem.asInstanceOf[Float]
      java.lang.System.arraycopy(array, 0, newArray, 1, length)
      new FloatArraySeq(newArray)
    }
    else super.prepend(elem)
  }

  override def prependAll[B >: Float](elems: Traverser[B]): ArraySeq[B] = {
    if (elems.isInstanceOf[ArrayLike[_]] && elems.asInstanceOf[ArrayLike[_]].erasure == ClassTag.Float) {
      val xs = elems.asInstanceOf[ArrayLike[Float]]
      val n = xs.length
      val newArray = new Array[Float](n + length)
      xs.copyToArray(0, newArray, 0, n)
      java.lang.System.arraycopy(array, 0, newArray, n, length)
      new FloatArraySeq(newArray)
    }
    else super.prependAll(elems)
  }

  override def insert[B >: Float](index: Int, elem: B): ArraySeq[B] = {
    if (elem.isInstanceOf[Float]) {
      if (index < 0 || index > length) throw new IndexOutOfBoundsException(index.toString)
      val newArray = new Array[Float](length + 1)
      java.lang.System.arraycopy(array, 0, newArray, 0, index)
      newArray(index) = elem.asInstanceOf[Float]
      java.lang.System.arraycopy(array, index, newArray, index + 1, length - index)
      new FloatArraySeq(newArray)
    }
    else super.insert(index, elem)
  }

  override def insertAll[B >: Float](index: Int, elems: Traverser[B]): ArraySeq[B] = {
    if (elems.isInstanceOf[ArrayLike[_]] && elems.asInstanceOf[ArrayLike[_]].erasure == ClassTag.Float) {
      val xs = elems.asInstanceOf[ArrayLike[Float]]
      val n = xs.length
      if (index < 0 || index > length) throw new IndexOutOfBoundsException(index.toString)
      val newArray = new Array[Float](length + n)
      java.lang.System.arraycopy(array, 0, newArray, 0, index)
      xs.copyToArray(0, newArray, index, n)
      java.lang.System.arraycopy(array, index, newArray, index + n, length - index)
      new FloatArraySeq(newArray)
    }
    else super.insertAll(index, elems)
  }

  override def remove(index: Int): ArraySeq[Float] = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    val newArray = new Array[Float](length - 1)
    java.lang.System.arraycopy(array, 0, newArray, 0, index)
    java.lang.System.arraycopy(array, index + 1, newArray, index, newArray.length - index)
    new FloatArraySeq(newArray)
  }

  override def remove(index: Int, count: Int): ArraySeq[Float] = {
    if (count < 0) throw new IllegalArgumentException("negative count")
    if (index < 0) throw new IndexOutOfBoundsException(index.toString)
    if (index + count > length) throw new IndexOutOfBoundsException((index + count).toString)
    val newArray = new Array[Float](length - count)
    java.lang.System.arraycopy(array, 0, newArray, 0, index)
    java.lang.System.arraycopy(array, index + count, newArray, index, newArray.length - index)
    new FloatArraySeq(newArray)
  }

  override def copyToArray[B >: Float](index: Int, to: Array[B], offset: Int, count: Int): Unit = {
    if (to.isInstanceOf[Array[Float]]) java.lang.System.arraycopy(array, index, to, offset, count)
    else super.copyToArray(index, to, offset, count)
  }

  override def toArray[B >: Float](implicit B: ClassTag[B]): Array[B] = {
    if (B == ClassTag.Float) {
      val newArray = new Array[Float](length)
      java.lang.System.arraycopy(array, 0, newArray, 0, newArray.length)
      newArray.asInstanceOf[Array[B]]
    }
    else super.toArray
  }

  override def iterator: Iterator[Float] = new FloatArraySeqIterator(array)
}

private[collections] final class FloatArraySeqIterator
    (array: Array[Float], private[this] var i: Int, n: Int)
  extends Iterator[Float] {

  def this(array: Array[Float]) = this(array, 0, array.length)

  override def isEmpty: Boolean = i >= n

  override def head: Float = {
    if (i >= n) Iterator.empty.head
    array(i)
  }

  override def step(): Unit = {
    if (i >= n) Iterator.empty.step()
    i += 1
  }

  override def dup: Iterator[Float] = new FloatArraySeqIterator(array, i, n)
}

private[collections] final class FloatArraySeqBuilder extends FloatArrayBuffer with State[ArraySeq[Float]] {
  override def state: ArraySeq[Float] = toArraySeq
  override def toString: String = "ArraySeq"+"."+"Builder"+"["+"Float"+"]"+"()"
}
