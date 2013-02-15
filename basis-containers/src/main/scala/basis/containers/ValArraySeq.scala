/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.memory._
import basis.runtime._
import basis.util._

/** An immutable contiguous value array.
  * 
  * @author Chris Sachs
  * @since  0.0
  */
private[containers] final class ValArraySeq[+A](data: Data)(implicit A: Struct[A]) extends ArraySeq[A] with Reified {
  if (data.size % A.size != 0L) throw new IllegalArgumentException("Data size not a multiple of struct size.")
  if (data.size / A.size > Int.MaxValue) throw new IllegalArgumentException("length > Int.MaxValue")
  
  protected override def T: TypeHint[_ <: A] = A
  
  override val length: Int = (data.size / A.size).toInt
  
  override def apply(index: Int): A = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    A.load(data, A.size * index)
  }
  
  override def update[B >: A](index: Int, elem: B): ArraySeq[B] = {
    if (A.runtimeClass.isInstance(elem)) {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      val newData = data.copy()
      A.store(data, A.size * index, elem.asInstanceOf[A])
      new ValArraySeq[A](newData)
    }
    else super.update(index, elem)
  }
  
  override def append[B >: A](elem: B): ArraySeq[B] = {
    if (A.runtimeClass.isInstance(elem)) {
      val newData = Data.alloc[A](length + 1)
      Data.copy(data, 0L, newData, 0L, data.size)
      A.store(data, data.size, elem.asInstanceOf[A])
      new ValArraySeq[A](newData)
    }
    else super.append(elem)
  }
  
  override def appendAll[B >: A](elems: Enumerator[B]): ArraySeq[B] = {
    if (elems.isInstanceOf[ArrayLike[_]] && Reified[A](elems)) {
      val xs = elems.asInstanceOf[ArrayLike[A]]
      val n = xs.length
      val newData = Data.alloc[A](length + n)
      var p = data.size
      Data.copy(data, 0L, newData, 0L, p)
      var i = 0
      while (i < n) {
        A.store(data, p, xs(i))
        p += A.size
        i += 1
      }
      new ValArraySeq[A](newData)
    }
    else super.appendAll(elems)
  }
  
  override def prepend[B >: A](elem: B): ArraySeq[B] = {
    if (A.runtimeClass.isInstance(elem)) {
      val newData = Data.alloc[A](length + 1)
      A.store(data, 0L, elem.asInstanceOf[A])
      Data.copy(data, 0L, newData, A.size, data.size)
      new ValArraySeq[A](newData)
    }
    else super.prepend(elem)
  }
  
  override def prependAll[B >: A](elems: Enumerator[B]): ArraySeq[B] = {
    if (elems.isInstanceOf[ArrayLike[_]] && Reified[A](elems)) {
      val xs = elems.asInstanceOf[ArrayLike[A]]
      val n = xs.length
      val newData = Data.alloc[A](n + length)
      var p = 0L
      var i = 0
      while (i < n) {
        A.store(data, p, xs(i))
        p += A.size
        i += 1
      }
      Data.copy(data, 0L, newData, p, data.size)
      new ValArraySeq[A](newData)
    }
    else super.prependAll(elems)
  }
  
  override def insert[B >: A](index: Int, elem: B): ArraySeq[B] = {
    if (A.runtimeClass.isInstance(elem)) {
      if (index < 0 || index > length) throw new IndexOutOfBoundsException(index.toString)
      val newData = Data.alloc[A](length + 1)
      val p = A.size * index
      Data.copy(data, 0L, newData, 0L, p)
      A.store(data, p, elem.asInstanceOf[A])
      Data.copy(data, p, newData, p + A.size, data.size - p)
      new ValArraySeq[A](newData)
    }
    else super.insert(index, elem)
  }
  
  override def insertAll[B >: A](index: Int, elems: Enumerator[B]): ArraySeq[B] = {
    if (elems.isInstanceOf[ArrayLike[_]] && Reified[A](elems)) {
      val xs = elems.asInstanceOf[ArrayLike[A]]
      val n = xs.length
      if (index < 0 || index > length) throw new IndexOutOfBoundsException(index.toString)
      val newData = Data.alloc[A](length + n)
      val p = A.size * index
      Data.copy(data, 0L, newData, 0L, p)
      var q = p
      var i = 0
      while (i < n) {
        A.store(data, q, xs(i))
        q += A.size
        i += 1
      }
      Data.copy(data, p, newData, q, data.size - p)
      new ValArraySeq[A](newData)
    }
    else super.insertAll(index, elems)
  }
  
  override def remove(index: Int): ArraySeq[A] = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    val newData = Data.alloc[A](length - 1)
    val p = A.size * index
    Data.copy(data, 0L, newData, 0L, p)
    Data.copy(data, p + A.size, newData, p, newData.size - p)
    new ValArraySeq[A](newData)
  }
  
  override def remove(index: Int, count: Int): ArraySeq[A] = {
    if (count < 0) throw new IllegalArgumentException("negative count")
    if (index < 0) throw new IndexOutOfBoundsException(index.toString)
    if (index + count > length) throw new IndexOutOfBoundsException((index + count).toString)
    val newData = Data.alloc[A](length - count)
    val p = A.size * index
    Data.copy(data, 0L, newData, 0L, p)
    Data.copy(data, p + A.size * count, newData, p, newData.size - p)
    new ValArraySeq[A](newData)
  }
}

private[containers] final class ValArraySeqBuilder[A](implicit A: Struct[A]) extends ArrayBuilder[A] {
  override type Scope = ArraySeq[_]
  override type State = ArraySeq[A]
  
  private[this] var buffer: Data = _
  private[this] var size: Int = 0
  private[this] var aliased: Boolean = true
  
  private[this] def capacity: Int = (buffer.size / A.size).toInt
  
  protected def defaultSize: Int = 16
  
  override def append(elem: A) {
    if (buffer == null) buffer = Data.alloc[A](defaultSize)
    else if (aliased || size + 1 > capacity) buffer = buffer.copy(A.size * expand(size + 1))
    A.store(buffer, A.size * size, elem)
    size += 1
    aliased = false
  }
  
  override def appendAll(elems: Enumerator[A]) {
    if (elems.isInstanceOf[ArrayLike[_]]) {
      val xs = elems.asInstanceOf[ArrayLike[A]]
      val n = xs.length
      if (buffer == null) buffer = Data.alloc[A](expand(n))
      else if (aliased || size + n > capacity) buffer = buffer.copy(A.size * expand(size + n))
      var i = 0
      while (i < n) {
        A.store(buffer, A.size * (size + i), xs(i))
        i += 1
      }
      size += n
      aliased = false
    }
    else appendAll(ArrayBuffer.coerce(elems))
  }
  
  override def appendArray(elems: Array[A]) {
    val n = elems.length
    if (buffer == null) buffer = Data.alloc[A](expand(n))
    else if (aliased || size + n > capacity) buffer = buffer.copy(A.size * expand(size + n))
    var i = 0
    while (i < n) {
      A.store(buffer, A.size * (size + i), elems(i))
      i += 1
    }
    size += n
    aliased = false
  }
  
  override def expect(count: Int): this.type = {
    if (buffer == null) {
      buffer = Data.alloc[A](0)
      aliased = false
    }
    else if (size + count > capacity) {
      buffer = buffer.copy(A.size * size)
      aliased = false
    }
    this
  }
  
  override def state: ArraySeq[A] = {
    if (buffer == null) buffer = Data.alloc[A](0)
    else if (size != capacity) buffer = buffer.copy(A.size * size)
    aliased = true
    new ValArraySeq[A](buffer)
  }
  
  override def clear() {
    aliased = true
    size = 0
    buffer = null
  }
  
  private[this] def expand(size: Int): Int = {
    var n = (defaultSize max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
  
  override def toString: String = "ArraySeqBuilder"+"("+ A +")"
}
