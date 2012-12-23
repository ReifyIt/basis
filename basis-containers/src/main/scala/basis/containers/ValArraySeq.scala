/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.memory._
import basis.util._

private[containers] final class ValArraySeq[+A](data: Data)(implicit A: Struct[A]) extends ArraySeq[A] {
  if (data.size % A.size != 0L) throw new IllegalArgumentException("Data size not a multiple of struct size.")
  if (data.size / A.size > Int.MaxValue) throw new IllegalArgumentException("length > Int.MaxValue")
  
  override val length: Int = (data.size / A.size).toInt
  
  override def isEmpty: Boolean = length == 0
  
  override def apply(index: Int): A = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    A.load(data, A.size * index)
  }
  
  override def update[B >: A](index: Int, elem: B): ArraySeq[B] = {
    if (A.runtimeClass.isInstance(elem)) {
      val newData = data.copy()
      A.store(data, A.size * index, elem.asInstanceOf[A])
      new ValArraySeq(newData)
    }
    else super.update(index, elem)
  }
  
  override def append[B >: A](elem: B): ArraySeq[B] = {
    if (A.runtimeClass.isInstance(elem)) {
      val newData = Data.alloc[A](length + 1)
      Data.copy(data, 0L, newData, 0L, data.size)
      A.store(data, data.size, elem.asInstanceOf[A])
      new ValArraySeq(newData)
    }
    else super.append(elem)
  }
  
  override def prepend[B >: A](elem: B): ArraySeq[B] = {
    if (A.runtimeClass.isInstance(elem)) {
      val newData = Data.alloc[A](length + 1)
      A.store(data, 0L, elem.asInstanceOf[A])
      Data.copy(data, 0L, newData, A.size, data.size)
      new ValArraySeq(newData)
    }
    else super.prepend(elem)
  }
  
  override def insert[B >: A](index: Int, elem: B): ArraySeq[B] = {
    if (A.runtimeClass.isInstance(elem)) {
      if (index < 0 || index > length) throw new IndexOutOfBoundsException(index.toString)
      val offset = A.size * index
      val newData = Data.alloc[A](length + 1)
      Data.copy(data, 0L, newData, 0L, offset)
      A.store(data, offset, elem.asInstanceOf[A])
      Data.copy(data, offset, newData, offset + A.size, data.size - offset)
      new ValArraySeq(newData)
    }
    else super.insert(index, elem)
  }
  
  override def remove(index: Int): ArraySeq[A] = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    val offset = A.size * index
    val newData = Data.alloc[A](length - 1)
    Data.copy(data, 0L, newData, 0L, offset)
    Data.copy(data, offset + A.size, newData, offset, newData.size - offset)
    new ValArraySeq(newData)
  }
}

private[containers] final class ValArraySeqBuilder[A](implicit A: Struct[A]) extends Builder[Any, A] {
  override type State = ArraySeq[A]
  
  private[this] var data: Data = _
  
  private[this] var aliased: Boolean = true
  
  private[this] var length: Int = 0
  
  private[this] def capacity: Int = (data.size / A.size).toInt
  
  private[this] def expand(base: Int, size: Int): Int = {
    var n = (base max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
  
  private[this] def resize(size: Int) {
    data = if (data != null) data.copy(A.size * size) else Data.alloc[A](size)
  }
  
  private[this] def prepare(size: Int) {
    if (aliased || size > capacity) {
      resize(expand(16, size))
      aliased = false
    }
  }
  
  override def append(elem: A) {
    prepare(length + 1)
    A.store(data, A.size * length, elem)
    length += 1
  }
  
  override def expect(count: Int): this.type = {
    if (data == null || length + count > capacity) {
      resize(length + count)
      aliased = false
    }
    this
  }
  
  override def state: ArraySeq[A] = {
    if (data == null || length != capacity) resize(length)
    aliased = true
    new ValArraySeq(data)
  }
  
  override def clear() {
    data = null
    aliased = true
    length = 0
  }
}
