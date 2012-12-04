/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers
package mutable

import basis.collections._
import basis.memory._
import basis.util._

private[containers] final class ValArraySeq[A]
    (data: Data, override val length: Int)
    (implicit struct: ValType[A])
  extends ArraySeq[A] {
  
  override def isEmpty: Boolean = length == 0
  
  override def apply(index: Int): A =
    struct.load(data, struct.size.toLong * index.toLong)
  
  override def update(index: Int, value: A): Unit =
    struct.store(data, struct.size.toLong * index.toLong, value)
}

private[containers] final class ValArraySeqBuilder[A](implicit struct: ValType[A]) extends Builder[Any, A] {
  override type State = ArraySeq[A]
  
  private[this] var data: Data = Data.alloc[A](0)
  
  private[this] var aliased: Boolean = false
  
  private[this] var length: Int = 0
  
  private[this] def capacity: Int = (data.size / struct.size.toLong).toInt
  
  private[this] def expand(base: Int, size: Int): Int = {
    var n = (base max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
  
  private[this] def resize(size: Int) {
    data = data.copy(struct.size.toLong * size.toLong)
  }
  
  private[this] def prepare(size: Int) {
    if (aliased || size > capacity) {
      resize(expand(16, size))
      aliased = false
    }
  }
  
  override def += (value: A): this.type = {
    prepare(length + 1)
    struct.store(data, struct.size.toLong * length.toLong, value)
    length += 1
    this
  }
  
  override def expect(count: Int): this.type = {
    if (length + count > capacity) {
      resize(length + count)
      aliased = false
    }
    this
  }
  
  override def state: ArraySeq[A] = {
    if (length != capacity) resize(length)
    aliased = true
    new ValArraySeq(data, length)
  }
  
  override def clear() {
    data = Data.alloc[A](0)
    aliased = false
    length = 0
  }
}
