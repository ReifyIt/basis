/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory
package collection
package mutable

import scala.collection.generic.CanBuildFrom
import scala.collection.generic.GenericTraversableTemplate
import scala.collection.mutable.BufferLike
import scala.collection.mutable.Builder
import scala.collection.mutable.IndexedSeqLike
import scala.collection.mutable.IndexedSeqOptimized

import basis.memory.collection.generic.ValSeqFactory

/** A buffer that stores its elements by value.
  * 
  * @author Chris Sachs
  */
class ValBuffer[A]
    (protected val initialCapacity: Int)
    (implicit protected val allocator: Allocator, val struct: Struct[A])
  extends RawBuffer[A]
    with GenericTraversableTemplate[A, RawBuffer]
    with BufferLike[A, ValBuffer[A]]
    with IndexedSeqLike[A, ValBuffer[A]]
    with IndexedSeqOptimized[A, ValBuffer[A]]
    with Builder[A, ValBuffer[A]] {
  
  def this()(implicit allocator: Allocator, struct: Struct[A]) = this(16)
  
  protected var buffer: Data = null
  
  protected var limit: Int = 0
  
  protected def capacity: Int = (data.size / struct.size).toInt
  
  def data: Data = {
    sizeHint(0)
    buffer
  }
  
  def length: Int = limit
  
  def apply(index: Int): A = {
    if (index < 0 || index >= limit) throw new IndexOutOfBoundsException(index.toString)
    struct.load(buffer, struct.size * index)
  }
  
  def update(index: Int, value: A) {
    if (index < 0 || index >= limit) throw new IndexOutOfBoundsException(index.toString)
    struct.store(buffer, struct.size * index, value)
  }
  
  def += (value: A): this.type = {
    ensureCapacity(limit + 1)
    struct.store(buffer, struct.size * limit, value)
    limit += 1
    this
  }
  
  def +=: (value: A): this.type = {
    ensureCapacity(limit + 1)
    buffer.move(0L, struct.size, struct.size * limit)
    struct.store(buffer, 0L, value)
    limit += 1
    this
  }
  
  override def ++=(values: TraversableOnce[A]): this.type = values match {
    case values: Traversable[_] =>
      insertAll(limit, values)
      this
    case _ => super.++=(values)
  }
  
  def insertAll(index: Int, values: Traversable[A]) {
    if (index < 0 || index > limit) throw new IndexOutOfBoundsException(index.toString)
    val count = values.size // assuming extra traversal is faster than extra copying
    ensureCapacity(limit + count)
    buffer.move(struct.size * index, struct.size * (index + count), struct.size * (limit - index))
    values foreach { value =>
      struct.store(buffer, struct.size * limit, value)
      limit += 1
    }
  }
  
  override def remove(index: Int, count: Int) {
    require(count >= 0)
    if (index < 0 || index > limit - count) throw new IndexOutOfBoundsException(index.toString)
    buffer.move(struct.size * (index + count), struct.size * index, struct.size * (limit - (index + count)))
    buffer.clear(struct.size * (limit - count), struct.size * limit)
    limit -= count
  }
  
  def remove(index: Int): A = {
    val removed = apply(index)
    remove(index, 1)
    removed
  }
  
  def clear() {
    buffer.clear()
    limit = 0
  }
  
  override def foreach[U](f: A => U) {
    val bound = struct.size * limit
    var p = 0L
    while (p < bound) {
      f(struct.load(buffer, p))
      p += struct.size
    }
  }
  
  def result(): ValBuffer[A] = {
    sizeHint(0)
    this
  }
  
  override def sizeHint(count: Int) {
    if (buffer == null) buffer = allocator.alloc[A](count)
    else {
      val need = struct.size * count
      if (buffer.size < need) {
        var size = math.max(2 * buffer.size, struct.size)
        while (size < need) size = 2 * size
        buffer = buffer.copy(size)
      }
    }
  }
  
  protected def ensureCapacity(count: Int) {
    if (buffer == null) buffer = allocator.alloc[A](math.max(count, initialCapacity))
    else sizeHint(count)
  }
  
  override protected def newBuilder: Builder[A, ValBuffer[A]] = ValBuffer.newBuilder[A]
  
  override def stringPrefix: String = "ValBuffer"
}

/** A factory for `ValBuffer` sequences. */
object ValBuffer extends ValSeqFactory[ValBuffer] {
  implicit def canBuildFrom[A](implicit allocator: Allocator, struct: Struct[A]): CanBuildFrom[ValBuffer[_], A, ValBuffer[A]] =
    new ValBuilderFactory[A]
  
  def newBuilder[A](implicit allocator: Allocator, struct: Struct[A]): Builder[A, ValBuffer[A]] =
    new ValBuffer[A]
}
