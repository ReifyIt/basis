/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory
package collection
package mutable

import scala.collection.CustomParallelizable
import scala.collection.generic.CanBuildFrom
import scala.collection.generic.GenericTraversableTemplate
import scala.collection.mutable.BufferLike
import scala.collection.mutable.Builder
import scala.collection.mutable.IndexedSeqLike
import scala.collection.mutable.IndexedSeqOptimized
import scala.collection.parallel.mutable.ParArray

import basis.memory.collection.generic.RefSeqFactory

/** A buffer that stores its elements by reference.
  * 
  * @author Chris Sachs
  */
class RefBuffer[A](protected val initialCapacity: Int)
  extends RawBuffer[A]
    with GenericTraversableTemplate[A, RawBuffer]
    with BufferLike[A, RefBuffer[A]]
    with IndexedSeqLike[A, RefBuffer[A]]
    with IndexedSeqOptimized[A, RefBuffer[A]]
    with Builder[A, RefBuffer[A]]
    with CustomParallelizable[A, ParArray[A]] {
  
  def this() = this(16)
  
  protected var buffer: Array[AnyRef] = null
  
  protected var limit: Int = 0
  
  protected def capacity: Int = buffer.length
  
  def array: Array[AnyRef] = {
    sizeHint(0)
    buffer
  }
  
  def length: Int = limit
  
  def apply(index: Int): A = {
    if (index < 0 || index >= limit) throw new IndexOutOfBoundsException(index.toString)
    buffer(index).asInstanceOf[A]
  }
  
  def update(index: Int, value: A) {
    if (index < 0 || index >= limit) throw new IndexOutOfBoundsException(index.toString)
    buffer(index) = value.asInstanceOf[AnyRef]
  }
  
  def += (value: A): this.type = {
    ensureCapacity(limit + 1)
    buffer(limit) = value.asInstanceOf[AnyRef]
    limit += 1
    this
  }
  
  def +=: (value: A): this.type = {
    ensureCapacity(limit + 1)
    Array.copy(buffer, 0, buffer, 1, limit)
    buffer(0) = value.asInstanceOf[AnyRef]
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
    Array.copy(buffer, index, buffer, index + count, limit - index)
    values.copyToArray(buffer.asInstanceOf[Array[Any]], index, count)
    limit += count
  }
  
  override def remove(index: Int, count: Int) {
    require(count >= 0)
    if (index < 0 || index > limit - count) throw new IndexOutOfBoundsException(index.toString)
    Array.copy(buffer, index + count, buffer, index, limit - (index + count))
    val target = limit - count
    var i = limit
    while (limit > target) {
      limit -= 1
      buffer(limit) = null
    }
  }
  
  def remove(index: Int): A = {
    val removed = apply(index)
    remove(index, 1)
    removed
  }
  
  def clear() {
    var i = limit
    while (limit > 0) {
      limit -= 1
      buffer(limit) = null
    }
  }
  
  override def foreach[U](f: A => U) {
    var i = 0
    while (i < limit) {
      f(buffer(i).asInstanceOf[A])
      i += 1
    }
  }
  
  def result(): RefBuffer[A] = {
    sizeHint(0)
    this
  }
  
  override def sizeHint(count: Int) {
    if (buffer == null) buffer = new Array[AnyRef](count)
    else if (capacity < count) {
      var length = math.max(2 * capacity, 1)
      while (length < count) length = 2 * length
      val expanded = new Array[AnyRef](length)
      Array.copy(buffer, 0, expanded, 0, limit)
      buffer = expanded
    }
  }
  
  protected def ensureCapacity(count: Int) {
    if (buffer == null) buffer = new Array[AnyRef](math.max(count, initialCapacity))
    else sizeHint(count)
  }
  
  override def par: ParArray[A] = ParArray.handoff(buffer.asInstanceOf[Array[A]], limit)
  
  override protected def newBuilder: Builder[A, RefBuffer[A]] = RefBuffer.newBuilder[A]
  
  override def stringPrefix: String = "RefBuffer"
}

/** A factory for `RefBuffer` sequences. */
object RefBuffer extends RefSeqFactory[RefBuffer] {
  implicit def canBuildFrom[A]: CanBuildFrom[RefBuffer[_], A, RefBuffer[A]] =
    RefBuilderFactory.asInstanceOf[RefBuilderFactory[A]]
  
  def newBuilder[A]: Builder[A, RefBuffer[A]] =
    new RefBuffer[A]
}
