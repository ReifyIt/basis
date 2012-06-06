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
import scala.collection.mutable.Builder
import scala.collection.mutable.IndexedSeqLike
import scala.collection.mutable.IndexedSeqOptimized

import basis.memory.collection.generic.BuilderProxy
import basis.memory.collection.generic.ValSeqFactory

/** An array that stores its elements by value.
  * 
  * @author Chris Sachs
  */
class ValArray[A]
    (val data: Data, val baseAddress: Long, val length: Int)
    (implicit protected val allocator: Allocator, val struct: Struct[A])
  extends RawArray[A]
    with GenericTraversableTemplate[A, RawArray]
    with IndexedSeqLike[A, ValArray[A]]
    with IndexedSeqOptimized[A, ValArray[A]] {
  
  require(baseAddress + struct.size * length <= data.size)
  
  def this(length: Int)(implicit allocator: Allocator, struct: Struct[A]) =
    this(allocator.alloc[A](length), 0L, length)
  
  def apply(index: Int): A = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    struct.load(data, baseAddress + struct.size * index)
  }
  
  def update(index: Int, value: A) {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    struct.store(data, baseAddress + struct.size * index, value)
  }
  
  override def foreach[U](f: A => U) {
    val bound = struct.size * length
    var p = baseAddress
    while (p < bound) {
      f(struct.load(data, p))
      p += struct.size
    }
  }
  
  override protected def newBuilder: Builder[A, ValArray[A]] = ValArray.newBuilder[A]
  
  override def stringPrefix: String = "ValArray"
}

/** A factory for `ValArray` sequences. */
object ValArray extends ValSeqFactory[ValArray] {
  implicit def canBuildFrom[A](implicit allocator: Allocator, struct: Struct[A]): CanBuildFrom[ValArray[_], A, ValArray[A]] =
    new ValBuilderFactory[A]
  
  def newBuilder[A](implicit allocator: Allocator, struct: Struct[A]): Builder[A, ValArray[A]] =
    new ValBuilder[A]
  
  protected class ValBuilder[A](implicit allocator: Allocator, struct: Struct[A]) extends BuilderProxy[A, ValArray[A]] {
    val self = new ValBuffer[A]
    def result() = new ValArray[A](self.data, 0L, self.length)
  }
}
