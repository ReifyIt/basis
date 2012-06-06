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
import scala.collection.generic.GenericCompanion
import scala.collection.generic.GenericTraversableTemplate
import scala.collection.mutable.Buffer
import scala.collection.mutable.BufferLike
import scala.collection.mutable.Builder
import scala.collection.mutable.IndexedSeqLike

import basis.memory.collection.generic.RawSeqFactory

/** A buffer that optionally stores its elements by value.
  * 
  * @author Chris Sachs
  */
trait RawBuffer[A]
  extends Buffer[A]
    with RawSeq[A]
    with GenericTraversableTemplate[A, RawBuffer]
    with BufferLike[A, RawBuffer[A]]
    with IndexedSeqLike[A, RawBuffer[A]] {
  
  override def companion: GenericCompanion[RawBuffer] = RawBuffer.opponent
}

/** A factory for `RawBuffer` sequences. */
object RawBuffer extends RawSeqFactory[RawBuffer] {
  implicit def canBuildFrom[A](implicit allocator: Allocator, raw: Option[Struct[A]]): CanBuildFrom[RawBuffer[_], A, RawBuffer[A]] =
    new RawBuilderFactory[A]
  
  def newBuilder[A](implicit allocator: Allocator, raw: Option[Struct[A]]): Builder[A, RawBuffer[A]] = raw match {
    case Some(struct) => ValBuffer.newBuilder[A](allocator, struct)
    case None => RefBuffer.newBuilder[A]
  }
}
