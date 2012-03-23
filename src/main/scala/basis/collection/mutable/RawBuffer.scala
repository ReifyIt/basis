/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection
package mutable

import scala.collection._
import scala.collection.generic._
import scala.collection.mutable.Builder

import basis.collection.generic._
import basis.memory._

/** A buffer that optionally stores its elements by value.
  * 
  * @author Chris Sachs
  */
trait RawBuffer[A]
  extends mutable.Buffer[A]
    with RawSeq[A]
    with GenericTraversableTemplate[A, RawBuffer]
    with mutable.BufferLike[A, RawBuffer[A]]
    with mutable.IndexedSeqLike[A, RawBuffer[A]] {
  
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
