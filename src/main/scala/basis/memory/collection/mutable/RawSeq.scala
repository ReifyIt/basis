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
import scala.collection.mutable.Builder
import scala.collection.mutable.IndexedSeq
import scala.collection.mutable.IndexedSeqLike

import basis.memory.collection.generic.RawSeqFactory

/** A mutable indexed sequence that optionally stores its elements by value.
  * 
  * @author Chris Sachs
  */
trait RawSeq[A]
  extends IndexedSeq[A]
    with basis.memory.collection.RawSeq[A]
    with GenericTraversableTemplate[A, RawSeq]
    with IndexedSeqLike[A, RawSeq[A]] {
  
  override def companion: GenericCompanion[RawSeq] = RawSeq.opponent
}

/** A factory for `RawSeq` sequences. */
object RawSeq extends RawSeqFactory[RawSeq] {
  implicit def canBuildFrom[A](implicit allocator: Allocator, raw: Option[Struct[A]]): CanBuildFrom[RawSeq[_], A, RawSeq[A]] =
    new RawBuilderFactory[A]
  
  def newBuilder[A](implicit allocator: Allocator, raw: Option[Struct[A]]): Builder[A, RawSeq[A]] =
    RawBuffer.newBuilder[A]
}
