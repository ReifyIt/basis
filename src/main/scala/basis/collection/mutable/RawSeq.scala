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

/** A mutable indexed sequence that optionally stores its elements by value.
  * 
  * @author Chris Sachs
  */
trait RawSeq[A]
  extends mutable.IndexedSeq[A]
    with basis.collection.RawSeq[A]
    with GenericTraversableTemplate[A, RawSeq]
    with mutable.IndexedSeqLike[A, RawSeq[A]] {
  
  override def companion: GenericCompanion[RawSeq] = RawSeq.opponent
}

/** A factory for `RawSeq` sequences. */
object RawSeq extends RawSeqFactory[RawSeq] {
  implicit def canBuildFrom[A](implicit allocator: Allocator, raw: Option[Struct[A]]): CanBuildFrom[RawSeq[_], A, RawSeq[A]] =
    new RawBuilderFactory[A]
  
  def newBuilder[A](implicit allocator: Allocator, raw: Option[Struct[A]]): Builder[A, RawSeq[A]] =
    RawBuffer.newBuilder[A]
}
