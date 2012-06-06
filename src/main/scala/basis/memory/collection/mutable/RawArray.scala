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
import scala.collection.mutable.IndexedSeqLike

import basis.memory.collection.generic.RawSeqFactory

/** An array that optionally stores its elements by value.
  * 
  * @author Chris Sachs
  */
trait RawArray[A]
  extends RawSeq[A]
    with GenericTraversableTemplate[A, RawArray]
    with IndexedSeqLike[A, RawArray[A]] {
  
  override def companion: GenericCompanion[RawArray] = RawArray.opponent
}

/** A factory for `RawArray` sequences. */
object RawArray extends RawSeqFactory[RawArray] {
  implicit def canBuildFrom[A](implicit allocator: Allocator, raw: Option[Struct[A]]): CanBuildFrom[RawArray[_], A, RawArray[A]] =
    new RawBuilderFactory[A]
  
  def newBuilder[A](implicit allocator: Allocator, raw: Option[Struct[A]]): Builder[A, RawArray[A]] = raw match {
    case Some(struct) => ValArray.newBuilder[A](allocator, struct)
    case None => RefArray.newBuilder[A]
  }
}
