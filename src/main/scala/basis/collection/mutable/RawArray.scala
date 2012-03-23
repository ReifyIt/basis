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

/** An array that optionally stores its elements by value.
  * 
  * @author Chris Sachs
  */
trait RawArray[A]
  extends RawSeq[A]
    with GenericTraversableTemplate[A, RawArray]
    with mutable.IndexedSeqLike[A, RawArray[A]] {
  
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
