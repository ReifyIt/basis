/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection
package generic

import scala.collection.generic._
import scala.collection.mutable.Builder

import basis.memory._

/** A factory for sequences that optionally store their elements by value.
  * 
  * @author Chris Sachs
  */
abstract class RawSeqFactory[CC[X] <: RawSeq[X]] extends GenericSeqFactory[CC] { factory =>
   def newBuilder[A](implicit allocator: Allocator, raw: Option[Struct[A]]): Builder[A, CC[A]]
  
  /** A pseudo-companion object to satisfy `GenericTraversableTemplate`. */
  lazy val opponent: GenericCompanion[CC] = new Opponent
  
  /** A shim for compatibility with `GenericTraversableTemplate`'s superfluous
    * dependency on a companion object. */
  protected class Opponent extends GenericCompanion[CC] {
    def newBuilder[A]: Builder[A, CC[A]] = factory.newBuilder[A](implicitly, None)
  }
  
  protected class RawBuilderFactory[A](implicit allocator: Allocator, raw: Option[Struct[A]]) extends CanBuildFrom[CC[_], A, CC[A]] {
    def apply(from: CC[_]): Builder[A, CC[A]] = apply()
    def apply(): Builder[A, CC[A]] = factory.newBuilder[A]
  }
}
