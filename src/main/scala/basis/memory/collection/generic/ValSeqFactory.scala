/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory
package collection
package generic

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder

import language.higherKinds

/** A factory for sequences that store their elements by value.
  * 
  * @author Chris Sachs
  */
abstract class ValSeqFactory[CC[X] <: RawSeq[X]] extends GenericSeqFactory[CC] { factory =>
  def newBuilder[A](implicit allocator: Allocator, struct: Struct[A]): Builder[A, CC[A]]
  
  protected class ValBuilderFactory[A](implicit val allocator: Allocator, val struct: Struct[A]) extends CanBuildFrom[CC[_], A, CC[A]] {
    def apply(from: CC[_]): Builder[A, CC[A]] = apply()
    def apply(): Builder[A, CC[A]] = factory.newBuilder[A]
  }
}
