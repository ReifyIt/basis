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

/** A factory for sequences that store their elements by reference.
  * 
  * @author Chris Sachs
  */
abstract class RefSeqFactory[CC[X] <: RawSeq[X]] extends GenericSeqFactory[CC] { factory =>
  def newBuilder[A]: Builder[A, CC[A]]
  
  /** A reusable builder factory. */
  protected lazy val RefBuilderFactory = new RefBuilderFactory[Nothing]
  
  protected class RefBuilderFactory[A] extends CanBuildFrom[CC[_], A, CC[A]] {
    def apply(from: CC[_]): Builder[A, CC[A]] = apply()
    def apply(): Builder[A, CC[A]] = factory.newBuilder[A]
  }
}
