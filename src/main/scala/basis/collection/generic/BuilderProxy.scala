/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection
package generic

import scala.collection._
import scala.collection.mutable.Builder

/** A builder that forwards calls to a different builder.
  * 
  * @author Chris Sachs
  */
abstract class BuilderProxy[-Elem, +To] extends Builder[Elem, To] with Proxy { builder =>
  def self: Builder[Elem, _]
  
  final def += (elem: Elem): this.type = {
    self += elem
    this
  }
  
  override final def ++= (xs: TraversableOnce[Elem]): this.type = {
    self ++= xs
    this
  }
  
  final def clear(): Unit =
    self.clear()
  
  override final def sizeHint(size: Int): Unit =
    self.sizeHint(size)
  
  override final def sizeHint(coll: TraversableLike[_, _], delta: Int): Unit =
    self.sizeHint(coll, delta)
  
  override final def sizeHintBounded(size: Int, boundingColl: TraversableLike[_, _]): Unit =
    self.sizeHintBounded(size, boundingColl)
  
  /** Returns a builder that transforms this builder's result. */
  override def mapResult[NewTo](f: To => NewTo): Builder[Elem, NewTo] = new BuilderProxy[Elem, NewTo] {
    def self = builder.self
    def result() = f(builder.result())
  }
}
