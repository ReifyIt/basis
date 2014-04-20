//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package generic

import basis._
import scala.reflect.macros._

trait CollectionFactory[+CC[_]] {
  def empty[A]: CC[A] = Builder[A].state

  def apply[A](elems: A*): CC[A] = macro CollectionFactoryMacros.apply[CC, A]

  def from[A](elems: Traverser[A]): CC[A] = {
    val builder = Builder[A]
    elems.traverse(new Buffer.Append(builder))
    builder.state
  }

  def from[A](elems: scala.collection.TraversableOnce[A]): CC[A] = {
    val builder = Builder[A]
    elems.foreach(new Buffer.Append(builder))
    builder.state
  }

  implicit def Builder[A]: Builder[A] with State[CC[A]]

  implicit def Factory: CollectionFactory[CC] = this
}

private[generic] class CollectionFactoryMacros(override val c: blackbox.Context { type PrefixType <: CollectionFactory[CC] forSome { type CC[_] } }) extends FactoryMacros(c)
