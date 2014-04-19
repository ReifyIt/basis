//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package special

import scala.reflect.macros._

trait CollectionSource[+CC, -A] {
  def empty: CC = Builder.state

  def apply(elems: A*): CC = macro CollectionSourceMacros.apply[CC, A]

  def from(elems: Traverser[A]): CC = {
    val builder = Builder
    elems.traverse(new Buffer.Append(builder))
    builder.state
  }

  def from(elems: scala.collection.TraversableOnce[A]): CC = {
    val builder = Builder
    elems.foreach(new Buffer.Append(builder))
    builder.state
  }

  implicit def Builder: Builder[A] with State[CC]
}

private[special] class CollectionSourceMacros(override val c: blackbox.Context { type PrefixType <: CollectionSource[_, _] }) extends SourceMacros(c)
