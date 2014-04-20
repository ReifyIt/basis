//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package special

import basis._
import scala.reflect.macros._

trait MapSource[+CC, -A, -T] {
  def empty: CC = Builder.state

  def apply(entries: (A, T)*): CC = macro MapSourceMacros.apply[CC, A, T]

  def from(entries: Traverser[(A, T)]): CC = {
    val builder = Builder
    entries.traverse(new Buffer.Append(builder))
    builder.state
  }

  def from(entries: scala.collection.TraversableOnce[(A, T)]): CC = {
    val builder = Builder
    entries.foreach(new Buffer.Append(builder))
    builder.state
  }

  implicit def Builder(): Builder[(A, T)] with State[CC]
}

private[special] class MapSourceMacros(val c: blackbox.Context { type PrefixType <: MapSource[_, _, _] }) {
  import c.{ Expr, prefix }
  import c.universe._

  def apply[CC, A, T](entries: Expr[(A, T)]*)(implicit CC: WeakTypeTag[CC]): Expr[CC] = {
    val n = q"${entries.length}"
    var b = q"$prefix.Builder.expect($n)"

    val xs = entries.iterator
    while (xs.hasNext) b = q"$b += ${xs.next()}"

    Expr[CC](q"$b.state")
  }
}
