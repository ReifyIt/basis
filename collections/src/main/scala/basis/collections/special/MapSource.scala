//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package special

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
    var b: Tree = Select(prefix.tree, "Builder": TermName)
    b = Apply(Select(b, "expect": TermName), Literal(Constant(entries.length)) :: Nil)

    val xs = entries.iterator
    while (xs.hasNext) b = Apply(Select(b, ("+=": TermName).encodedName), xs.next().tree :: Nil)

    Expr[CC](Select(b, "state": TermName))
  }
}
