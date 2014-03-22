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

private[special] class CollectionSourceMacros(val c: blackbox.Context { type PrefixType <: CollectionSource[_, _] }) {
  import c.{ Expr, prefix }
  import c.universe._

  def apply[CC, A](elems: Expr[A]*)(implicit CC: WeakTypeTag[CC]): Expr[CC] = {
    var b: Tree = Select(prefix.tree, "Builder": TermName)
    b = Apply(Select(b, "expect": TermName), Literal(Constant(elems.length)) :: Nil)

    val xs = elems.iterator
    while (xs.hasNext) b = Apply(Select(b, ("+=": TermName).encodedName), xs.next().tree :: Nil)

    Expr[CC](Select(b, "state": TermName))
  }
}
