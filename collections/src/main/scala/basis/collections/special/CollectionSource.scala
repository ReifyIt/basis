//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package special

import basis.util._

trait CollectionSource[+CC, -A] {
  def empty: CC = Builder.state

  def apply(elems: A*): CC =
    macro CollectionSource.apply[CC, A]

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

private[special] object CollectionSource {
  import scala.collection.immutable.{ ::, Nil }

  def apply[CC, A]
      (c: ContextWithPre[CollectionSource[CC, A]])
      (elems: c.Expr[A]*)
      (implicit CCTag: c.WeakTypeTag[CC])
    : c.Expr[CC] = {
    import c.{ Expr, prefix }
    import c.universe._

    var b: Tree = Select(prefix.tree, "Builder": TermName)
    b = Apply(Select(b, "expect": TermName), Literal(Constant(elems.length)) :: Nil)

    val xs = elems.iterator
    while (xs.hasNext) b = Apply(Select(b, ("+=": TermName).encodedName), xs.next().tree :: Nil)

    Expr[CC](Select(b, "state": TermName))
  }
}
