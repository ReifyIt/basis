//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package special

import basis.util._

trait MapSource[+CC, -A, -T] {
  def empty: CC = Builder().state

  def apply(entries: (A, T)*): CC =
    macro MapSource.apply[CC, A, T]

  def from(entries: Traverser[(A, T)]): CC = {
    val builder = Builder()
    entries.traverse(new Buffer.Append(builder))
    builder.state
  }

  def from(entries: scala.collection.TraversableOnce[(A, T)]): CC = {
    val builder = Builder()
    entries.foreach(new Buffer.Append(builder))
    builder.state
  }

  implicit def Builder(): Builder[(A, T)] with State[CC]
}

private[special] object MapSource {
  import scala.collection.immutable.{ ::, Nil }

  def apply[CC, A, T]
      (c: ContextWithPre[MapSource[CC, A, T]])
      (entries: c.Expr[(A, T)]*)
      (implicit CCTag: c.WeakTypeTag[CC])
    : c.Expr[CC] = {

    import c.{ Expr, prefix }
    import c.universe._

    var b = Apply(Select(prefix.tree, "Builder": TermName), Nil)
    b = Apply(Select(b, "expect": TermName), Literal(Constant(entries.length)) :: Nil)

    val xs = entries.iterator
    while (xs.hasNext) b = Apply(Select(b, ("+=": TermName).encodedName), xs.next().tree :: Nil)

    Expr[CC](Select(b, "state": TermName))
  }
}
