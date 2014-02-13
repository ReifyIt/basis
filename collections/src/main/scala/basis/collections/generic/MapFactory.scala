//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package generic

import basis.util._

trait MapFactory[+CC[_, _]] {
  def empty[A, T]: CC[A, T] = Builder[A, T]().state

  def apply[A, T](entries: (A, T)*): CC[A, T] =
    macro MapFactory.apply[CC, A, T]

  def from[A, T](entries: Traverser[(A, T)]): CC[A, T] = {
    val builder = Builder[A, T]()
    entries traverse new Buffer.Append(builder)
    builder.state
  }

  def from[A, T](entries: TraversableOnce[(A, T)]): CC[A, T] = {
    val builder = Builder[A, T]()
    entries foreach new Buffer.Append(builder)
    builder.state
  }

  implicit def Builder[A, T](): Builder[(A, T)] with State[CC[A, T]]

  implicit def Factory: MapFactory[CC] = this
}

private[generic] object MapFactory {
  import scala.collection.immutable.{ ::, Nil }

  def apply[CC[_, _], A, T]
      (c: ContextWithPre[MapFactory[CC]])
      (entries: c.Expr[(A, T)]*)
      (implicit CCTag: c.WeakTypeTag[CC[_, _]], ATag: c.WeakTypeTag[A], TTag: c.WeakTypeTag[T])
    : c.Expr[CC[A, T]] = {

    import c.{ Expr, prefix, weakTypeOf, WeakTypeTag }
    import c.universe._

    var b = Apply(TypeApply(Select(prefix.tree, "Builder": TermName), TypeTree(weakTypeOf[A]) :: TypeTree(weakTypeOf[T]) :: Nil), Nil)
    b = Apply(Select(b, "expect": TermName), Literal(Constant(entries.length)) :: Nil)

    val xs = entries.iterator
    while (xs.hasNext) b = Apply(Select(b, ("+=": TermName).encodedName), xs.next().tree :: Nil)

    implicit val CCATTag = WeakTypeTag[CC[A, T]](appliedType(weakTypeOf[CC[_, _]], weakTypeOf[A] :: weakTypeOf[T] :: Nil))
    Expr[CC[A, T]](Select(b, "state": TermName))
  }
}
