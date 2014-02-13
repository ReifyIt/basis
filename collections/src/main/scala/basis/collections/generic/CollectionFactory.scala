//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package generic

import basis.util._
// applied

trait CollectionFactory[+CC[_]] {
  def empty[A]: CC[A] = Builder[A]().state

  def apply[A](elems: A*): CC[A] =
    macro CollectionFactory.apply[CC, A]

  def from[A](elems: Traverser[A]): CC[A] = {
    val builder = Builder[A]()
    elems.traverse(new Buffer.Append(builder))
    builder.state
  }

  def from[A](elems: scala.collection.TraversableOnce[A]): CC[A] = {
    val builder = Builder[A]()
    elems.foreach(new Buffer.Append(builder))
    builder.state
  }

  implicit def Builder[A](): Builder[A] with State[CC[A]]

  implicit def Factory: CollectionFactory[CC] = this
}

private[generic] object CollectionFactory {
  import scala.collection.immutable.{ ::, Nil }

  def apply[CC[_], A]
      (c: ContextWithPre[CollectionFactory[CC]])
      (elems: c.Expr[A]*)
      (implicit CCTag: c.WeakTypeTag[CC[_]], ATag: c.WeakTypeTag[A])
    : c.Expr[CC[A]] = {

    import c.{ Expr, prefix, weakTypeOf, WeakTypeTag }
    import c.universe._

    var b = Apply(TypeApply(Select(prefix.tree, "Builder": TermName), TypeTree(weakTypeOf[A]) :: Nil), Nil)
    b = Apply(Select(b, "expect": TermName), Literal(Constant(elems.length)) :: Nil)

    val xs = elems.iterator
    while (xs.hasNext) b = Apply(Select(b, ("+=": TermName).encodedName), xs.next().tree :: Nil)

    implicit val CCATag = applied[CC, A](c)
    Expr[CC[A]](Select(b, "state": TermName))
  }
}
