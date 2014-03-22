//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package generic

import scala.reflect.macros._

trait MapFactory[+CC[_, _]] {
  def empty[A, T]: CC[A, T] = Builder[A, T].state

  def apply[A, T](entries: (A, T)*): CC[A, T] = macro MapFactoryMacros.apply[CC, A, T]

  def from[A, T](entries: Traverser[(A, T)]): CC[A, T] = {
    val builder = Builder[A, T]
    entries traverse new Buffer.Append(builder)
    builder.state
  }

  def from[A, T](entries: TraversableOnce[(A, T)]): CC[A, T] = {
    val builder = Builder[A, T]
    entries foreach new Buffer.Append(builder)
    builder.state
  }

  implicit def Builder[A, T]: Builder[(A, T)] with State[CC[A, T]]

  implicit def Factory: MapFactory[CC] = this
}

private[generic] class MapFactoryMacros(val c: blackbox.Context { type PrefixType <: MapFactory[CC] forSome { type CC[_, _] } }) {
  import c.{ Expr, prefix, WeakTypeTag }
  import c.universe._

  def apply[CC[_, _], A, T](entries: Expr[(A, T)]*)(implicit CC: WeakTypeTag[CC[_, _]], A: WeakTypeTag[A], T: WeakTypeTag[T]): Expr[CC[A, T]] = {
    var b: Tree = TypeApply(Select(prefix.tree, "Builder": TermName), TypeTree(A.tpe) :: TypeTree(weakTypeOf[T]) :: Nil)
    b = Apply(Select(b, "expect": TermName), Literal(Constant(entries.length)) :: Nil)

    val xs = entries.iterator
    while (xs.hasNext) b = Apply(Select(b, ("+=": TermName).encodedName), xs.next().tree :: Nil)

    implicit val CCAT = WeakTypeTag[CC[A, T]](appliedType(CC.tpe, A.tpe :: T.tpe :: Nil))
    Expr[CC[A, T]](Select(b, "state": TermName))
  }
}
