/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package generic

trait MapFactory[+CC[_, _]] {
  def Builder[A, T]: Builder[Any, (A, T), CC[A, T]]
  
  def apply[A, T](xs: (A, T)*): CC[A, T] =
    macro MapFactory.apply[CC, A, T]
}

private[collections] object MapFactory {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def apply[CC[_, _], A, T]
      (c: Context { type PrefixType <: MapFactory[CC] })
      (xs: c.Expr[(A, T)]*)
      (implicit CCTag: c.WeakTypeTag[CC[_, _]], ATag: c.WeakTypeTag[A], TTag: c.WeakTypeTag[T])
    : c.Expr[CC[A, T]] = {
    import c.{Expr, prefix, WeakTypeTag}
    import c.universe._
    val builder = TypeApply(Select(prefix.tree, "Builder"), TypeTree(ATag.tpe) :: TypeTree(TTag.tpe) :: Nil)
    var b = Apply(Select(builder, "expect"), Literal(Constant(xs.length)) :: Nil)
    val iter = xs.iterator
    while (iter.hasNext) b = Apply(Select(b, "$plus$eq"), iter.next().tree :: Nil)
    Expr(Select(b, "state"))(WeakTypeTag[CC[A, T]](appliedType(CCTag.tpe, ATag.tpe :: TTag.tpe :: Nil)))
  }
}
