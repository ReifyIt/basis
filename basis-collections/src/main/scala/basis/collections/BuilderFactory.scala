/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

trait BuilderFactory[CC[_]] {
  implicit def Builder[A]: Builder[Any, A] { type State = CC[A] }
  
  def apply[A](xs: A*): CC[A] =
    macro BuilderFactory.apply[CC, A]
}

private[collections] object BuilderFactory {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def apply[CC[_], A]
      (c: Context { type PrefixType <: BuilderFactory[CC] })
      (xs: c.Expr[A]*)
      (implicit CCTag: c.WeakTypeTag[CC[_]], ATag: c.WeakTypeTag[A])
    : c.Expr[CC[A]] = {
    import c.{Expr, prefix, WeakTypeTag}
    import c.universe._
    val builder = TypeApply(Select(prefix.tree, "Builder"), TypeTree(ATag.tpe) :: Nil)
    var b = Apply(Select(builder, "expect"), Literal(Constant(xs.length)) :: Nil)
    val iter = xs.iterator
    while (iter.hasNext) b = Apply(Select(b, "$plus$eq"), iter.next().tree :: Nil)
    Expr(Select(b, "state"))(WeakTypeTag(appliedType(CCTag.tpe, ATag.tpe :: Nil)))
  }
}
