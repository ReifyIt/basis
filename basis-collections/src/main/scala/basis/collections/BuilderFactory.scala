/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.ClassTag

trait BuilderFactory[+CC[_]] {
  type Product[A] = CC[A]
  
  implicit def Builder[A]
      (implicit A: ClassTag[A] = ClassTag.Any.asInstanceOf[ClassTag[A]])
    : Builder[Any, A] { type State = CC[A] @uncheckedVariance }
  
  def empty[A]
      (implicit A: ClassTag[A] = ClassTag.Any.asInstanceOf[ClassTag[A]])
    : CC[A] = Builder(A).state
  
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
    import c.{Expr, prefix, Tree, WeakTypeTag}
    import c.universe._
    var builder = TypeApply(Select(prefix.tree, "Builder"), TypeTree(ATag.tpe) :: Nil): Tree
    builder = Apply(Select(builder, "expect"), Literal(Constant(xs.length)) :: Nil)
    val iter = xs.iterator
    while (iter.hasNext) builder = Apply(Select(builder, "$plus$eq"), iter.next().tree :: Nil)
    Expr(Select(builder, "state"))(WeakTypeTag(appliedType(CCTag.tpe, ATag.tpe :: Nil)))
  }
}
