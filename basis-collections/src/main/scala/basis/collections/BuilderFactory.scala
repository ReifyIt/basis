/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

import basis.runtime._

import scala.annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance

/** A factory for buildable collections. */
@implicitNotFound("No builder factory available for ${CC}.")
trait BuilderFactory[+CC[_]] {
  type Product[A] = CC[A]
  
  implicit def Factory: this.type = this
  
  implicit def Builder[A](implicit A: TypeHint[A]): Builder[Any, A] { type State = CC[A] @uncheckedVariance }
  
  def empty[A](implicit A: TypeHint[A]): CC[A] = Builder(A).state
  
  def coerce[A](elems: Enumerator[A])(implicit A: TypeHint[A]): CC[A] = (Builder(A) ++= elems).state
  
  def apply[A](elems: A*): CC[A] = macro BuilderFactory.apply[CC, A]
}

private[collections] object BuilderFactory {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def apply[CC[_], A]
      (c: Context { type PrefixType <: BuilderFactory[CC] })
      (elems: c.Expr[A]*)
      (implicit CCTag: c.WeakTypeTag[CC[_]], ATag: c.WeakTypeTag[A])
    : c.Expr[CC[A]] = {
    import c.{Expr, prefix, Tree, WeakTypeTag}
    import c.universe._
    var builder = TypeApply(Select(prefix.tree, "Builder"), TypeTree(ATag.tpe) :: Nil): Tree
    builder = Apply(Select(builder, "expect"), Literal(Constant(elems.length)) :: Nil)
    val xs = elems.iterator
    while (xs.hasNext) builder = Apply(Select(builder, "$plus$eq"), xs.next().tree :: Nil)
    Expr(Select(builder, "state"))(WeakTypeTag(appliedType(CCTag.tpe, ATag.tpe :: Nil)))
  }
}
