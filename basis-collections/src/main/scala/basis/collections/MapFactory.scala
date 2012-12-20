/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

import scala.annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.ClassTag

@implicitNotFound("No map factory available for ${CC}.")
trait MapFactory[+CC[_, _]] {
  type Product[A, T] = CC[A, T]
  
  implicit def Factory: this.type = this
  
  implicit def Builder[A, T]
      (implicit A: ClassTag[A] = ClassTag.Any.asInstanceOf[ClassTag[A]],
                T: ClassTag[T] = ClassTag.Any.asInstanceOf[ClassTag[T]])
    : Builder[Any, (A, T)] { type State = CC[A, T] @uncheckedVariance }
  
  def empty[A, T]
      (implicit A: ClassTag[A] = ClassTag.Any.asInstanceOf[ClassTag[A]],
                T: ClassTag[T] = ClassTag.Any.asInstanceOf[ClassTag[T]])
    : CC[A, T] = Builder(A, T).state
  
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
    import c.{Expr, prefix, Tree, WeakTypeTag}
    import c.universe._
    var builder = TypeApply(Select(prefix.tree, "Builder"), TypeTree(ATag.tpe) :: TypeTree(TTag.tpe) :: Nil): Tree
    builder = Apply(Select(builder, "expect"), Literal(Constant(xs.length)) :: Nil)
    val iter = xs.iterator
    while (iter.hasNext) builder = Apply(Select(builder, "$plus$eq"), iter.next().tree :: Nil)
    Expr(Select(builder, "state"))(WeakTypeTag(appliedType(CCTag.tpe, ATag.tpe :: TTag.tpe :: Nil)))
  }
}
