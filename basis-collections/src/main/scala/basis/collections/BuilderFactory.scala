/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

import basis.runtime._

import scala.annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.TraversableOnce

/** A factory for buildable collections.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    Factories
  */
@implicitNotFound("No builder factory available for ${CC}.")
trait BuilderFactory[+CC[_]] {
  implicit def Builder[A](implicit A: TypeHint[A])
    : Builder[A] {
      type Scope = CC[X] @uncheckedVariance forSome { type X }
      type State = CC[A] @uncheckedVariance
    }
  
  def empty[A](implicit A: TypeHint[A]): CC[A] =
    Builder(A).state
  
  def coerce[A](elems: Enumerator[A])(implicit A: TypeHint[A]): CC[A] =
    (Builder(A) ++= elems).state
  
  def coerce[A](elems: TraversableOnce[A])(implicit A: TypeHint[A]): CC[A] = {
    val builder = Builder(A)
    elems.foreach(new basis.collections.Builder.Append(builder))
    builder.state
  }
  
  def apply[A](elems: A*): CC[A] =
    macro BuilderFactory.apply[CC, A]
}

private[collections] object BuilderFactory {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def apply[CC[_], A]
      (c: Context { type PrefixType <: BuilderFactory[CC] })
      (elems: c.Expr[A]*)
      (implicit CCTag: c.WeakTypeTag[CC[_]], ATag: c.WeakTypeTag[A])
    : c.Expr[CC[A]] = {
    import c.{Expr, prefix, Tree, weakTypeOf, WeakTypeTag}
    import c.universe._
    
    var b = TypeApply(Select(prefix.tree, "Builder": TermName), TypeTree(weakTypeOf[A]) :: Nil): Tree
    b = Apply(Select(b, "expect": TermName), Literal(Constant(elems.length)) :: Nil)
    
    val xs = elems.iterator
    while (xs.hasNext) b = Apply(Select(b, ("+=": TermName).encodedName), xs.next().tree :: Nil)
    
    implicit val CCATag = WeakTypeTag[CC[A]](appliedType(weakTypeOf[CC[_]], weakTypeOf[A] :: Nil))
    Expr[CC[A]](Select(b, "state": TermName))
  }
}
