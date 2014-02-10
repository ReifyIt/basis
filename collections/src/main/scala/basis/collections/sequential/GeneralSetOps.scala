//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis.util._

final class GeneralSetOps[+A](val __ : Set[A]) extends AnyVal {
  def foreach[U](f: A => U): Unit =
    macro GeneralContainerOps.foreach[A, U]

  def fold[B >: A](z: B)(op: (B, B) => B): B =
    macro GeneralContainerOps.foldLeft[A, B]

  def reduce[B >: A](op: (B, B) => B): B =
    macro GeneralContainerOps.reduceLeft[A, B]

  def mayReduce[B >: A](op: (B, B) => B): Maybe[B] =
    macro GeneralContainerOps.mayReduceLeft[A, B]

  def foldLeft[B](z: B)(op: (B, A) => B): B =
    macro GeneralContainerOps.foldLeft[A, B]

  def reduceLeft[B >: A](op: (B, A) => B): B =
    macro GeneralContainerOps.reduceLeft[A, B]

  def mayReduceLeft[B >: A](op: (B, A) => B): Maybe[B] =
    macro GeneralContainerOps.mayReduceLeft[A, B]

  def find(p: A => Boolean): Maybe[A] =
    macro GeneralContainerOps.find[A]

  def forall(p: A => Boolean): Boolean =
    macro GeneralContainerOps.forall[A]

  def exists(p: A => Boolean): Boolean =
    macro GeneralContainerOps.exists[A]

  def count(p: A => Boolean): Int =
    macro GeneralContainerOps.count[A]

  def choose[B](q: PartialFunction[A, B]): Maybe[B] =
    macro GeneralContainerOps.choose[A, B]

  def eagerly: StrictSetOps[A, Set[_]] =
    macro GeneralSetOps.eagerly[A]

  def lazily: NonStrictSetOps[A] =
    macro GeneralSetOps.lazily[A]
}

private[sequential] object GeneralSetOps {
  import scala.collection.immutable.{ ::, Nil }
  import scala.reflect.macros.Context

  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[Set[A]] = {
    import c.{ Expr, mirror, prefix, typeCheck, weakTypeOf, WeakTypeTag }
    import c.universe._
    val Apply(_, these :: Nil) = prefix.tree
    implicit val SetATag =
      WeakTypeTag[Set[A]](
        appliedType(
          mirror.staticClass("basis.collections.Set").toType,
          weakTypeOf[A] :: Nil))
    Expr[Set[A]](typeCheck(these, weakTypeOf[Set[A]]))
  }

  def eagerly[A : c.WeakTypeTag](c: Context): c.Expr[StrictSetOps[A, Set[_]]] =
    Strict.SetToStrictOps[A](c)(unApply[A](c))

  def lazily[A : c.WeakTypeTag](c: Context): c.Expr[NonStrictSetOps[A]] =
    NonStrict.SetToNonStrictOps[A](c)(unApply[A](c))
}
