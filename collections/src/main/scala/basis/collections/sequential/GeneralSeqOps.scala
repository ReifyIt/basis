//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis.util._

final class GeneralSeqOps[+A](val __ : Seq[A]) extends AnyVal {
  def foreach[U](f: A => U): Unit                      = macro GeneralContainerMacros.foreach[A, U]
  def fold[B >: A](z: B)(op: (B, B) => B): B           = macro GeneralContainerMacros.foldLeft[A, B]
  def reduce[B >: A](op: (B, B) => B): B               = macro GeneralContainerMacros.reduceLeft[A, B]
  def mayReduce[B >: A](op: (B, B) => B): Maybe[B]     = macro GeneralContainerMacros.mayReduceLeft[A, B]
  def foldLeft[B](z: B)(op: (B, A) => B): B            = macro GeneralContainerMacros.foldLeft[A, B]
  def reduceLeft[B >: A](op: (B, A) => B): B           = macro GeneralContainerMacros.reduceLeft[A, B]
  def mayReduceLeft[B >: A](op: (B, A) => B): Maybe[B] = macro GeneralContainerMacros.mayReduceLeft[A, B]
  def find(p: A => Boolean): Maybe[A]                  = macro GeneralContainerMacros.find[A]
  def forall(p: A => Boolean): Boolean                 = macro GeneralContainerMacros.forall[A]
  def exists(p: A => Boolean): Boolean                 = macro GeneralContainerMacros.exists[A]
  def count(p: A => Boolean): Int                      = macro GeneralContainerMacros.count[A]
  def choose[B](q: PartialFunction[A, B]): Maybe[B]    = macro GeneralContainerMacros.choose[A, B]
  def eagerly: StrictSeqOps[A, Seq[_]]                 = macro GeneralSeqMacros.eagerly[A]
  def lazily: NonStrictSeqOps[A]                       = macro GeneralSeqMacros.lazily[A]
}

private[sequential] object GeneralSeqMacros {
  import scala.collection.immutable.{ ::, Nil }
  import scala.reflect.macros.Context

  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[Seq[A]] = {
    import c.{ Expr, mirror, prefix, typeCheck, weakTypeOf, WeakTypeTag }
    import c.universe._
    val Apply(_, these :: Nil) = prefix.tree
    implicit val SeqATag =
      WeakTypeTag[Seq[A]](
        appliedType(
          mirror.staticClass("basis.collections.Seq").toType,
          weakTypeOf[A] :: Nil))
    Expr[Seq[A]](typeCheck(these, weakTypeOf[Seq[A]]))
  }

  def eagerly[A : c.WeakTypeTag](c: Context): c.Expr[StrictSeqOps[A, Seq[_]]] =
    Strict.SeqToStrictOps[A](c)(unApply[A](c))

  def lazily[A : c.WeakTypeTag](c: Context): c.Expr[NonStrictSeqOps[A]] =
    NonStrict.SeqToNonStrictOps[A](c)(unApply[A](c))
}
