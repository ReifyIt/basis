//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis.util._

final class GeneralLinearSeqOps[+A](val __ : LinearSeq[A]) extends AnyVal {
  def foreach[U](f: A => U): Unit                      = macro GeneralLinearSeqMacros.foreach[A, U]
  def fold[B >: A](z: B)(op: (B, B) => B): B           = macro GeneralLinearSeqMacros.foldLeft[A, B]
  def reduce[B >: A](op: (B, B) => B): B               = macro GeneralLinearSeqMacros.reduceLeft[A, B]
  def mayReduce[B >: A](op: (B, B) => B): Maybe[B]     = macro GeneralLinearSeqMacros.mayReduceLeft[A, B]
  def foldLeft[B](z: B)(op: (B, A) => B): B            = macro GeneralLinearSeqMacros.foldLeft[A, B]
  def reduceLeft[B >: A](op: (B, A) => B): B           = macro GeneralLinearSeqMacros.reduceLeft[A, B]
  def mayReduceLeft[B >: A](op: (B, A) => B): Maybe[B] = macro GeneralLinearSeqMacros.mayReduceLeft[A, B]
  def find(p: A => Boolean): Maybe[A]                  = macro GeneralLinearSeqMacros.find[A]
  def forall(p: A => Boolean): Boolean                 = macro GeneralLinearSeqMacros.forall[A]
  def exists(p: A => Boolean): Boolean                 = macro GeneralLinearSeqMacros.exists[A]
  def count(p: A => Boolean): Int                      = macro GeneralLinearSeqMacros.count[A]
  def choose[B](q: PartialFunction[A, B]): Maybe[B]    = macro GeneralLinearSeqMacros.choose[A, B]
  def eagerly: StrictLinearSeqOps[A, LinearSeq[_]]     = macro GeneralLinearSeqMacros.eagerly[A]
  def lazily: NonStrictLinearSeqOps[A]                 = macro GeneralLinearSeqMacros.lazily[A]
}

private[sequential] object GeneralLinearSeqMacros {
  import scala.collection.immutable.{ ::, Nil }
  import scala.reflect.macros.Context

  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[LinearSeq[A]] = {
    import c.{ Expr, mirror, prefix, typeCheck, weakTypeOf, WeakTypeTag }
    import c.universe._
    val Apply(_, these :: Nil) = prefix.tree
    implicit val SLinkATag =
      WeakTypeTag[LinearSeq[A]](
        appliedType(
          mirror.staticClass("basis.collections.LinearSeq").toType,
          weakTypeOf[A] :: Nil))
    Expr[LinearSeq[A]](typeCheck(these, weakTypeOf[LinearSeq[A]]))
  }

  def foreach[A : c.WeakTypeTag, U : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => U])
    : c.Expr[Unit] =
    new LinearSeqMacros[c.type](c).foreach[A, U](unApply[A](c))(f)

  def foldLeft[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (z: c.Expr[B])
      (op: c.Expr[(B, A) => B])
    : c.Expr[B] =
    new LinearSeqMacros[c.type](c).foldLeft[A, B](unApply[A](c))(z)(op)

  def reduceLeft[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(B, A) => B])
    : c.Expr[B] =
    new LinearSeqMacros[c.type](c).reduceLeft[A, B](unApply[A](c))(op)

  def mayReduceLeft[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(B, A) => B])
    : c.Expr[Maybe[B]] =
    new LinearSeqMacros[c.type](c).mayReduceLeft[A, B](unApply[A](c))(op)

  def find[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Maybe[A]] =
    new LinearSeqMacros[c.type](c).find[A](unApply[A](c))(p)

  def forall[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Boolean] =
    new LinearSeqMacros[c.type](c).forall[A](unApply[A](c))(p)

  def exists[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Boolean] =
    new LinearSeqMacros[c.type](c).exists[A](unApply[A](c))(p)

  def count[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Int] =
    new LinearSeqMacros[c.type](c).count[A](unApply[A](c))(p)

  def choose[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[A, B]])
    : c.Expr[Maybe[B]] =
    new LinearSeqMacros[c.type](c).choose[A, B](unApply[A](c))(q)

  def eagerly[A : c.WeakTypeTag](c: Context): c.Expr[StrictLinearSeqOps[A, LinearSeq[_]]] =
    Strict.LinearSeqToStrictOps[A](c)(unApply[A](c))

  def lazily[A : c.WeakTypeTag](c: Context): c.Expr[NonStrictLinearSeqOps[A]] =
    NonStrict.LinearSeqToNonStrictOps[A](c)(unApply[A](c))
}
