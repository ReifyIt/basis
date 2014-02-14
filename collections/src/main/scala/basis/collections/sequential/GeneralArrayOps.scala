//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis.util._

final class GeneralArrayOps[A](val __ : Array[A]) extends AnyVal {
  @inline def choose[B](q: PartialFunction[A, B]): Maybe[B]     = macro GeneralArrayOps.choose[A, B]
  @inline def count(p: A => Boolean): Int                       = macro GeneralArrayOps.count[A]
  @inline def eagerly: StrictArrayOps[A, Array[_]]              = macro GeneralArrayOps.eagerly[A]
  @inline def exists(p: A => Boolean): Boolean                  = macro GeneralArrayOps.exists[A]
  @inline def find(p: A => Boolean): Maybe[A]                   = macro GeneralArrayOps.find[A]
  @inline def foldLeft[B](z: B)(op: (B, A) => B): B             = macro GeneralArrayOps.foldLeft[A, B]
  @inline def foldRight[B](z: B)(op: (A, B) => B): B            = macro GeneralArrayOps.foldRight[A, B]
  @inline def fold[B >: A](z: B)(op: (B, B) => B): B            = macro GeneralArrayOps.foldLeft[A, B]
  @inline def forall(p: A => Boolean): Boolean                  = macro GeneralArrayOps.forall[A]
  @inline def foreach[U](f: A => U): Unit                       = macro GeneralArrayOps.foreach[A, U]
  @inline def isEmpty: Boolean                                  = macro GeneralArrayOps.isEmpty[A]
  @inline def lazily: NonStrictArrayOps[A]                      = macro GeneralArrayOps.lazily[A]
  @inline def mayReduceLeft[B >: A](op: (B, A) => B): Maybe[B]  = macro GeneralArrayOps.mayReduceLeft[A, B]
  @inline def mayReduceRight[B >: A](op: (A, B) => B): Maybe[B] = macro GeneralArrayOps.mayReduceRight[A, B]
  @inline def mayReduce[B >: A](op: (B, B) => B): Maybe[B]      = macro GeneralArrayOps.mayReduceLeft[A, B]
  @inline def reduceLeft[B >: A](op: (B, A) => B): B            = macro GeneralArrayOps.reduceLeft[A, B]
  @inline def reduceRight[B >: A](op: (A, B) => B): B           = macro GeneralArrayOps.reduceRight[A, B]
  @inline def reduce[B >: A](op: (B, B) => B): B                = macro GeneralArrayOps.reduceLeft[A, B]
}

private[sequential] object GeneralArrayOps {
  import scala.collection.immutable.{ ::, Nil }
  import scala.reflect.macros.Context

  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[Array[A]] = {
    import c.{ Expr, mirror, prefix, typeCheck, weakTypeOf, WeakTypeTag }
    import c.universe._
    val Apply(_, these :: Nil) = prefix.tree
    implicit val ArrayATag = applied[Array, A](c)
    Expr[Array[A]](typeCheck(these, weakTypeOf[Array[A]]))
  }

  def isEmpty[A : c.WeakTypeTag](c: Context): c.Expr[Boolean] =
    new ArrayMacros[c.type](c).isEmpty[A](unApply[A](c))

  def foreach[A : c.WeakTypeTag, U : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => U])
    : c.Expr[Unit] =
    new ArrayMacros[c.type](c).foreach[A, U](unApply[A](c))(f)

  def foldLeft[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (z: c.Expr[B])
      (op: c.Expr[(B, A) => B])
    : c.Expr[B] =
    new ArrayMacros[c.type](c).foldLeft[A, B](unApply[A](c))(z)(op)

  def reduceLeft[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(B, A) => B])
    : c.Expr[B] =
    new ArrayMacros[c.type](c).reduceLeft[A, B](unApply[A](c))(op)

  def mayReduceLeft[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(B, A) => B])
    : c.Expr[Maybe[B]] =
    new ArrayMacros[c.type](c).mayReduceLeft[A, B](unApply[A](c))(op)

  def foldRight[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (z: c.Expr[B])
      (op: c.Expr[(A, B) => B])
    : c.Expr[B] =
    new ArrayMacros[c.type](c).foldRight[A, B](unApply[A](c))(z)(op)

  def reduceRight[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(A, B) => B])
    : c.Expr[B] =
    new ArrayMacros[c.type](c).reduceRight[A, B](unApply[A](c))(op)

  def mayReduceRight[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(A, B) => B])
    : c.Expr[Maybe[B]] =
    new ArrayMacros[c.type](c).mayReduceRight[A, B](unApply[A](c))(op)

  def find[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Maybe[A]] =
    new ArrayMacros[c.type](c).find[A](unApply[A](c))(p)

  def forall[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Boolean] =
    new ArrayMacros[c.type](c).forall[A](unApply[A](c))(p)

  def exists[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Boolean] =
    new ArrayMacros[c.type](c).exists[A](unApply[A](c))(p)

  def count[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Int] =
    new ArrayMacros[c.type](c).count[A](unApply[A](c))(p)

  def choose[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[A, B]])
    : c.Expr[Maybe[B]] =
    new ArrayMacros[c.type](c).choose[A, B](unApply[A](c))(q)

  def eagerly[A : c.WeakTypeTag](c: Context): c.Expr[StrictArrayOps[A, Array[_]]] =
    Strict.ArrayToStrictOps[A](c)(unApply[A](c))

  def lazily[A : c.WeakTypeTag](c: Context): c.Expr[NonStrictArrayOps[A]] =
    NonStrict.ArrayToNonStrictOps[A](c)(unApply[A](c))
}
