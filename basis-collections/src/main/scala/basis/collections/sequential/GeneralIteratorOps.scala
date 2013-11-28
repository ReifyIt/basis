//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis.util._

final class GeneralIteratorOps[+A](val __ : Iterator[A]) extends AnyVal {
  def length: Int = {
    var count = 0
    while (!__.isEmpty) {
      count += 1
      __.step()
    }
    count
  }

  def foreach[U](f: A => U): Unit =
    macro GeneralIteratorOps.foreach[A, U]

  def fold[B >: A](z: B)(op: (B, B) => B): B =
    macro GeneralIteratorOps.foldLeft[A, B]

  def reduce[B >: A](op: (B, B) => B): B =
    macro GeneralIteratorOps.reduceLeft[A, B]

  def mayReduce[B >: A](op: (B, B) => B): Maybe[B] =
    macro GeneralIteratorOps.mayReduceLeft[A, B]

  def foldLeft[B](z: B)(op: (B, A) => B): B =
    macro GeneralIteratorOps.foldLeft[A, B]

  def reduceLeft[B >: A](op: (B, A) => B): B =
    macro GeneralIteratorOps.reduceLeft[A, B]

  def mayReduceLeft[B >: A](op: (B, A) => B): Maybe[B] =
    macro GeneralIteratorOps.mayReduceLeft[A, B]

  def find(p: A => Boolean): Maybe[A] =
    macro GeneralIteratorOps.find[A]

  def forall(p: A => Boolean): Boolean =
    macro GeneralIteratorOps.forall[A]

  def exists(p: A => Boolean): Boolean =
    macro GeneralIteratorOps.exists[A]

  def count(p: A => Boolean): Int =
    macro GeneralIteratorOps.count[A]

  def choose[B](q: PartialFunction[A, B]): Maybe[B] =
    macro GeneralIteratorOps.choose[A, B]

  def eagerly: StrictIteratorOps[A, Iterator[_]] =
    macro GeneralIteratorOps.eagerly[A]

  def lazily: NonStrictIteratorOps[A] =
    macro GeneralIteratorOps.lazily[A]
}

private[sequential] object GeneralIteratorOps {
  import scala.collection.immutable.{ ::, Nil }
  import scala.reflect.macros.Context

  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[Iterator[A]] = {
    import c.{ Expr, mirror, prefix, typeCheck, weakTypeOf, WeakTypeTag }
    import c.universe._
    val Apply(_, these :: Nil) = prefix.tree
    implicit val IteratorATag =
      WeakTypeTag[Iterator[A]](
        appliedType(
          mirror.staticClass("basis.collections.Iterator").toType,
          weakTypeOf[A] :: Nil))
    Expr[Iterator[A]](typeCheck(these, weakTypeOf[Iterator[A]]))
  }

  def foreach[A : c.WeakTypeTag, U : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => U])
    : c.Expr[Unit] =
    new IteratorMacros[c.type](c).foreach[A, U](unApply[A](c))(f)

  def foldLeft[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (z: c.Expr[B])
      (op: c.Expr[(B, A) => B])
    : c.Expr[B] =
    new IteratorMacros[c.type](c).foldLeft[A, B](unApply[A](c))(z)(op)

  def reduceLeft[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(B, A) => B])
    : c.Expr[B] =
    new IteratorMacros[c.type](c).reduceLeft[A, B](unApply[A](c))(op)

  def mayReduceLeft[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(B, A) => B])
    : c.Expr[Maybe[B]] =
    new IteratorMacros[c.type](c).mayReduceLeft[A, B](unApply[A](c))(op)

  def find[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Maybe[A]] =
    new IteratorMacros[c.type](c).find[A](unApply[A](c))(p)

  def forall[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Boolean] =
    new IteratorMacros[c.type](c).forall[A](unApply[A](c))(p)

  def exists[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Boolean] =
    new IteratorMacros[c.type](c).exists[A](unApply[A](c))(p)

  def count[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Int] =
    new IteratorMacros[c.type](c).count[A](unApply[A](c))(p)

  def choose[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[A, B]])
    : c.Expr[Maybe[B]] =
    new IteratorMacros[c.type](c).choose[A, B](unApply[A](c))(q)

  def eagerly[A : c.WeakTypeTag](c: Context): c.Expr[StrictIteratorOps[A, Iterator[_]]] =
    Strict.IteratorToStrictOps[A](c)(unApply[A](c))

  def lazily[A : c.WeakTypeTag](c: Context): c.Expr[NonStrictIteratorOps[A]] =
    NonStrict.IteratorToNonStrictOps[A](c)(unApply[A](c))
}
