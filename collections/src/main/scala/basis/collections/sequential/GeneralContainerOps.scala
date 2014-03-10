//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis.util._
import scala.reflect.macros._

final class GeneralContainerOps[+A](val __ : Container[A]) extends AnyVal {
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
//def eagerly: StrictContainerOps[A, Container[_]]     = macro GeneralContainerMacros.eagerly[A]
//def lazily: NonStrictContainerOps[A]                 = macro GeneralContainerMacros.lazily[A]
}

private[sequential] class GeneralContainerMacros(val c: whitebox.Context { type PrefixType <: GeneralContainerOps[_] }) {
  import c.{ Expr, mirror, prefix, WeakTypeTag }
  import c.universe._

  def foreach[A : WeakTypeTag, U : WeakTypeTag](f: Expr[A => U]): Expr[Unit]                      = new IteratorMacros[c.type](c).foreach[A, U](iterator[A])(f)
  def foldLeft[A : WeakTypeTag, B : WeakTypeTag](z: Expr[B])(op: Expr[(B, A) => B]): Expr[B]      = new IteratorMacros[c.type](c).foldLeft[A, B](iterator[A])(z)(op)
  def reduceLeft[A : WeakTypeTag, B >: A : WeakTypeTag](op: Expr[(B, A) => B]): Expr[B]           = new IteratorMacros[c.type](c).reduceLeft[A, B](iterator[A])(op)
  def mayReduceLeft[A : WeakTypeTag, B >: A : WeakTypeTag](op: Expr[(B, A) => B]): Expr[Maybe[B]] = new IteratorMacros[c.type](c).mayReduceLeft[A, B](iterator[A])(op)
  def find[A : WeakTypeTag](p: Expr[A => Boolean]): Expr[Maybe[A]]                                = new IteratorMacros[c.type](c).find[A](iterator[A])(p)
  def forall[A : WeakTypeTag](p: Expr[A => Boolean]): Expr[Boolean]                               = new IteratorMacros[c.type](c).forall[A](iterator[A])(p)
  def exists[A : WeakTypeTag](p: Expr[A => Boolean]): Expr[Boolean]                               = new IteratorMacros[c.type](c).exists[A](iterator[A])(p)
  def count[A : WeakTypeTag](p: Expr[A => Boolean]): Expr[Int]                                    = new IteratorMacros[c.type](c).count[A](iterator[A])(p)
  def choose[A : WeakTypeTag, B : WeakTypeTag](q: Expr[PartialFunction[A, B]]): Expr[Maybe[B]]    = new IteratorMacros[c.type](c).choose[A, B](iterator[A])(q)

//def eagerly[A : WeakTypeTag](c: Context): Expr[StrictContainerOps[A, Container[_]]] =
//  Strict.ContainerToStrictOps[A](c)(unApply[A](c))

//def lazily[A : WeakTypeTag](c: Context): Expr[NonStrictContainerOps[A]] =
//  NonStrict.ContainerToNonStrictOps[A](c)(unApply[A](c))

  protected def iterator[A : WeakTypeTag]: Expr[Iterator[A]] = Expr[Iterator[A]](q"$prefix.__.iterator")

  implicit protected def IteratorTag[A](implicit A: WeakTypeTag[A]): WeakTypeTag[Iterator[A]] = WeakTypeTag(appliedType(mirror.staticClass(s"basis.collections.Iterator").toTypeConstructor, A.tpe :: Nil))
  implicit protected def MaybeTag[A](implicit A: WeakTypeTag[A]): WeakTypeTag[Maybe[A]] = WeakTypeTag(appliedType(mirror.staticClass(s"basis.collections.Maybe").toTypeConstructor, A.tpe :: Nil))
}
