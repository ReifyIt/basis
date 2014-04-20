//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis._
import scala.reflect.macros._

final class GeneralIteratorOps[+A](val __ : Iterator[A]) extends AnyVal {
  def length: Int = {
    var count = 0
    while (!__.isEmpty) {
      count += 1
      __.step()
    }
    count
  }

  def choose[B](q: PartialFunction[A, B]): Maybe[B]    = macro GeneralIteratorMacros.choose[A, B]
  def count(p: A => Boolean): Int                      = macro GeneralIteratorMacros.count[A]
  def exists(p: A => Boolean): Boolean                 = macro GeneralIteratorMacros.exists[A]
  def find(p: A => Boolean): Maybe[A]                  = macro GeneralIteratorMacros.find[A]
  def fold[B >: A](z: B)(op: (B, B) => B): B           = macro GeneralIteratorMacros.foldLeft[A, B]
  def foldLeft[B](z: B)(op: (B, A) => B): B            = macro GeneralIteratorMacros.foldLeft[A, B]
  def forall(p: A => Boolean): Boolean                 = macro GeneralIteratorMacros.forall[A]
  def foreach[U](f: A => U): Unit                      = macro GeneralIteratorMacros.foreach[A, U]
  def mayReduce[B >: A](op: (B, B) => B): Maybe[B]     = macro GeneralIteratorMacros.mayReduceLeft[A, B]
  def mayReduceLeft[B >: A](op: (B, A) => B): Maybe[B] = macro GeneralIteratorMacros.mayReduceLeft[A, B]
  def reduce[B >: A](op: (B, B) => B): B               = macro GeneralIteratorMacros.reduceLeft[A, B]
  def reduceLeft[B >: A](op: (B, A) => B): B           = macro GeneralIteratorMacros.reduceLeft[A, B]

  def eagerly: StrictIteratorOps[A, Iterator[_]]       = macro GeneralIteratorMacros.eagerly[A]
  def lazily: NonStrictIteratorOps[A]                  = macro GeneralIteratorMacros.lazily[A]
}

private[sequential] class GeneralIteratorMacros(override val c: blackbox.Context { type PrefixType <: GeneralIteratorOps[_] }) extends IteratorMacros(c) {
  import c.{ Expr, prefix, WeakTypeTag }
  import c.universe._

  override def these: Expr[Iterator[_]] = Expr[Iterator[Any]](q"$prefix.__")

  def eagerly[A : WeakTypeTag]: Expr[StrictIteratorOps[A, Iterator[_]]] = StrictOps1[StrictIteratorOps, A](these)
  def lazily[A : WeakTypeTag]: Expr[NonStrictIteratorOps[A]]            = NonStrictOps1[NonStrictIteratorOps, A](these)
}
