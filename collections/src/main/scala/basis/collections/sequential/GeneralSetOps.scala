//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis.util._
import scala.reflect.macros._

final class GeneralSetOps[+A](val __ : Set[A]) extends AnyVal {
  def choose[B](q: PartialFunction[A, B]): Maybe[B]    = macro GeneralSetMacros.choose[A, B]
  def count(p: A => Boolean): Int                      = macro GeneralSetMacros.count[A]
  def exists(p: A => Boolean): Boolean                 = macro GeneralSetMacros.exists[A]
  def find(p: A => Boolean): Maybe[A]                  = macro GeneralSetMacros.find[A]
  def fold[B >: A](z: B)(op: (B, B) => B): B           = macro GeneralSetMacros.foldLeft[A, B]
  def foldLeft[B](z: B)(op: (B, A) => B): B            = macro GeneralSetMacros.foldLeft[A, B]
  def forall(p: A => Boolean): Boolean                 = macro GeneralSetMacros.forall[A]
  def foreach[U](f: A => U): Unit                      = macro GeneralSetMacros.foreach[A, U]
  def mayReduce[B >: A](op: (B, B) => B): Maybe[B]     = macro GeneralSetMacros.mayReduceLeft[A, B]
  def mayReduceLeft[B >: A](op: (B, A) => B): Maybe[B] = macro GeneralSetMacros.mayReduceLeft[A, B]
  def reduce[B >: A](op: (B, B) => B): B               = macro GeneralSetMacros.reduceLeft[A, B]
  def reduceLeft[B >: A](op: (B, A) => B): B           = macro GeneralSetMacros.reduceLeft[A, B]

//def eagerly: StrictSetOps[A, Set[_]]                 = macro GeneralSetMacros.eagerly[A]
//def lazily: NonStrictSetOps[A]                       = macro GeneralSetMacros.lazily[A]
}

private[sequential] class GeneralSetMacros(override val c: blackbox.Context { type PrefixType <: GeneralSetOps[_] }) extends IteratorMacros(c) {
  import c.{ Expr, prefix }
  import c.universe._

  override def these: Expr[Iterator[_]] = Expr[Iterator[Any]](q"$prefix.__.iterator")
}
