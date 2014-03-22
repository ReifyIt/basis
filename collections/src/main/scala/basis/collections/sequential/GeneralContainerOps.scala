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
  def choose[B](q: PartialFunction[A, B]): Maybe[B]    = macro GeneralContainerMacros.choose[A, B]
  def count(p: A => Boolean): Int                      = macro GeneralContainerMacros.count[A]
  def exists(p: A => Boolean): Boolean                 = macro GeneralContainerMacros.exists[A]
  def find(p: A => Boolean): Maybe[A]                  = macro GeneralContainerMacros.find[A]
  def fold[B >: A](z: B)(op: (B, B) => B): B           = macro GeneralContainerMacros.foldLeft[A, B]
  def foldLeft[B](z: B)(op: (B, A) => B): B            = macro GeneralContainerMacros.foldLeft[A, B]
  def forall(p: A => Boolean): Boolean                 = macro GeneralContainerMacros.forall[A]
  def foreach[U](f: A => U): Unit                      = macro GeneralContainerMacros.foreach[A, U]
  def mayReduce[B >: A](op: (B, B) => B): Maybe[B]     = macro GeneralContainerMacros.mayReduceLeft[A, B]
  def mayReduceLeft[B >: A](op: (B, A) => B): Maybe[B] = macro GeneralContainerMacros.mayReduceLeft[A, B]
  def reduce[B >: A](op: (B, B) => B): B               = macro GeneralContainerMacros.reduceLeft[A, B]
  def reduceLeft[B >: A](op: (B, A) => B): B           = macro GeneralContainerMacros.reduceLeft[A, B]

//def eagerly: StrictContainerOps[A, Container[_]]     = macro GeneralContainerMacros.eagerly[A]
//def lazily: NonStrictContainerOps[A]                 = macro GeneralContainerMacros.lazily[A]
}

private[sequential] class GeneralContainerMacros(override val c: blackbox.Context { type PrefixType <: GeneralContainerOps[_] }) extends IteratorMacros(c) {
  import c.{ Expr, prefix }
  import c.universe._

  override def these: Expr[Iterator[_]] = Expr[Iterator[Any]](q"$prefix.__.iterator")
}
