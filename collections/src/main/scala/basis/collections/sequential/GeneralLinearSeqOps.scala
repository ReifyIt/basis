//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis.util._
import scala.reflect.macros._

final class GeneralLinearSeqOps[+A](val __ : LinearSeq[A]) extends AnyVal {
  def choose[B](q: PartialFunction[A, B]): Maybe[B]    = macro GeneralLinearSeqMacros.choose[A, B]
  def count(p: A => Boolean): Int                      = macro GeneralLinearSeqMacros.count[A]
  def exists(p: A => Boolean): Boolean                 = macro GeneralLinearSeqMacros.exists[A]
  def find(p: A => Boolean): Maybe[A]                  = macro GeneralLinearSeqMacros.find[A]
  def fold[B >: A](z: B)(op: (B, B) => B): B           = macro GeneralLinearSeqMacros.foldLeft[A, B]
  def foldLeft[B](z: B)(op: (B, A) => B): B            = macro GeneralLinearSeqMacros.foldLeft[A, B]
  def forall(p: A => Boolean): Boolean                 = macro GeneralLinearSeqMacros.forall[A]
  def foreach[U](f: A => U): Unit                      = macro GeneralLinearSeqMacros.foreach[A, U]
  def mayReduce[B >: A](op: (B, B) => B): Maybe[B]     = macro GeneralLinearSeqMacros.mayReduceLeft[A, B]
  def mayReduceLeft[B >: A](op: (B, A) => B): Maybe[B] = macro GeneralLinearSeqMacros.mayReduceLeft[A, B]
  def reduce[B >: A](op: (B, B) => B): B               = macro GeneralLinearSeqMacros.reduceLeft[A, B]
  def reduceLeft[B >: A](op: (B, A) => B): B           = macro GeneralLinearSeqMacros.reduceLeft[A, B]

//def eagerly: StrictLinearSeqOps[A, LinearSeq[_]]     = macro GeneralLinearSeqMacros.eagerly[A]
//def lazily: NonStrictLinearSeqOps[A]                 = macro GeneralLinearSeqMacros.lazily[A]
}

private[sequential] class GeneralLinearSeqMacros(override val c: blackbox.Context { type PrefixType <: GeneralLinearSeqOps[_] }) extends LinearSeqMacros(c) {
  import c.{ Expr, prefix }
  import c.universe._

  override def these: Expr[LinearSeq[_]] = Expr[LinearSeq[Any]](q"$prefix.__")
}
