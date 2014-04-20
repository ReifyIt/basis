//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis.util._
import scala.reflect.macros._

final class GeneralSeqOps[+A](val __ : Seq[A]) extends AnyVal {
  def choose[B](q: PartialFunction[A, B]): Maybe[B]    = macro GeneralSeqMacros.choose[A, B]
  def count(p: A => Boolean): Int                      = macro GeneralSeqMacros.count[A]
  def exists(p: A => Boolean): Boolean                 = macro GeneralSeqMacros.exists[A]
  def find(p: A => Boolean): Maybe[A]                  = macro GeneralSeqMacros.find[A]
  def fold[B >: A](z: B)(op: (B, B) => B): B           = macro GeneralSeqMacros.foldLeft[A, B]
  def foldLeft[B](z: B)(op: (B, A) => B): B            = macro GeneralSeqMacros.foldLeft[A, B]
  def forall(p: A => Boolean): Boolean                 = macro GeneralSeqMacros.forall[A]
  def foreach[U](f: A => U): Unit                      = macro GeneralSeqMacros.foreach[A, U]
  def mayReduce[B >: A](op: (B, B) => B): Maybe[B]     = macro GeneralSeqMacros.mayReduceLeft[A, B]
  def mayReduceLeft[B >: A](op: (B, A) => B): Maybe[B] = macro GeneralSeqMacros.mayReduceLeft[A, B]
  def reduce[B >: A](op: (B, B) => B): B               = macro GeneralSeqMacros.reduceLeft[A, B]
  def reduceLeft[B >: A](op: (B, A) => B): B           = macro GeneralSeqMacros.reduceLeft[A, B]

  def eagerly: StrictSeqOps[A, Seq[_]]                 = macro GeneralSeqMacros.eagerly[A]
  def lazily: NonStrictSeqOps[A]                       = macro GeneralSeqMacros.lazily[A]
}

private[sequential] class GeneralSeqMacros(override val c: blackbox.Context { type PrefixType <: GeneralSeqOps[_] }) extends IteratorMacros(c) {
  import c.{ Expr, prefix, WeakTypeTag }
  import c.universe._

  override def these: Expr[Iterator[_]] = Expr[Iterator[Any]](q"$prefix.__.iterator")
  private def self: Expr[Seq[_]] = Expr[Seq[Any]](q"$prefix.__")

  def eagerly[A : WeakTypeTag]: Expr[StrictSeqOps[A, Seq[_]]] = StrictOps1[StrictSeqOps, A](self)
  def lazily[A : WeakTypeTag]: Expr[NonStrictSeqOps[A]]       = NonStrictOps1[NonStrictSeqOps, A](self)
}
