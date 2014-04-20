//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis.util._
import scala.reflect.macros._

final class GeneralIndexedSeqOps[+A](val __ : IndexedSeq[A]) extends AnyVal {
  def choose[B](q: PartialFunction[A, B]): Maybe[B]     = macro GeneralIndexedSeqMacros.choose[A, B]
  def count(p: A => Boolean): Int                       = macro GeneralIndexedSeqMacros.count[A]
  def exists(p: A => Boolean): Boolean                  = macro GeneralIndexedSeqMacros.exists[A]
  def find(p: A => Boolean): Maybe[A]                   = macro GeneralIndexedSeqMacros.find[A]
  def fold[B >: A](z: B)(op: (B, B) => B): B            = macro GeneralIndexedSeqMacros.foldLeft[A, B]
  def foldLeft[B](z: B)(op: (B, A) => B): B             = macro GeneralIndexedSeqMacros.foldLeft[A, B]
  def foldRight[B](z: B)(op: (A, B) => B): B            = macro GeneralIndexedSeqMacros.foldRight[A, B]
  def forall(p: A => Boolean): Boolean                  = macro GeneralIndexedSeqMacros.forall[A]
  def foreach[U](f: A => U): Unit                       = macro GeneralIndexedSeqMacros.foreach[A, U]
  def mayReduce[B >: A](op: (B, B) => B): Maybe[B]      = macro GeneralIndexedSeqMacros.mayReduceLeft[A, B]
  def mayReduceLeft[B >: A](op: (B, A) => B): Maybe[B]  = macro GeneralIndexedSeqMacros.mayReduceLeft[A, B]
  def mayReduceRight[B >: A](op: (A, B) => B): Maybe[B] = macro GeneralIndexedSeqMacros.mayReduceRight[A, B]
  def reduce[B >: A](op: (B, B) => B): B                = macro GeneralIndexedSeqMacros.reduceLeft[A, B]
  def reduceLeft[B >: A](op: (B, A) => B): B            = macro GeneralIndexedSeqMacros.reduceLeft[A, B]
  def reduceRight[B >: A](op: (A, B) => B): B           = macro GeneralIndexedSeqMacros.reduceRight[A, B]

  def eagerly: StrictIndexedSeqOps[A, IndexedSeq[_]]    = macro GeneralIndexedSeqMacros.eagerly[A]
  def lazily: NonStrictIndexedSeqOps[A]                 = macro GeneralIndexedSeqMacros.lazily[A]
}

private[sequential] class GeneralIndexedSeqMacros(override val c: blackbox.Context { type PrefixType <: GeneralIndexedSeqOps[_] }) extends IndexedSeqMacros(c) {
  import c.{ Expr, prefix, WeakTypeTag }
  import c.universe._

  override def these: Expr[IndexedSeq[_]] = Expr[IndexedSeq[Any]](q"$prefix.__")

  def eagerly[A : WeakTypeTag]: Expr[StrictIndexedSeqOps[A, IndexedSeq[_]]] = StrictOps1[StrictIndexedSeqOps, A](these)
  def lazily[A : WeakTypeTag]: Expr[NonStrictIndexedSeqOps[A]]              = NonStrictOps1[NonStrictIndexedSeqOps, A](these)
}
