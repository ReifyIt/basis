//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis.util._
import scala.reflect.macros._

final class GeneralArrayOps[A](val __ : Array[A]) extends AnyVal {
  @inline def choose[B](q: PartialFunction[A, B]): Maybe[B]     = macro GeneralArrayMacros.choose[A, B]
  @inline def count(p: A => Boolean): Int                       = macro GeneralArrayMacros.count[A]
  @inline def exists(p: A => Boolean): Boolean                  = macro GeneralArrayMacros.exists[A]
  @inline def find(p: A => Boolean): Maybe[A]                   = macro GeneralArrayMacros.find[A]
  @inline def foldLeft[B](z: B)(op: (B, A) => B): B             = macro GeneralArrayMacros.foldLeft[A, B]
  @inline def foldRight[B](z: B)(op: (A, B) => B): B            = macro GeneralArrayMacros.foldRight[A, B]
  @inline def fold[B >: A](z: B)(op: (B, B) => B): B            = macro GeneralArrayMacros.foldLeft[A, B]
  @inline def forall(p: A => Boolean): Boolean                  = macro GeneralArrayMacros.forall[A]
  @inline def foreach[U](f: A => U): Unit                       = macro GeneralArrayMacros.foreach[A, U]
  @inline def isEmpty: Boolean                                  = macro GeneralArrayMacros.isEmpty[A]
  @inline def mayReduceLeft[B >: A](op: (B, A) => B): Maybe[B]  = macro GeneralArrayMacros.mayReduceLeft[A, B]
  @inline def mayReduceRight[B >: A](op: (A, B) => B): Maybe[B] = macro GeneralArrayMacros.mayReduceRight[A, B]
  @inline def mayReduce[B >: A](op: (B, B) => B): Maybe[B]      = macro GeneralArrayMacros.mayReduceLeft[A, B]
  @inline def reduceLeft[B >: A](op: (B, A) => B): B            = macro GeneralArrayMacros.reduceLeft[A, B]
  @inline def reduceRight[B >: A](op: (A, B) => B): B           = macro GeneralArrayMacros.reduceRight[A, B]
  @inline def reduce[B >: A](op: (B, B) => B): B                = macro GeneralArrayMacros.reduceLeft[A, B]

  @inline def eagerly: StrictArrayOps[A, Array[_]]              = macro GeneralArrayMacros.eagerly[A]
  @inline def lazily: NonStrictArrayOps[A]                      = macro GeneralArrayMacros.lazily[A]
}

private[sequential] class GeneralArrayMacros(override val c: blackbox.Context { type PrefixType <: GeneralArrayOps[_] }) extends ArrayMacros(c) {
  import c.{ Expr, prefix }
  import c.universe._

  override def these: Expr[Array[_]] = Expr[Array[Any]](q"$prefix.__")

  def eagerly[A : WeakTypeTag]: Expr[StrictArrayOps[A, Array[_]]] = StrictOpsFamily1[StrictArrayOps, A, Array[_]](these)
  def lazily[A : WeakTypeTag]: Expr[NonStrictArrayOps[A]]         = NonStrictOps1[NonStrictArrayOps, A](these)
}
