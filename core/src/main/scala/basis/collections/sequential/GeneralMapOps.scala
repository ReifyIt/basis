//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis._
import scala.reflect.macros._

final class GeneralMapOps[+A, +T](val __ : Map[A, T]) extends AnyVal {
  def choose[B](q: PartialFunction[(A, T), B]): Maybe[B]         = macro GeneralMapMacros.choose[(A, T), B]
  def count(p: ((A, T)) => Boolean): Int                         = macro GeneralMapMacros.count[(A, T)]
  def exists(p: ((A, T)) => Boolean): Boolean                    = macro GeneralMapMacros.exists[(A, T)]
  def find(p: ((A, T)) => Boolean): Maybe[(A, T)]                = macro GeneralMapMacros.find[(A, T)]
  def fold[B >: (A, T)](z: B)(op: (B, B) => B): B                = macro GeneralMapMacros.foldLeft[(A, T), B]
  def foldLeft[B](z: B)(op: (B, (A, T)) => B): B                 = macro GeneralMapMacros.foldLeft[(A, T), B]
  def forall(p: ((A, T)) => Boolean): Boolean                    = macro GeneralMapMacros.forall[(A, T)]
  def foreach[U](f: ((A, T)) => U): Unit                         = macro GeneralMapMacros.foreach[(A, T), U]
  def mayReduce[B >: (A, T)](op: (B, B) => B): Maybe[B]          = macro GeneralMapMacros.mayReduceLeft[(A, T), B]
  def mayReduceLeft[B >: (A, T)](op: (B, (A, T)) => B): Maybe[B] = macro GeneralMapMacros.mayReduceLeft[(A, T), B]
  def reduce[B >: (A, T)](op: (B, B) => B): B                    = macro GeneralMapMacros.reduceLeft[(A, T), B]
  def reduceLeft[B >: (A, T)](op: (B, (A, T)) => B): B           = macro GeneralMapMacros.reduceLeft[(A, T), B]

  def eagerly: StrictMapOps[A, T, Map[_, _]]                     = macro GeneralMapMacros.eagerly[A, T]
  def lazily: NonStrictMapOps[A, T]                              = macro GeneralMapMacros.lazily[A, T]
}

private[sequential] class GeneralMapMacros(override val c: blackbox.Context { type PrefixType <: GeneralMapOps[_, _] }) extends IteratorMacros(c) {
  import c.{ Expr, prefix, WeakTypeTag }
  import c.universe._

  override def these: Expr[Iterator[_]] = Expr[Iterator[Any]](q"$prefix.__.iterator")
  private def self: Expr[Map[_, _]] = Expr[Map[Any, Any]](q"$prefix.__")

  def eagerly[A : WeakTypeTag, T : WeakTypeTag]: Expr[StrictMapOps[A, T, Map[_, _]]] = StrictOps2[StrictMapOps, A, T](self)
  def lazily[A : WeakTypeTag, T : WeakTypeTag]: Expr[NonStrictMapOps[A, T]]          = NonStrictOps2[NonStrictMapOps, A, T](self)
}
