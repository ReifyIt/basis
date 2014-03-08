//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis.util._

final class GeneralMapOps[+A, +T](val __ : Map[A, T]) extends AnyVal {
  def choose[B](q: PartialFunction[(A, T), B]): Maybe[B]         = macro GeneralContainerMacros.choose[(A, T), B]
  def count(p: ((A, T)) => Boolean): Int                         = macro GeneralContainerMacros.count[(A, T)]
  def exists(p: ((A, T)) => Boolean): Boolean                    = macro GeneralContainerMacros.exists[(A, T)]
  def find(p: ((A, T)) => Boolean): Maybe[(A, T)]                = macro GeneralContainerMacros.find[(A, T)]
  def foldLeft[B](z: B)(op: (B, (A, T)) => B): B                 = macro GeneralContainerMacros.foldLeft[(A, T), B]
  def fold[B >: (A, T)](z: B)(op: (B, B) => B): B                = macro GeneralContainerMacros.foldLeft[(A, T), B]
  def forall(p: ((A, T)) => Boolean): Boolean                    = macro GeneralContainerMacros.forall[(A, T)]
  def foreach[U](f: ((A, T)) => U): Unit                         = macro GeneralContainerMacros.foreach[(A, T), U]
  def mayReduceLeft[B >: (A, T)](op: (B, (A, T)) => B): Maybe[B] = macro GeneralContainerMacros.mayReduceLeft[(A, T), B]
  def mayReduce[B >: (A, T)](op: (B, B) => B): Maybe[B]          = macro GeneralContainerMacros.mayReduceLeft[(A, T), B]
  def reduceLeft[B >: (A, T)](op: (B, (A, T)) => B): B           = macro GeneralContainerMacros.reduceLeft[(A, T), B]
  def reduce[B >: (A, T)](op: (B, B) => B): B                    = macro GeneralContainerMacros.reduceLeft[(A, T), B]
  def eagerly: StrictMapOps[A, T, Map[_, _]]                     = macro GeneralMapMacros.eagerly[A, T]
  def lazily: NonStrictMapOps[A, T]                              = macro GeneralMapMacros.lazily[A, T]
}

private[sequential] object GeneralMapMacros {
  import scala.collection.immutable.{ ::, Nil }
  import scala.reflect.macros.Context

  private def unApply[A : c.WeakTypeTag, T : c.WeakTypeTag](c: Context): c.Expr[Map[A, T]] = {
    import c.universe._
    val Apply(_, these :: Nil) = c.prefix
    General.typed[Map[A, T]](c)(these)
  }

  def eagerly[A : c.WeakTypeTag, T : c.WeakTypeTag](c: Context): c.Expr[StrictMapOps[A, T, Map[_, _]]] =
    Strict.MapToStrictOps[A, T](c)(unApply[A, T](c))

  def lazily[A : c.WeakTypeTag, T : c.WeakTypeTag](c: Context): c.Expr[NonStrictMapOps[A, T]] =
    NonStrict.MapToNonStrictOps[A, T](c)(unApply[A, T](c))
}
