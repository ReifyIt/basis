//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis.util._

final class GeneralMapOps[+A, +T](val __ : Map[A, T]) extends AnyVal {
  def foreach[U](f: ((A, T)) => U): Unit =
    macro GeneralContainerOps.foreach[(A, T), U]

  def fold[B >: (A, T)](z: B)(op: (B, B) => B): B =
    macro GeneralContainerOps.foldLeft[(A, T), B]

  def reduce[B >: (A, T)](op: (B, B) => B): B =
    macro GeneralContainerOps.reduceLeft[(A, T), B]

  def mayReduce[B >: (A, T)](op: (B, B) => B): Maybe[B] =
    macro GeneralContainerOps.mayReduceLeft[(A, T), B]

  def foldLeft[B](z: B)(op: (B, (A, T)) => B): B =
    macro GeneralContainerOps.foldLeft[(A, T), B]

  def reduceLeft[B >: (A, T)](op: (B, (A, T)) => B): B =
    macro GeneralContainerOps.reduceLeft[(A, T), B]

  def mayReduceLeft[B >: (A, T)](op: (B, (A, T)) => B): Maybe[B] =
    macro GeneralContainerOps.mayReduceLeft[(A, T), B]

  def find(p: ((A, T)) => Boolean): Maybe[(A, T)] =
    macro GeneralContainerOps.find[(A, T)]

  def forall(p: ((A, T)) => Boolean): Boolean =
    macro GeneralContainerOps.forall[(A, T)]

  def exists(p: ((A, T)) => Boolean): Boolean =
    macro GeneralContainerOps.exists[(A, T)]

  def count(p: ((A, T)) => Boolean): Int =
    macro GeneralContainerOps.count[(A, T)]

  def choose[B](q: PartialFunction[(A, T), B]): Maybe[B] =
    macro GeneralContainerOps.choose[(A, T), B]

  def eagerly: StrictMapOps[A, T, Map[_, _]] =
    macro GeneralMapOps.eagerly[A, T]

  def lazily: NonStrictMapOps[A, T] =
    macro GeneralMapOps.lazily[A, T]
}

private[sequential] object GeneralMapOps {
  import scala.collection.immutable.{ ::, Nil }
  import scala.reflect.macros.Context

  private def unApply[A : c.WeakTypeTag, T : c.WeakTypeTag](c: Context): c.Expr[Map[A, T]] = {
    import c.{ Expr, mirror, prefix, typeCheck, weakTypeOf, WeakTypeTag }
    import c.universe._
    val Apply(_, these :: Nil) = prefix.tree
    implicit val MapATTag =
      WeakTypeTag[Map[A, T]](
        appliedType(
          mirror.staticClass("basis.collections.Map").toType,
          weakTypeOf[A] :: weakTypeOf[T] :: Nil))
    Expr[Map[A, T]](typeCheck(these, weakTypeOf[Map[A, T]]))
  }

  def eagerly[A : c.WeakTypeTag, T : c.WeakTypeTag](c: Context): c.Expr[StrictMapOps[A, T, Map[_, _]]] =
    Strict.MapToStrictOps[A, T](c)(unApply[A, T](c))

  def lazily[A : c.WeakTypeTag, T : c.WeakTypeTag](c: Context): c.Expr[NonStrictMapOps[A, T]] =
    NonStrict.MapToNonStrictOps[A, T](c)(unApply[A, T](c))
}
