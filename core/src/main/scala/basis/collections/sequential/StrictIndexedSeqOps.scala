//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis._
import scala.reflect.macros._

final class StrictIndexedSeqOps[+A, -Family](val __ : IndexedSeq[A]) extends AnyVal {
  def collect[B](q: PartialFunction[A, B])(implicit builder: Builder[B] with From[Family]): builder.State = macro StrictIndexedSeqMacros.collect[A, B]
  def drop(lower: Int)(implicit builder: Builder[A] with From[Family]): builder.State                     = macro StrictIndexedSeqMacros.drop[A]
  def dropWhile(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State           = macro StrictIndexedSeqMacros.dropWhile[A]
  def filter(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State              = macro StrictIndexedSeqMacros.filter[A]
  def flatMap[B](f: A => Traverser[B])(implicit builder: Builder[B] with From[Family]): builder.State     = macro StrictIndexedSeqMacros.flatMap[A, B]
  def map[B](f: A => B)(implicit builder: Builder[B] with From[Family]): builder.State                    = macro StrictIndexedSeqMacros.map[A, B]
  def reverse(implicit builder: Builder[A] with From[Family]): builder.State                              = macro StrictIndexedSeqMacros.reverse[A]
  def slice(lower: Int, upper: Int)(implicit builder: Builder[A] with From[Family]): builder.State        = macro StrictIndexedSeqMacros.slice[A]
  def take(upper: Int)(implicit builder: Builder[A] with From[Family]): builder.State                     = macro StrictIndexedSeqMacros.take[A]
  def takeWhile(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State           = macro StrictIndexedSeqMacros.takeWhile[A]
  def withFilter(p: A => Boolean): IndexedSeq[A]                                                          = new NonStrictIndexedSeqOps.Filter(__, p)
  def zip[B](those: IndexedSeq[B])(implicit builder: Builder[(A, B)] with From[Family]): builder.State    = macro StrictIndexedSeqMacros.zip[A, B]

  def span(p: A => Boolean)(implicit builder1: Builder[A] with From[Family], builder2: Builder[A] with From[Family]): (builder1.State, builder2.State) = macro StrictIndexedSeqMacros.span[A]

  def ++ [B >: A](those: IndexedSeq[B])(implicit builder: Builder[B] with From[Family]): builder.State = macro StrictIndexedSeqMacros.++[B]
  def +: [B >: A](elem: B)(implicit builder: Builder[B] with From[Family]): builder.State              = macro StrictIndexedSeqMacros.+:[B]
  def :+ [B >: A](elem: B)(implicit builder: Builder[B] with From[Family]): builder.State              = macro StrictIndexedSeqMacros.:+[B]
}

private[sequential] class StrictIndexedSeqMacros(override val c: blackbox.Context { type PrefixType <: StrictIndexedSeqOps[_, _] }) extends IndexedSeqMacros(c) {
  import c.{ Expr, prefix }
  import c.universe._

  override def these: Expr[IndexedSeq[_]] = Expr[IndexedSeq[Any]](q"$prefix.__")
}
