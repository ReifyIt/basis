//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis._
import scala.reflect.macros._

final class StrictSeqOps[+A, -Family](val __ : Seq[A]) extends AnyVal {
  def collect[B](q: PartialFunction[A, B])(implicit builder: Builder[B] with From[Family]): builder.State = macro StrictSeqMacros.collect[A, B]
  def drop(lower: Int)(implicit builder: Builder[A] with From[Family]): builder.State                     = macro StrictSeqMacros.drop[A]
  def dropWhile(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State           = macro StrictSeqMacros.dropWhile[A]
  def filter(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State              = macro StrictSeqMacros.filter[A]
  def flatMap[B](f: A => Traverser[B])(implicit builder: Builder[B] with From[Family]): builder.State     = macro StrictSeqMacros.flatMap[A, B]
  def map[B](f: A => B)(implicit builder: Builder[B] with From[Family]): builder.State                    = macro StrictSeqMacros.map[A, B]
  def slice(lower: Int, upper: Int)(implicit builder: Builder[A] with From[Family]): builder.State        = macro StrictSeqMacros.slice[A]
  def take(upper: Int)(implicit builder: Builder[A] with From[Family]): builder.State                     = macro StrictSeqMacros.take[A]
  def takeWhile(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State           = macro StrictSeqMacros.takeWhile[A]
  def withFilter(p: A => Boolean): Seq[A]                                                                 = new NonStrictSeqOps.Filter(__, p)
  def zip[B](those: Container[B])(implicit builder: Builder[(A, B)] with From[Family]): builder.State     = macro StrictSeqMacros.zipContainer[A, B]

  def span(p: A => Boolean)(implicit builder1: Builder[A] with From[Family], builder2: Builder[A] with From[Family]): (builder1.State, builder2.State) = macro StrictSeqMacros.span[A]

  def ++ [B >: A](those: Seq[B])(implicit builder: Builder[B] with From[Family]): builder.State = macro StrictSeqMacros.++[B]
  def +: [B >: A](elem: B)(implicit builder: Builder[B] with From[Family]): builder.State       = macro StrictSeqMacros.+:[B]
  def :+ [B >: A](elem: B)(implicit builder: Builder[B] with From[Family]): builder.State       = macro StrictSeqMacros.:+[B]
}

private[sequential] class StrictSeqMacros(override val c: blackbox.Context { type PrefixType <: StrictSeqOps[_, _] }) extends IteratorMacros(c) {
  import c.{ Expr, prefix }
  import c.universe._

  override def these: Expr[Iterator[_]] = Expr[Iterator[Any]](q"$prefix.__.iterator")
}
