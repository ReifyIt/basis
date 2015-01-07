//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis._
import scala.reflect.macros._

final class StrictLinearSeqOps[+A, -Family](val __ : LinearSeq[A]) extends AnyVal {
  def collect[B](q: PartialFunction[A, B])(implicit builder: Builder[B] with From[Family]): builder.State = macro StrictLinearSeqMacros.collect[A, B]
  def drop(lower: Int)(implicit builder: Builder[A] with From[Family]): builder.State                     = macro StrictLinearSeqMacros.drop[A]
  def dropWhile(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State           = macro StrictLinearSeqMacros.dropWhile[A]
  def filter(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State              = macro StrictLinearSeqMacros.filter[A]
  def flatMap[B](f: A => Traverser[B])(implicit builder: Builder[B] with From[Family]): builder.State     = macro StrictLinearSeqMacros.flatMap[A, B]
  def map[B](f: A => B)(implicit builder: Builder[B] with From[Family]): builder.State                    = macro StrictLinearSeqMacros.map[A, B]
  def slice(lower: Int, upper: Int)(implicit builder: Builder[A] with From[Family]): builder.State        = macro StrictLinearSeqMacros.slice[A]
  def take(upper: Int)(implicit builder: Builder[A] with From[Family]): builder.State                     = macro StrictLinearSeqMacros.take[A]
  def takeWhile(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State           = macro StrictLinearSeqMacros.takeWhile[A]
  def withFilter(p: A => Boolean): LinearSeq[A]                                                           = new NonStrictLinearSeqOps.Filter(__, p)
  def zip[B](those: LinearSeq[B])(implicit builder: Builder[(A, B)] with From[Family]): builder.State     = macro StrictLinearSeqMacros.zip[A, B]

  def span(p: A => Boolean)(implicit builder1: Builder[A] with From[Family], builder2: Builder[A] with From[Family]): (builder1.State, builder2.State) = macro StrictLinearSeqMacros.span[A]

  def ++ [B >: A](those: LinearSeq[B])(implicit builder: Builder[B] with From[Family]): builder.State = macro StrictLinearSeqMacros.++[B]
  def +: [B >: A](elem: B)(implicit builder: Builder[B] with From[Family]): builder.State             = macro StrictLinearSeqMacros.+:[B]
  def :+ [B >: A](elem: B)(implicit builder: Builder[B] with From[Family]): builder.State             = macro StrictLinearSeqMacros.:+[B]
}

private[sequential] class StrictLinearSeqMacros(override val c: blackbox.Context { type PrefixType <: StrictLinearSeqOps[_, _] }) extends LinearSeqMacros(c) {
  import c.{ Expr, prefix }
  import c.universe._

  override def these: Expr[LinearSeq[_]] = Expr[LinearSeq[Any]](q"$prefix.__")
}
