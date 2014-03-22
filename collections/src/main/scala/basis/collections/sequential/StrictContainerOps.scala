//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import scala.reflect.macros._

final class StrictContainerOps[+A, -Family](val __ : Container[A]) extends AnyVal {
  def collect[B](q: PartialFunction[A, B])(implicit builder: Builder[B] with From[Family]): builder.State = macro StrictContainerMacros.collect[A, B]
  def drop(lower: Int)(implicit builder: Builder[A] with From[Family]): builder.State                     = macro StrictContainerMacros.drop[A]
  def dropWhile(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State           = macro StrictContainerMacros.dropWhile[A]
  def filter(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State              = macro StrictContainerMacros.filter[A]
  def flatMap[B](f: A => Traverser[B])(implicit builder: Builder[B] with From[Family]): builder.State     = macro StrictContainerMacros.flatMap[A, B]
  def map[B](f: A => B)(implicit builder: Builder[B] with From[Family]): builder.State                    = macro StrictContainerMacros.map[A, B]
  def slice(lower: Int, upper: Int)(implicit builder: Builder[A] with From[Family]): builder.State        = macro StrictContainerMacros.slice[A]
  def take(upper: Int)(implicit builder: Builder[A] with From[Family]): builder.State                     = macro StrictContainerMacros.take[A]
  def takeWhile(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State           = macro StrictContainerMacros.takeWhile[A]
  def withFilter(p: A => Boolean): Container[A]                                                           = new NonStrictContainerOps.Filter(__, p)
  def zip[B](those: Container[B])(implicit builder: Builder[(A, B)] with From[Family]): builder.State     = macro StrictContainerMacros.zipContainer[A, B]

  def span(p: A => Boolean)(implicit builder1: Builder[A] with From[Family], builder2: Builder[A] with From[Family]): (builder1.State, builder2.State) = macro StrictContainerMacros.span[A]

  def ++ [B >: A](those: Container[B])(implicit builder: Builder[B] with From[Family]): builder.State = macro StrictContainerMacros.++[B]
}

private[sequential] class StrictContainerMacros(override val c: blackbox.Context { type PrefixType <: StrictContainerOps[_, _] }) extends IteratorMacros(c) {
  import c.{ Expr, prefix }
  import c.universe._

  override def these: Expr[Iterator[_]] = Expr[Iterator[Any]](q"$prefix.__.iterator")
}
