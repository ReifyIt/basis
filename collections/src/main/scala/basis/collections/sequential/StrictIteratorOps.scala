//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import scala.reflect.macros._

final class StrictIteratorOps[+A, -Family](val __ : Iterator[A]) extends AnyVal {
  def collect[B](q: PartialFunction[A, B])(implicit builder: Builder[B] with From[Family]): builder.State = macro StrictIteratorMacros.collect[A, B]
  def drop(lower: Int)(implicit builder: Builder[A] with From[Family]): builder.State                     = macro StrictIteratorMacros.drop[A]
  def dropWhile(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State           = macro StrictIteratorMacros.dropWhile[A]
  def filter(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State              = macro StrictIteratorMacros.filter[A]
  def flatMap[B](f: A => Traverser[B])(implicit builder: Builder[B] with From[Family]): builder.State     = macro StrictIteratorMacros.flatMap[A, B]
  def map[B](f: A => B)(implicit builder: Builder[B] with From[Family]): builder.State                    = macro StrictIteratorMacros.map[A, B]
  def slice(lower: Int, upper: Int)(implicit builder: Builder[A] with From[Family]): builder.State        = macro StrictIteratorMacros.slice[A]
  def take(upper: Int)(implicit builder: Builder[A] with From[Family]): builder.State                     = macro StrictIteratorMacros.take[A]
  def takeWhile(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State           = macro StrictIteratorMacros.takeWhile[A]
  def withFilter(p: A => Boolean): Iterator[A]                                                            = new NonStrictIteratorOps.Filter(__, p)
  def zip[B](those: Iterator[B])(implicit builder: Builder[(A, B)] with From[Family]): builder.State      = macro StrictIteratorMacros.zip[A, B]

  def span(p: A => Boolean)(implicit builder1: Builder[A] with From[Family], builder2: Builder[A] with From[Family]): (builder1.State, builder2.State) = macro StrictIteratorMacros.span[A]

  def ++ [B >: A](those: Iterator[B])(implicit builder: Builder[B] with From[Family]): builder.State = macro StrictIteratorMacros.++[B]
}

private[sequential] class StrictIteratorMacros(override val c: blackbox.Context { type PrefixType <: StrictIteratorOps[_, _] }) extends IteratorMacros(c) {
  import c.{ Expr, prefix }
  import c.universe._

  override def these: Expr[Iterator[_]] = Expr[Iterator[Any]](q"$prefix.__")
}
