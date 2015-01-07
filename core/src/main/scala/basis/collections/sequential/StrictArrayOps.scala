//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis._
import scala.reflect.macros._

final class StrictArrayOps[A, -Family](val __ : Array[A]) extends AnyVal {
  def collect[B](q: PartialFunction[A, B])(implicit builder: Builder[B] with From[Family]): builder.State = macro StrictArrayMacros.collect[A, B]
  def map[B](f: A => B)(implicit builder: Builder[B] with From[Family]): builder.State                    = macro StrictArrayMacros.map[A, B]
  def flatMap[B](f: A => Traverser[B])(implicit builder: Builder[B] with From[Family]): builder.State     = macro StrictArrayMacros.flatMap[A, B]
  def filter(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State              = macro StrictArrayMacros.filter[A]
  def withFilter(p: A => Boolean): IndexedSeq[A]                                                          = new NonStrictArrayOps.Filter(__, p)
  def dropWhile(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State           = macro StrictArrayMacros.dropWhile[A]
  def takeWhile(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State           = macro StrictArrayMacros.takeWhile[A]
  def drop(lower: Int)(implicit builder: Builder[A] with From[Family]): builder.State                     = macro StrictArrayMacros.drop[A]
  def take(upper: Int)(implicit builder: Builder[A] with From[Family]): builder.State                     = macro StrictArrayMacros.take[A]
  def slice(lower: Int, upper: Int)(implicit builder: Builder[A] with From[Family]): builder.State        = macro StrictArrayMacros.slice[A]
  def reverse(implicit builder: Builder[A] with From[Family]): builder.State                              = macro StrictArrayMacros.reverse[A]
  def zip[B](those: Array[B])(implicit builder: Builder[(A, B)] with From[Family]): builder.State         = macro StrictArrayMacros.zip[A, B]

  def span(p: A => Boolean)(implicit builder1: Builder[A] with From[Family], builder2: Builder[A] with From[Family]): (builder1.State, builder2.State) = macro StrictArrayMacros.span[A]

  def ++ (those: Array[A])(implicit builder: ArrayBuilder[A] with From[Family]): builder.State = macro StrictArrayMacros.++[A]
  def +: (elem: A)(implicit builder: ArrayBuilder[A] with From[Family]): builder.State         = macro StrictArrayMacros.+:[A]
  def :+ (elem: A)(implicit builder: ArrayBuilder[A] with From[Family]): builder.State         = macro StrictArrayMacros.:+[A]
}

private[sequential] class StrictArrayMacros(override val c: blackbox.Context { type PrefixType <: StrictArrayOps[_, _] }) extends ArrayMacros(c) {
  import c.{ Expr, prefix }
  import c.universe._

  override def these: Expr[Array[_]] = Expr[Array[Any]](q"$prefix.__")
}
