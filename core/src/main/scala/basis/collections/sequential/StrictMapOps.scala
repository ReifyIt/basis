//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis._
import scala.reflect.macros._

final class StrictMapOps[+A, +T, -Family](val __ : Map[A, T]) extends AnyVal {
  def collect[B](q: PartialFunction[(A, T), B])(implicit builder: Builder[B] with From[Family]): builder.State = macro StrictMapMacros.collect[(A, T), B]
  def drop(lower: Int)(implicit builder: Builder[(A, T)] with From[Family]): builder.State                     = macro StrictMapMacros.drop[(A, T)]
  def dropWhile(p: ((A, T)) => Boolean)(implicit builder: Builder[(A, T)] with From[Family]): builder.State    = macro StrictMapMacros.dropWhile[(A, T)]
  def filter(p: ((A, T)) => Boolean)(implicit builder: Builder[(A, T)] with From[Family]): builder.State       = macro StrictMapMacros.filter[(A, T)]
  def flatMap[B](f: ((A, T)) => Traverser[B])(implicit builder: Builder[B] with From[Family]): builder.State   = macro StrictMapMacros.flatMap[(A, T), B]
  def map[B](f: ((A, T)) => B)(implicit builder: Builder[B] with From[Family]): builder.State                  = macro StrictMapMacros.map[(A, T), B]
  def slice(lower: Int, upper: Int)(implicit builder: Builder[(A, T)] with From[Family]): builder.State        = macro StrictMapMacros.slice[(A, T)]
  def take(upper: Int)(implicit builder: Builder[(A, T)] with From[Family]): builder.State                     = macro StrictMapMacros.take[(A, T)]
  def takeWhile(p: ((A, T)) => Boolean)(implicit builder: Builder[(A, T)] with From[Family]): builder.State    = macro StrictMapMacros.takeWhile[(A, T)]
  def withFilter(p: ((A, T)) => Boolean): Map[A, T]                                                            = new NonStrictMapOps.Filter(__, p)

  def span(p: ((A, T)) => Boolean)(implicit builder1: Builder[(A, T)] with From[Family], builder2: Builder[(A, T)] with From[Family]): (builder1.State, builder2.State) = macro StrictMapMacros.span[(A, T)]

  def ++ [B >: A, U >: T](those: Map[B, U])(implicit builder: Builder[(B, U)] with From[Family]): builder.State = macro StrictMapMacros.++[(B, U)]
  def + [B >: A, U >: T](elem: (B, U))(implicit builder: Builder[(B, U)] with From[Family]): builder.State     = macro StrictMapMacros.:+[(B, U)]
}

private[sequential] class StrictMapMacros(override val c: blackbox.Context { type PrefixType <: StrictMapOps[_, _, _] }) extends IteratorMacros(c) {
  import c.{ Expr, prefix }
  import c.universe._

  override def these: Expr[Iterator[_]] = Expr[Iterator[Any]](q"$prefix.__.iterator")
}
