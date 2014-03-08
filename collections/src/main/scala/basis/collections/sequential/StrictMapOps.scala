//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

final class StrictMapOps[+A, +T, -Family](val __ : Map[A, T]) extends AnyVal {
  def collect[B](q: PartialFunction[(A, T), B])(implicit builder: Builder[B] with From[Family]): builder.State = macro StrictContainerMacros.collect[(A, T), B]
  def map[B](f: ((A, T)) => B)(implicit builder: Builder[B] with From[Family]): builder.State                  = macro StrictContainerMacros.map[(A, T), B]
  def flatMap[B](f: ((A, T)) => Traverser[B])(implicit builder: Builder[B] with From[Family]): builder.State   = macro StrictContainerMacros.flatMap[(A, T), B]
  def filter(p: ((A, T)) => Boolean)(implicit builder: Builder[(A, T)] with From[Family]): builder.State       = macro StrictContainerMacros.filter[(A, T)]
  def withFilter(p: ((A, T)) => Boolean): Map[A, T]                                                            = new NonStrictMapOps.Filter(__, p)
  def dropWhile(p: ((A, T)) => Boolean)(implicit builder: Builder[(A, T)] with From[Family]): builder.State    = macro StrictContainerMacros.dropWhile[(A, T)]
  def takeWhile(p: ((A, T)) => Boolean)(implicit builder: Builder[(A, T)] with From[Family]): builder.State    = macro StrictContainerMacros.takeWhile[(A, T)]
  def drop(lower: Int)(implicit builder: Builder[(A, T)] with From[Family]): builder.State                     = macro StrictContainerMacros.drop[(A, T)]
  def take(upper: Int)(implicit builder: Builder[(A, T)] with From[Family]): builder.State                     = macro StrictContainerMacros.take[(A, T)]
  def slice(lower: Int, upper: Int)(implicit builder: Builder[(A, T)] with From[Family]): builder.State        = macro StrictContainerMacros.slice[(A, T)]
  def ++ [B >: A, U >: T](those: Map[B, U])(implicit builder: Builder[B] with From[Family]): builder.State     = macro StrictTraverserMacros.++[(B, U)]

  def span(p: ((A, T)) => Boolean)(implicit builder1: Builder[(A, T)] with From[Family], builder2: Builder[(A, T)] with From[Family]): (builder1.State, builder2.State) = macro StrictContainerMacros.span[(A, T)]
}
