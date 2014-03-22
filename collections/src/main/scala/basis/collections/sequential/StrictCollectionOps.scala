//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

final class StrictCollectionOps[+A, -Family](val __ : Collection[A]) extends AnyVal {
  def collect[B](q: PartialFunction[A, B])(implicit builder: Builder[B] with From[Family]): builder.State = new StrictTraverserOps[A, Family](__).collect[B](q)(builder)
  def drop(lower: Int)(implicit builder: Builder[A] with From[Family]): builder.State                     = new StrictTraverserOps[A, Family](__).drop(lower)(builder)
  def dropWhile(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State           = new StrictTraverserOps[A, Family](__).dropWhile(p)(builder)
  def filter(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State              = new StrictTraverserOps[A, Family](__).filter(p)(builder)
  def flatMap[B](f: A => Traverser[B])(implicit builder: Builder[B] with From[Family]): builder.State     = new StrictTraverserOps[A, Family](__).flatMap[B](f)(builder)
  def map[B](f: A => B)(implicit builder: Builder[B] with From[Family]): builder.State                    = new StrictTraverserOps[A, Family](__).map[B](f)(builder)
  def slice(lower: Int, upper: Int)(implicit builder: Builder[A] with From[Family]): builder.State        = new StrictTraverserOps[A, Family](__).slice(lower, upper)(builder)
  def take(upper: Int)(implicit builder: Builder[A] with From[Family]): builder.State                     = new StrictTraverserOps[A, Family](__).take(upper)(builder)
  def takeWhile(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State           = new StrictTraverserOps[A, Family](__).takeWhile(p)(builder)
  def withFilter(p: A => Boolean): Collection[A]                                                          = new NonStrictCollectionOps.Filter(__, p)

  def span(p: A => Boolean)(implicit builder1: Builder[A] with From[Family], builder2: Builder[A] with From[Family]): (builder1.State, builder2.State) = new StrictTraverserOps[A, Family](__).span(p)(builder1, builder2)

  def ++ [B >: A](those: Collection[B])(implicit builder: Builder[B] with From[Family]): builder.State = new StrictTraverserOps[A, Family](__).++[B](those)(builder)
}
