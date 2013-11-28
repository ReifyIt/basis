//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

final class NonStrictCollectionOps[+A](val __ : Collection[A]) extends AnyVal {
  def collect[B](q: PartialFunction[A, B]): Collection[B] =
    new NonStrictCollectionOps.Collect(__, q)

  def map[B](f: A => B): Collection[B] =
    new NonStrictCollectionOps.Map(__, f)

  def flatMap[B](f: A => Collection[B]): Collection[B] =
    new NonStrictCollectionOps.FlatMap(__, f)

  def filter(p: A => Boolean): Collection[A] =
    new NonStrictCollectionOps.Filter(__, p)

  def withFilter(p: A => Boolean): Collection[A] =
    new NonStrictCollectionOps.Filter(__, p)

  def dropWhile(p: A => Boolean): Collection[A] =
    new NonStrictCollectionOps.DropWhile(__, p)

  def takeWhile(p: A => Boolean): Collection[A] =
    new NonStrictCollectionOps.TakeWhile(__, p)

  def span(p: A => Boolean): (Collection[A], Collection[A]) =
    (takeWhile(p), dropWhile(p))

  def drop(lower: Int): Collection[A] =
    new NonStrictCollectionOps.Drop(__, lower)

  def take(upper: Int): Collection[A] =
    new NonStrictCollectionOps.Take(__, upper)

  def slice(lower: Int, upper: Int): Collection[A] =
    new NonStrictCollectionOps.Slice(__, lower, upper)

  def ++ [B >: A](those: Collection[B]): Collection[B] =
    new NonStrictCollectionOps.++(__, those)
}

private[sequential] object NonStrictCollectionOps {
  class Collect[-A, +B](these: Collection[A], q: PartialFunction[A, B])
    extends NonStrictTraverserOps.Collect[A, B](these, q) with Collection[B]

  class Map[-A, +B](these: Collection[A], f: A => B)
    extends NonStrictTraverserOps.Map[A, B](these, f) with Collection[B]

  class FlatMap[-A, +B](these: Collection[A], f: A => Collection[B])
    extends NonStrictTraverserOps.FlatMap[A, B](these, f) with Collection[B]

  class Filter[+A](these: Collection[A], p: A => Boolean)
    extends NonStrictTraverserOps.Filter[A](these, p) with Collection[A]

  class DropWhile[+A](these: Collection[A], p: A => Boolean)
    extends NonStrictTraverserOps.DropWhile[A](these, p) with Collection[A]

  class TakeWhile[+A](these: Collection[A], p: A => Boolean)
    extends NonStrictTraverserOps.TakeWhile[A](these, p) with Collection[A]

  class Drop[+A](these: Collection[A], lower: Int)
    extends NonStrictTraverserOps.Drop[A](these, lower) with Collection[A]

  class Take[+A](these: Collection[A], upper: Int)
    extends NonStrictTraverserOps.Take[A](these, upper) with Collection[A]

  class Slice[+A](these: Collection[A], lower: Int, upper: Int)
    extends NonStrictTraverserOps.Slice[A](these, lower, upper) with Collection[A]

  class ++[+A](these: Collection[A], those: Collection[A])
    extends NonStrictTraverserOps.++[A](these, those) with Collection[A]
}
