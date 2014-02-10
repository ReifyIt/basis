//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

final class NonStrictSeqOps[+A](val __ : Seq[A]) extends AnyVal {
  def collect[B](q: PartialFunction[A, B]): Seq[B] =
    new NonStrictSeqOps.Collect(__, q)

  def map[B](f: A => B): Seq[B] =
    new NonStrictSeqOps.Map(__, f)

  def flatMap[B](f: A => Seq[B]): Seq[B] =
    new NonStrictSeqOps.FlatMap(__, f)

  def filter(p: A => Boolean): Seq[A] =
    new NonStrictSeqOps.Filter(__, p)

  def withFilter(p: A => Boolean): Seq[A] =
    new NonStrictSeqOps.Filter(__, p)

  def dropWhile(p: A => Boolean): Seq[A] =
    new NonStrictSeqOps.DropWhile(__, p)

  def takeWhile(p: A => Boolean): Seq[A] =
    new NonStrictSeqOps.TakeWhile(__, p)

  def span(p: A => Boolean): (Seq[A], Seq[A]) =
    (takeWhile(p), dropWhile(p))

  def drop(lower: Int): Seq[A] =
    new NonStrictSeqOps.Drop(__, lower)

  def take(upper: Int): Seq[A] =
    new NonStrictSeqOps.Take(__, upper)

  def slice(lower: Int, upper: Int): Seq[A] =
    new NonStrictSeqOps.Slice(__, lower, upper)

  def zip[B](those: Seq[B]): Seq[(A, B)] =
    new NonStrictSeqOps.Zip(__, those)

  def ++ [B >: A](those: Seq[B]): Seq[B] =
    new NonStrictSeqOps.++(__, those)
}

private[sequential] object NonStrictSeqOps {
  class Collect[-A, +B](
      protected[this] override val these: Seq[A],
      protected[this] override val q: PartialFunction[A, B])
    extends NonStrictContainerOps.Collect[A, B](these, q) with Seq[B]

  class Map[-A, +B](
      protected[this] override val these: Seq[A],
      protected[this] override val f: A => B)
    extends NonStrictContainerOps.Map[A, B](these, f) with Seq[B]

  class FlatMap[-A, +B](
      protected[this] override val these: Seq[A],
      protected[this] override val f: A => Seq[B])
    extends NonStrictContainerOps.FlatMap[A, B](these, f) with Seq[B]

  class Filter[+A](
      protected[this] override val these: Seq[A],
      protected[this] override val p: A => Boolean)
    extends NonStrictContainerOps.Filter[A](these, p) with Seq[A]

  class DropWhile[+A](
      protected[this] override val these: Seq[A],
      protected[this] override val p: A => Boolean)
    extends NonStrictContainerOps.DropWhile[A](these, p) with Seq[A]

  class TakeWhile[+A](
      protected[this] override val these: Seq[A],
      protected[this] override val p: A => Boolean)
    extends NonStrictContainerOps.TakeWhile[A](these, p) with Seq[A]

  class Drop[+A](
      protected[this] override val these: Seq[A],
      protected[this] override val lower: Int)
    extends NonStrictContainerOps.Drop[A](these, lower) with Seq[A]

  class Take[+A](
      protected[this] override val these: Seq[A],
      protected[this] override val upper: Int)
    extends NonStrictContainerOps.Take[A](these, upper) with Seq[A]

  class Slice[+A](
      protected[this] override val these: Seq[A],
      protected[this] override val lower: Int,
      protected[this] override val upper: Int)
    extends NonStrictContainerOps.Slice[A](these, lower, upper) with Seq[A]

  class Zip[+A, +B](
      protected[this] override val these: Seq[A],
      protected[this] override val those: Seq[B])
    extends NonStrictContainerOps.Zip[A, B](these, those) with Seq[(A, B)]

  class ++[+A](
      protected[this] override val these: Seq[A],
      protected[this] override val those: Seq[A])
    extends NonStrictContainerOps.++[A](these, those) with Seq[A]
}
