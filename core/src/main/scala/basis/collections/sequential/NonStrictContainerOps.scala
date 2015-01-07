//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

final class NonStrictContainerOps[+A](val __ : Container[A]) extends AnyVal {
  def ++ [B >: A](those: Container[B]): Container[B]      = new NonStrictContainerOps.++(__, those)
  def collect[B](q: PartialFunction[A, B]): Container[B]  = new NonStrictContainerOps.Collect(__, q)
  def drop(lower: Int): Container[A]                      = new NonStrictContainerOps.Drop(__, lower)
  def dropWhile(p: A => Boolean): Container[A]            = new NonStrictContainerOps.DropWhile(__, p)
  def filter(p: A => Boolean): Container[A]               = new NonStrictContainerOps.Filter(__, p)
  def flatMap[B](f: A => Container[B]): Container[B]      = new NonStrictContainerOps.FlatMap(__, f)
  def map[B](f: A => B): Container[B]                     = new NonStrictContainerOps.Map(__, f)
  def slice(lower: Int, upper: Int): Container[A]         = new NonStrictContainerOps.Slice(__, lower, upper)
  def span(p: A => Boolean): (Container[A], Container[A]) = (takeWhile(p), dropWhile(p))
  def take(upper: Int): Container[A]                      = new NonStrictContainerOps.Take(__, upper)
  def takeWhile(p: A => Boolean): Container[A]            = new NonStrictContainerOps.TakeWhile(__, p)
  def withFilter(p: A => Boolean): Container[A]           = new NonStrictContainerOps.Filter(__, p)
  def zip[B](those: Container[B]): Container[(A, B)]      = new NonStrictContainerOps.Zip(__, those)
}

private[sequential] object NonStrictContainerOps {
  class Collect[-A, +B](
      protected[this] override val these: Container[A],
      protected[this] override val q: PartialFunction[A, B])
    extends NonStrictCollectionOps.Collect[A, B](these, q) with Container[B] {

    override def iterator: Iterator[B] =
      new NonStrictIteratorOps.Collect(these.iterator, q)
  }

  class Map[-A, +B](
      protected[this] override val these: Container[A],
      protected[this] override val f: A => B)
    extends NonStrictCollectionOps.Map[A, B](these, f) with Container[B] {

    override def iterator: Iterator[B] =
      new NonStrictIteratorOps.Map(these.iterator, f)
  }

  class FlatMap[-A, +B](
      protected[this] override val these: Container[A],
      protected[this] override val f: A => Container[B])
    extends NonStrictCollectionOps.FlatMap[A, B](these, f) with Container[B] {

    override def iterator: Iterator[B] =
      new NonStrictIteratorOps.FlatMapContainer(these.iterator, f)
  }

  class Filter[+A](
      protected[this] override val these: Container[A],
      protected[this] override val p: A => Boolean)
    extends NonStrictCollectionOps.Filter[A](these, p) with Container[A] {

    override def iterator: Iterator[A] =
      new NonStrictIteratorOps.Filter(these.iterator, p)
  }

  class DropWhile[+A](
      protected[this] override val these: Container[A],
      protected[this] override val p: A => Boolean)
    extends NonStrictCollectionOps.DropWhile[A](these, p) with Container[A] {

    override def iterator: Iterator[A] =
      new NonStrictIteratorOps.DropWhile(these.iterator, p)
  }

  class TakeWhile[+A](
      protected[this] override val these: Container[A],
      protected[this] override val p: A => Boolean)
    extends NonStrictCollectionOps.TakeWhile[A](these, p) with Container[A] {

    override def iterator: Iterator[A] =
      new NonStrictIteratorOps.TakeWhile(these.iterator, p)
  }

  class Drop[+A](
      protected[this] override val these: Container[A],
      protected[this] override val lower: Int)
    extends NonStrictCollectionOps.Drop[A](these, lower) with Container[A] {

    override def iterator: Iterator[A] =
      new NonStrictIteratorOps.Drop(these.iterator, lower)
  }

  class Take[+A](
      protected[this] override val these: Container[A],
      protected[this] override val upper: Int)
    extends NonStrictCollectionOps.Take[A](these, upper) with Container[A] {

    override def iterator: Iterator[A] =
      new NonStrictIteratorOps.Take(these.iterator, upper)
  }

  class Slice[+A](
      protected[this] override val these: Container[A],
      protected[this] override val lower: Int,
      protected[this] override val upper: Int)
    extends NonStrictCollectionOps.Slice[A](these, lower, upper) with Container[A] {

    override def iterator: Iterator[A] =
      new NonStrictIteratorOps.Slice(these.iterator, lower, upper)
  }

  class Zip[+A, +B](
      protected[this] val these: Container[A],
      protected[this] val those: Container[B])
    extends Container[(A, B)] {

    override def iterator: Iterator[(A, B)] =
      new NonStrictIteratorOps.Zip(these.iterator, those.iterator)
  }

  class ++[+A](
      protected[this] override val these: Container[A],
      protected[this] override val those: Container[A])
    extends NonStrictCollectionOps.++[A](these, those) with Container[A] {

    override def iterator: Iterator[A] =
      new NonStrictIteratorOps.++(these.iterator, those.iterator)
  }
}
