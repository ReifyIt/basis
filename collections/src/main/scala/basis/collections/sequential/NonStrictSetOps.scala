//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

final class NonStrictSetOps[+A](val __ : Set[A]) extends AnyVal {
  def filter(p: A => Boolean): Set[A] =
    new NonStrictSetOps.Filter(__, p)

  def withFilter(p: A => Boolean): Set[A] =
    new NonStrictSetOps.Filter(__, p)

  def dropWhile(p: A => Boolean): Set[A] =
    new NonStrictSetOps.DropWhile(__, p)

  def takeWhile(p: A => Boolean): Set[A] =
    new NonStrictSetOps.TakeWhile(__, p)

  def span(p: A => Boolean): (Set[A], Set[A]) =
    (takeWhile(p), dropWhile(p))

  def drop(lower: Int): Set[A] =
    new NonStrictSetOps.Drop(__, lower)

  def take(upper: Int): Set[A] =
    new NonStrictSetOps.Take(__, upper)

  def slice(lower: Int, upper: Int): Set[A] =
    new NonStrictSetOps.Slice(__, lower, upper)

  def ++ [B >: A](those: Set[B]): Set[B] =
    new NonStrictSetOps.++(__, those)
}

private[sequential] object NonStrictSetOps {
  import scala.annotation._
  import scala.annotation.unchecked._

  class Filter[+A](
      protected[this] override val these: Set[A],
      protected[this] override val p: A => Boolean)
    extends NonStrictContainerOps.Filter[A](these, p) with Set[A] {

    override def contains(elem: A @uncheckedVariance): Boolean =
      these.contains(elem) && p(elem)
  }

  class DropWhile[+A](
      protected[this] override val these: Set[A],
      protected[this] override val p: A => Boolean)
    extends NonStrictContainerOps.DropWhile[A](these, p) with Set[A]

  class TakeWhile[+A](
      protected[this] override val these: Set[A],
      protected[this] override val p: A => Boolean)
    extends NonStrictContainerOps.TakeWhile[A](these, p) with Set[A]

  class Drop[+A](
      protected[this] override val these: Set[A],
      protected[this] override val lower: Int)
    extends NonStrictContainerOps.Drop[A](these, lower) with Set[A]

  class Take[+A](
      protected[this] override val these: Set[A],
      protected[this] override val upper: Int)
    extends NonStrictContainerOps.Take[A](these, upper) with Set[A]

  class Slice[+A](
      protected[this] override val these: Set[A],
      protected[this] override val lower: Int,
      protected[this] override val upper: Int)
    extends NonStrictContainerOps.Slice[A](these, lower, upper) with Set[A]

  class ++[+A](
      protected[this] override val these: Set[A],
      protected[this] override val those: Set[A])
    extends NonStrictContainerOps.++[A](these, those) with Set[A] {

    override def contains(elem: A @uncheckedVariance): Boolean =
      these.contains(elem) || those.contains(elem)

    override def iterator: Iterator[A] = {
      if (these.isEmpty) those.iterator
      else if (those.isEmpty) these.iterator
      else new CombinedIterator(these, those)
    }
  }

  final class CombinedIterator[+A] private (
      protected[this] val these: Iterator[A],
      protected[this] val those: Iterator[A],
      protected[this] val that: Set[A],
      protected[this] var segment: Int)
    extends Iterator[A] {

    def this(these: Set[A], those: Set[A]) = this(these.iterator, those.iterator, those, 0)

    @tailrec override def isEmpty: Boolean = segment match {
      case 0 =>
        if (these.isEmpty) { segment = 1; isEmpty }
        else if (that.contains(these.head)) { these.step(); isEmpty }
        else false
      case 1 => those.isEmpty
    }

    @tailrec override def head: A = segment match {
      case 0 =>
        if (these.isEmpty) { segment = 1; head }
        else {
          val x = these.head
          if (that.contains(x)) { these.step(); head } else x
        }
      case 1 => those.head
    }

    override def step(): Unit = segment match {
      case 0 => if (these.isEmpty) segment = 1 else these.step()
      case 1 => those.step()
    }

    override def dup: Iterator[A] = segment match {
      case 0 => new CombinedIterator(these.dup, those.dup, that, 0)
      case 1 => those.dup
    }
  }
}
