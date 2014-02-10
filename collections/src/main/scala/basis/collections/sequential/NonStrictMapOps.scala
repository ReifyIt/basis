//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

final class NonStrictMapOps[+A, +T](val __ : Map[A, T]) extends AnyVal {
  def filter(p: ((A, T)) => Boolean): Map[A, T] =
    new NonStrictMapOps.Filter(__, p)

  def withFilter(p: ((A, T)) => Boolean): Map[A, T] =
    new NonStrictMapOps.Filter(__, p)

  def dropWhile(p: ((A, T)) => Boolean): Map[A, T] =
    new NonStrictMapOps.DropWhile(__, p)

  def takeWhile(p: ((A, T)) => Boolean): Map[A, T] =
    new NonStrictMapOps.TakeWhile(__, p)

  def span(p: ((A, T)) => Boolean): (Map[A, T], Map[A, T]) =
    (takeWhile(p), dropWhile(p))

  def drop(lower: Int): Map[A, T] =
    new NonStrictMapOps.Drop(__, lower)

  def take(upper: Int): Map[A, T] =
    new NonStrictMapOps.Take(__, upper)

  def slice(lower: Int, upper: Int): Map[A, T] =
    new NonStrictMapOps.Slice(__, lower, upper)

  def ++ [B >: A, U >: T](those: Map[B, U]): Map[B, U] =
    new NonStrictMapOps.++(__, those)
}

private[sequential] object NonStrictMapOps {
  import scala.annotation._
  import scala.annotation.unchecked._

  class Filter[+A, +T](
      protected[this] override val these: Map[A, T],
      protected[this] override val p: ((A, T)) => Boolean)
    extends NonStrictContainerOps.Filter[(A, T)](these, p) with Map[A, T] {

    override def contains(key: A @uncheckedVariance): Boolean =
      try p((key, these(key))) catch { case _: NoSuchElementException => false }
  }

  class DropWhile[+A, +T](
      protected[this] override val these: Map[A, T],
      protected[this] override val p: ((A, T)) => Boolean)
    extends NonStrictContainerOps.DropWhile[(A, T)](these, p) with Map[A, T]

  class TakeWhile[+A, +T](
      protected[this] override val these: Map[A, T],
      protected[this] override val p: ((A, T)) => Boolean)
    extends NonStrictContainerOps.TakeWhile[(A, T)](these, p) with Map[A, T]

  class Drop[+A, +T](
      protected[this] override val these: Map[A, T],
      protected[this] override val lower: Int)
    extends NonStrictContainerOps.Drop[(A, T)](these, lower) with Map[A, T]

  class Take[+A, +T](
      protected[this] override val these: Map[A, T],
      protected[this] override val upper: Int)
    extends NonStrictContainerOps.Take[(A, T)](these, upper) with Map[A, T]

  class Slice[+A, +T](
      protected[this] override val these: Map[A, T],
      protected[this] override val lower: Int,
      protected[this] override val upper: Int)
    extends NonStrictContainerOps.Slice[(A, T)](these, lower, upper) with Map[A, T]

  class ++[+A, +T](
      protected[this] override val these: Map[A, T],
      protected[this] override val those: Map[A, T])
    extends NonStrictContainerOps.++[(A, T)](these, those) with Map[A, T] {

    override def contains(key: A @uncheckedVariance): Boolean =
      these.contains(key) || those.contains(key)

    override def iterator: Iterator[(A, T)] = {
      if (these.isEmpty) those.iterator
      else if (those.isEmpty) these.iterator
      else new CombinedIterator(these, those)
    }
  }

  final class CombinedIterator[+A, +T] private (
      protected[this] val these: Iterator[(A, T)],
      protected[this] val those: Iterator[(A, T)],
      protected[this] val that: Map[A, T],
      protected[this] var segment: Int)
    extends Iterator[(A, T)] {

    def this(these: Map[A, T], those: Map[A, T]) = this(these.iterator, those.iterator, those, 0)

    @tailrec override def isEmpty: Boolean = segment match {
      case 0 =>
        if (these.isEmpty) { segment = 1; isEmpty }
        else if (that.contains(these.head._1)) { these.step(); isEmpty }
        else false
      case 1 => those.isEmpty
    }

    @tailrec override def head: (A, T) = segment match {
      case 0 =>
        if (these.isEmpty) { segment = 1; head }
        else {
          val x = these.head
          if (that.contains(x._1)) { these.step(); head } else x
        }
      case 1 => those.head
    }

    override def step(): Unit = segment match {
      case 0 => if (these.isEmpty) segment = 1 else these.step()
      case 1 => those.step()
    }

    override def dup: Iterator[(A, T)] = segment match {
      case 0 => new CombinedIterator(these.dup, those.dup, that, 0)
      case 1 => those.dup
    }
  }
}
