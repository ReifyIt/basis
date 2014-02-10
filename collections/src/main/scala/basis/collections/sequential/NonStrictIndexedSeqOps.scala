//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis.util._

final class NonStrictIndexedSeqOps[+A](val __ : IndexedSeq[A]) extends AnyVal {
  def collect[B](q: PartialFunction[A, B]): IndexedSeq[B] =
    new NonStrictIndexedSeqOps.Collect(__, q)

  def map[B](f: A => B): IndexedSeq[B] =
    new NonStrictIndexedSeqOps.Map(__, f)

  def filter(p: A => Boolean): IndexedSeq[A] =
    new NonStrictIndexedSeqOps.Filter(__, p)

  def withFilter(p: A => Boolean): IndexedSeq[A] =
    new NonStrictIndexedSeqOps.Filter(__, p)

  def dropWhile(p: A => Boolean): IndexedSeq[A] =
    new NonStrictIndexedSeqOps.DropWhile(__, p)

  def takeWhile(p: A => Boolean): IndexedSeq[A] =
    new NonStrictIndexedSeqOps.TakeWhile(__, p)

  def span(p: A => Boolean): (IndexedSeq[A], IndexedSeq[A]) =
    (takeWhile(p), dropWhile(p))

  def drop(lower: Int): IndexedSeq[A] =
    new NonStrictIndexedSeqOps.Drop(__, lower)

  def take(upper: Int): IndexedSeq[A] =
    new NonStrictIndexedSeqOps.Take(__, upper)

  def slice(lower: Int, upper: Int): IndexedSeq[A] =
    new NonStrictIndexedSeqOps.Slice(__, lower, upper)

  def reverse: IndexedSeq[A] =
    new NonStrictIndexedSeqOps.Reverse(__)

  def zip[B](those: IndexedSeq[B]): IndexedSeq[(A, B)] =
    new NonStrictIndexedSeqOps.Zip(__, those)

  def ++ [B >: A](those: IndexedSeq[B]): IndexedSeq[B] =
    new NonStrictIndexedSeqOps.++(__, those)
}

private[sequential] object NonStrictIndexedSeqOps {
  import scala.annotation._

  final class Collect[-A, +B](these: IndexedSeq[A], q: PartialFunction[A, B]) extends IndexedSeq[B] {
    private[this] var table: Array[Int] = _
    private[this] def lookup: Array[Int] = synchronized {
      if (table == null) {
        val n = these.length
        var i = 0
        var j = 0
        table = new Array[Int](n)
        while (j < n) {
          if (q.isDefinedAt(these(j))) {
            table(i) = j
            i += 1
          }
          j += 1
        }
        if (i != n) {
          val newTable = new Array[Int](i)
          java.lang.System.arraycopy(table, 0, newTable, 0, i)
          table = newTable
        }
      }
      table
    }

    override def length: Int = lookup.length

    override def apply(index: Int): B = q(these(lookup(index)))
  }

  final class Map[-A, +B](these: IndexedSeq[A], f: A => B) extends IndexedSeq[B] {
    override def length: Int = these.length

    override def apply(index: Int): B = f(these(index))
  }

  final class Filter[+A](these: IndexedSeq[A], p: A => Boolean) extends IndexedSeq[A] {
    private[this] var table: Array[Int] = _
    private[this] def lookup: Array[Int] = synchronized {
      if (table == null) {
        val n = these.length
        var i = 0
        var j = 0
        table = new Array[Int](n)
        while (j < n) {
          if (p(these(j))) {
            table(i) = j
            i += 1
          }
          j += 1
        }
        if (i != n) {
          val newTable = new Array[Int](i)
          java.lang.System.arraycopy(table, 0, newTable, 0, i)
          table = newTable
        }
      }
      table
    }

    override def length: Int = lookup.length

    override def apply(index: Int): A = these(lookup(index))
  }

  final class DropWhile[+A](these: IndexedSeq[A], p: A => Boolean) extends IndexedSeq[A] {
    private[this] var lower: Int = -1
    private[this] def offset: Int = synchronized {
      if (lower < 0) {
        val n = these.length
        lower = 0
        while (lower < n && p(these(lower))) lower += 1
      }
      lower
    }

    override def length: Int = these.length - offset

    override def apply(index: Int): A = {
      val i = offset + index
      if (i < 0 || i >= length) throw new IndexOutOfBoundsException(index.toString)
      these(i)
    }
  }

  final class TakeWhile[+A](these: IndexedSeq[A], p: A => Boolean) extends IndexedSeq[A] {
    private[this] var upper: Int = -1

    override def length: Int = synchronized {
      if (upper < 0) {
        val n = these.length
        upper = 0
        while (upper < n && p(these(upper))) upper += 1
      }
      upper
    }

    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      these(index)
    }
  }

  final class Drop[+A](these: IndexedSeq[A], lower: Int) extends IndexedSeq[A] {
    private[this] val offset: Int = 0 max lower min these.length

    override def length: Int = these.length - offset

    override def apply(index: Int): A = {
      val i = offset + index
      if (i < 0 || i >= length) throw new IndexOutOfBoundsException(index.toString)
      these(i)
    }
  }

  final class Take[+A](these: IndexedSeq[A], upper: Int) extends IndexedSeq[A] {
    override val length: Int = 0 max upper min these.length

    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      these(index)
    }
  }

  final class Slice[+A](these: IndexedSeq[A], lower: Int, upper: Int) extends IndexedSeq[A] {
    private[this] val offset: Int = 0 max lower min these.length

    override val length: Int = (offset max upper min these.length) - offset

    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      these(offset + index)
    }
  }

  final class Reverse[+A](these: IndexedSeq[A]) extends IndexedSeq[A] {
    override def length: Int = these.length

    override def apply(index: Int): A = {
      val n = these.length
      if (index < 0 || index >= n) throw new IndexOutOfBoundsException(index.toString)
      these(n - index - 1)
    }
  }

  final class Zip[+A, +B](these: IndexedSeq[A], those: IndexedSeq[B]) extends IndexedSeq[(A, B)] {
    override val length: Int = these.length min those.length

    override def apply(index: Int): (A, B) = (these(index), those(index))
  }

  final class ++[+A](these: IndexedSeq[A], those: IndexedSeq[A]) extends IndexedSeq[A] {
    override val length: Int = these.length + those.length

    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      val n = these.length
      if (index < n) these(index) else those(index - n)
    }
  }
}
