//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis.util._

final class NonStrictIteratorOps[+A](val __ : Iterator[A]) extends AnyVal {
  def collect[B](q: PartialFunction[A, B]): Iterator[B] =
    new NonStrictIteratorOps.Collect(__.dup, q)

  def map[B](f: A => B): Iterator[B] =
    new NonStrictIteratorOps.Map(__.dup, f)

  def flatMap[B](f: A => Iterator[B]): Iterator[B] =
    new NonStrictIteratorOps.FlatMap(__.dup, f)

  def filter(p: A => Boolean): Iterator[A] =
    new NonStrictIteratorOps.Filter(__.dup, p)

  def withFilter(p: A => Boolean): Iterator[A] =
    new NonStrictIteratorOps.Filter(__.dup, p)

  def dropWhile(p: A => Boolean): Iterator[A] =
    new NonStrictIteratorOps.DropWhile(__.dup, p)

  def takeWhile(p: A => Boolean): Iterator[A] =
    new NonStrictIteratorOps.TakeWhile(__.dup, p)

  def span(p: A => Boolean): (Iterator[A], Iterator[A]) =
    (takeWhile(p), dropWhile(p))

  def drop(lower: Int): Iterator[A] =
    new NonStrictIteratorOps.Drop(__.dup, lower)

  def take(upper: Int): Iterator[A] =
    new NonStrictIteratorOps.Take(__.dup, upper)

  def slice(lower: Int, upper: Int): Iterator[A] =
    new NonStrictIteratorOps.Slice(__.dup, lower, upper)

  def zip[B](those: Iterator[B]): Iterator[(A, B)] =
    new NonStrictIteratorOps.Zip(__.dup, those.dup)

  def ++ [B >: A](those: Iterator[B]): Iterator[B] =
    new NonStrictIteratorOps.++(__.dup, those.dup)
}

private[sequential] object NonStrictIteratorOps {
  import scala.annotation._

  final class Collect[-A, +B](these: Iterator[A], q: PartialFunction[A, B]) extends Iterator[B] {
    @tailrec override def isEmpty: Boolean =
      these.isEmpty || !q.isDefinedAt(these.head) && { these.step(); isEmpty }

    @tailrec override def head: B = {
      val x = these.head
      if (q.isDefinedAt(x)) q(x)
      else { these.step(); head }
    }

    override def step(): Unit = these.step()

    override def dup: Iterator[B] = new Collect(these.dup, q)
  }

  final class Map[-A, +B](these: Iterator[A], f: A => B) extends Iterator[B] {
    override def isEmpty: Boolean = these.isEmpty

    override def head: B = f(these.head)

    override def step(): Unit = these.step()

    override def dup: Iterator[B] = new Map(these.dup, f)
  }

  final class FlatMap[-A, +B] private
      (these: Iterator[A], f: A => Iterator[B], private[this] var inner: Iterator[B])
    extends Iterator[B] {

    def this(these: Iterator[A], f: A => Iterator[B]) = this(these, f, Iterator.empty)

    @tailrec override def isEmpty: Boolean =
      inner.isEmpty && (these.isEmpty || { inner = f(these.head); these.step(); isEmpty })

    @tailrec override def head: B = {
      if (!inner.isEmpty) inner.head
      else if (!these.isEmpty) { inner = f(these.head); these.step(); head }
      else Iterator.empty.head
    }

    @tailrec override def step(): Unit = {
      if (!inner.isEmpty) inner.step()
      else if (!these.isEmpty) { inner = f(these.head); these.step(); step() }
      else Iterator.empty.step()
    }

    override def dup: Iterator[B] = new FlatMap(these.dup, f, inner.dup)
  }

  final class FlatMapContainer[-A, +B] private
      (these: Iterator[A], f: A => Container[B], private[this] var inner: Iterator[B])
    extends Iterator[B] {

    def this(these: Iterator[A], f: A => Container[B]) = this(these, f, Iterator.empty)

    @tailrec override def isEmpty: Boolean =
      inner.isEmpty && (these.isEmpty || { inner = f(these.head).iterator; these.step(); isEmpty })

    @tailrec override def head: B = {
      if (!inner.isEmpty) inner.head
      else if (!these.isEmpty) { inner = f(these.head).iterator; these.step(); head }
      else Iterator.empty.head
    }

    @tailrec override def step(): Unit = {
      if (!inner.isEmpty) inner.step()
      else if (!these.isEmpty) { inner = f(these.head).iterator; these.step(); step() }
      else Iterator.empty.step()
    }

    override def dup: Iterator[B] = new FlatMapContainer(these.dup, f, inner.dup)
  }

  final class Filter[+A](these: Iterator[A], p: A => Boolean) extends Iterator[A] {
    @tailrec override def isEmpty: Boolean =
      these.isEmpty || !p(these.head) && { these.step(); isEmpty }

    @tailrec override def head: A = {
      val x = these.head
      if (p(x)) x else { these.step(); head }
    }

    override def step(): Unit = these.step()

    override def dup: Iterator[A] = new Filter(these.dup, p)
  }

  final class DropWhile[+A] private
      (these: Iterator[A], p: A => Boolean, private[this] var dropped: Boolean)
    extends Iterator[A] {

    def this(these: Iterator[A], p: A => Boolean) = this(these, p, false)

    @tailrec override def isEmpty: Boolean =
      these.isEmpty || (!dropped && (if (p(these.head)) { these.step(); isEmpty } else { dropped = true; false }))

    @tailrec override def head: A = {
      if (dropped) these.head
      else {
        val x = these.head
        if (!p(x)) { dropped = true; x } else { these.step(); head }
      }
    }

    @tailrec override def step(): Unit = {
      if (dropped) these.step()
      else if (!p(these.head)) { dropped = true; these.step() }
      else { these.step(); step() }
    }

    override def dup: Iterator[A] = new DropWhile(these.dup, p, dropped)
  }

  final class TakeWhile[+A] private
      (these: Iterator[A], p: A => Boolean, private[this] var taking: Boolean)
    extends Iterator[A] {

    def this(these: Iterator[A], p: A => Boolean) = this(these, p, true)

    override def isEmpty: Boolean =
      !taking && (these.isEmpty || !p(these.head) && { taking = false; true })

    @tailrec override def head: A = {
      if (taking) {
        val x = these.head
        if (p(x)) x else { taking = false; head }
      }
      else Iterator.empty.head
    }

    @tailrec override def step(): Unit = {
      if (taking) {
        if (p(these.head)) these.step()
        else { taking = false; step() }
      }
      else Iterator.empty.step()
    }

    override def dup: Iterator[A] = new TakeWhile(these.dup, p, taking)
  }

  final class Drop[+A] private
      (these: Iterator[A], lower: Int, private[this] var index: Int)
    extends Iterator[A] {

    def this(these: Iterator[A], lower: Int) = this(these, lower, 0)

    @tailrec override def isEmpty: Boolean =
      these.isEmpty || index < lower && { these.step(); index += 1; isEmpty }

    @tailrec override def head: A = {
      if (index >= lower) these.head
      else { these.step(); index += 1; head }
    }

    @tailrec override def step(): Unit = {
      if (index >= lower) these.step()
      else { these.step(); index += 1; step() }
    }

    override def dup: Iterator[A] = new Drop(these.dup, lower, index)
  }

  final class Take[+A] private
      (these: Iterator[A], upper: Int, private[this] var index: Int)
    extends Iterator[A] {

    def this(these: Iterator[A], upper: Int) = this(these, upper, 0)

    override def isEmpty: Boolean =
      index >= upper || these.isEmpty

    override def head: A = {
      if (index < upper) these.head
      else Iterator.empty.head
    }

    override def step(): Unit = {
      if (index < upper) { these.step(); index += 1 }
      else Iterator.empty.step()
    }

    override def dup: Iterator[A] = new Take(these.dup, upper, index)
  }

  final class Slice[+A] private
      (these: Iterator[A], lower: Int, upper: Int, private[this] var index: Int)
    extends Iterator[A] {

    def this(these: Iterator[A], lower: Int, upper: Int) =
      this(these, 0 max lower, 0 max lower max upper, 0)

    @tailrec override def isEmpty: Boolean =
      index >= upper || these.isEmpty || index < lower && { these.step(); index += 1; isEmpty }

    @tailrec override def head: A = {
      if (index < lower) { these.step(); index += 1; head }
      else if (index < upper) these.head
      else Iterator.empty.head
    }

    @tailrec override def step(): Unit = {
      if (index < lower) { these.step(); index += 1; step() }
      else if (index < upper) these.step()
      else Iterator.empty.step()
    }

    override def dup: Iterator[A] = new Slice(these.dup, lower, upper, index)
  }

  final class Zip[+A, +B](these: Iterator[A], those: Iterator[B]) extends Iterator[(A, B)] {
    override def isEmpty: Boolean = these.isEmpty || those.isEmpty

    override def head: (A, B) = (these.head, those.head)

    override def step(): Unit = {
      these.step()
      those.step()
    }

    override def dup: Iterator[(A, B)] = new Zip(these.dup, those.dup)
  }

  final class ++[+A] private
      (these: Iterator[A], those: Iterator[A], private[this] var segment: Int)
    extends Iterator[A] {

    def this(these: Iterator[A], those: Iterator[A]) = this(these, those, 0)

    @tailrec override def isEmpty: Boolean = (segment: @switch) match {
      case 0 => these.isEmpty && { segment = 1; isEmpty }
      case 1 => those.isEmpty
    }

    @tailrec override def head: A = (segment: @switch) match {
      case 0 => if (!these.isEmpty) these.head else { segment = 1; head }
      case 1 => those.head
    }

    @tailrec override def step(): Unit = (segment: @switch) match {
      case 0 => if (!these.isEmpty) these.step() else { segment = 1; step() }
      case 1 => those.step()
    }

    override def dup: Iterator[A] = (segment: @switch) match {
      case 0 if !these.isEmpty => new ++(these.dup, those.dup, 0)
      case _ => those.dup
    }
  }
}
