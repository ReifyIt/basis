//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis.util._

final class NonStrictLinearSeqOps[+A](val __ : LinearSeq[A]) extends AnyVal {
  def ++ [B >: A](those: LinearSeq[B]): LinearSeq[B]      = new NonStrictLinearSeqOps.++(__, those)
  def collect[B](q: PartialFunction[A, B]): LinearSeq[B]  = new NonStrictLinearSeqOps.Collect(__, q)
  def drop(lower: Int): LinearSeq[A]                      = new NonStrictLinearSeqOps.Drop(__, lower)
  def dropWhile(p: A => Boolean): LinearSeq[A]            = new NonStrictLinearSeqOps.DropWhile(__, p)
  def filter(p: A => Boolean): LinearSeq[A]               = new NonStrictLinearSeqOps.Filter(__, p)
  def flatMap[B](f: A => LinearSeq[B]): LinearSeq[B]      = new NonStrictLinearSeqOps.FlatMap(__, f)
  def map[B](f: A => B): LinearSeq[B]                     = new NonStrictLinearSeqOps.Map(__, f)
  def slice(lower: Int, upper: Int): LinearSeq[A]         = new NonStrictLinearSeqOps.Slice(__, lower, upper)
  def span(p: A => Boolean): (LinearSeq[A], LinearSeq[A]) = (takeWhile(p), dropWhile(p))
  def take(upper: Int): LinearSeq[A]                      = new NonStrictLinearSeqOps.Take(__, upper)
  def takeWhile(p: A => Boolean): LinearSeq[A]            = new NonStrictLinearSeqOps.TakeWhile(__, p)
  def withFilter(p: A => Boolean): LinearSeq[A]           = new NonStrictLinearSeqOps.Filter(__, p)
  def zip[B](those: LinearSeq[B]): LinearSeq[(A, B)]      = new NonStrictLinearSeqOps.Zip(__, those)
}

private[sequential] object NonStrictLinearSeqOps {
  import scala.annotation._

  final class Collect[-A, +B](
      private[this] var these: LinearSeq[A],
      private[this] val q: PartialFunction[A, B])
    extends LinearSeq[B] {

    @tailrec override def isEmpty: Boolean =
      these.isEmpty || !q.isDefinedAt(these.head) && { these = these.tail; isEmpty }

    @tailrec override def head: B = {
      val x = these.head
      if (q.isDefinedAt(x)) q(x)
      else { these = these.tail; head }
    }

    @tailrec override def tail: LinearSeq[B] = {
      if (!these.isEmpty && q.isDefinedAt(these.head)) new Collect(these.tail, q)
      else { these = these.tail; tail }
    }
  }

  final class Map[-A, +B](
      private[this] val these: LinearSeq[A],
      private[this] val f: A => B)
    extends LinearSeq[B] {

    override def isEmpty: Boolean = these.isEmpty

    override def head: B = f(these.head)

    override def tail: LinearSeq[B] = new Map(these.tail, f)
  }

  final class FlatMap[-A, +B] private (
      private[this] var these: LinearSeq[A],
      private[this] val f: A => LinearSeq[B],
      private[this] var inner: LinearSeq[B])
    extends LinearSeq[B] {

    def this(these: LinearSeq[A], f: A => LinearSeq[B]) = this(these, f, LinearSeq.empty)

    @tailrec override def isEmpty: Boolean =
      inner.isEmpty && (these.isEmpty || { inner = f(these.head); these = these.tail; isEmpty })

    @tailrec override def head: B = {
      if (!inner.isEmpty) inner.head
      else if (!these.isEmpty) { inner = f(these.head); these = these.tail; head }
      else LinearSeq.empty.head
    }

    override def tail: LinearSeq[B] = {
      if (!inner.isEmpty) new FlatMap(these, f, inner.tail)
      else if (!these.isEmpty) new FlatMap(these.tail, f, f(these.head))
      else LinearSeq.empty.tail
    }
  }

  final class Filter[+A](
      private[this] var these: LinearSeq[A],
      private[this] val p: A => Boolean)
    extends LinearSeq[A] {

    @tailrec override def isEmpty: Boolean =
      these.isEmpty || !p(these.head) && { these = these.tail; isEmpty }

    @tailrec override def head: A = {
      val x = these.head
      if (p(x)) x else { these = these.tail; head }
    }

    @tailrec override def tail: LinearSeq[A] = {
      if (!these.isEmpty && p(these.head)) new Filter(these.tail, p)
      else { these = these.tail; tail }
    }
  }

  final class DropWhile[+A](
      private[this] var these: LinearSeq[A],
      private[this] val p: A => Boolean)
    extends LinearSeq[A] {

    private[this] var dropped: Boolean = false

    @tailrec override def isEmpty: Boolean =
      these.isEmpty || (!dropped && (if (p(these.head)) { these = these.tail; isEmpty } else { dropped = true; false }))

    @tailrec override def head: A = {
      if (dropped) these.head
      else {
        val x = these.head
        if (!p(x)) { dropped = true; x } else { these = these.tail; head }
      }
    }

    @tailrec override def tail: LinearSeq[A] = {
      if (dropped) these.tail
      else if (!p(these.head)) { dropped = true; tail }
      else { these = these.tail; tail }
    }
  }

  final class TakeWhile[+A](
      private[this] val these: LinearSeq[A],
      private[this] val p: A => Boolean)
    extends LinearSeq[A] {

    private[this] lazy val taking: Boolean = !these.isEmpty && p(these.head)

    override def isEmpty: Boolean = !taking

    override def head: A = {
      if (taking) these.head
      else LinearSeq.empty.head
    }

    override def tail: LinearSeq[A] = {
      if (taking) new TakeWhile(these.tail, p)
      else LinearSeq.empty.tail
    }
  }

  final class Drop[+A] private (
      private[this] var these: LinearSeq[A],
      private[this] val lower: Int,
      private[this] var index: Int)
    extends LinearSeq[A] {

    def this(these: LinearSeq[A], lower: Int) = this(these, lower, 0)

    @tailrec override def isEmpty: Boolean =
      these.isEmpty || index < lower && { these = these.tail; index += 1; isEmpty }

    @tailrec override def head: A = {
      if (index >= lower) these.head
      else { these = these.tail; index += 1; head }
    }

    @tailrec override def tail: LinearSeq[A] = {
      if (index >= lower) these.tail
      else { these = these.tail; index += 1; tail }
    }
  }

  final class Take[+A] private (
      private[this] val these: LinearSeq[A],
      private[this] val upper: Int,
      private[this] val index: Int)
    extends LinearSeq[A] {

    def this(these: LinearSeq[A], upper: Int) = this(these, upper, 0)

    override def isEmpty: Boolean =
      index >= upper || these.isEmpty

    override def head: A = {
      if (index < upper) these.head
      else LinearSeq.empty.head
    }

    override def tail: LinearSeq[A] = {
      if (index < upper) new Take(these.tail, upper, index + 1)
      else LinearSeq.empty.tail
    }
  }

  final class Slice[+A] private (
      private[this] var these: LinearSeq[A],
      private[this] val lower: Int,
      private[this] val upper: Int,
      private[this] var index: Int)
    extends LinearSeq[A] {

    def this(these: LinearSeq[A], lower: Int, upper: Int) =
      this(these,0 max lower, 0 max lower max upper, 0)

    @tailrec override def isEmpty: Boolean =
      index >= upper || these.isEmpty || index < lower && { these = these.tail; index += 1; isEmpty }

    @tailrec override def head: A = {
      if (index < lower) { these = these.tail; index += 1; head }
      else if (index < upper) these.head
      else LinearSeq.empty.head
    }

    @tailrec override def tail: LinearSeq[A] = {
      if (index < lower) { these = these.tail; index += 1; tail }
      else if (index < upper) new Slice(these.tail, lower, upper, index + 1)
      else LinearSeq.empty.tail
    }
  }

  final class Zip[+A, +B](
      private[this] val these: LinearSeq[A],
      private[this] val those: LinearSeq[B])
    extends LinearSeq[(A, B)] {

    override def isEmpty: Boolean = these.isEmpty || those.isEmpty

    override def head: (A, B) = (these.head, those.head)

    override def tail: LinearSeq[(A, B)] = new Zip(these.tail, those.tail)
  }

  final class ++[+A] private (
      private[this] val these: LinearSeq[A],
      private[this] val those: LinearSeq[A],
      private[this] var segment: Int)
    extends LinearSeq[A] {

    def this(these: LinearSeq[A], those: LinearSeq[A]) = this(these, those, 0)

    @tailrec override def isEmpty: Boolean = segment match {
      case 0 => these.isEmpty && { segment = 1; isEmpty }
      case 1 => those.isEmpty
    }

    @tailrec override def head: A = segment match {
      case 0 => if (!these.isEmpty) these.head else { segment = 1; head }
      case 1 => those.head
    }

    override def tail: LinearSeq[A] = segment match {
      case 0 if !these.isEmpty => new ++(these.tail, those, 0)
      case _ => those.tail
    }
  }
}
