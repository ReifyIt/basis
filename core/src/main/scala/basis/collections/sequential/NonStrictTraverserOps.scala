//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis.util._

final class NonStrictTraverserOps[+A](val __ : Traverser[A]) extends AnyVal {
  def ++ [B >: A](those: Traverser[B]): Traverser[B]      = new NonStrictTraverserOps.++(__, those)
  def collect[B](q: PartialFunction[A, B]): Traverser[B]  = new NonStrictTraverserOps.Collect(__, q)
  def drop(lower: Int): Traverser[A]                      = new NonStrictTraverserOps.Drop(__, lower)
  def dropWhile(p: A => Boolean): Traverser[A]            = new NonStrictTraverserOps.DropWhile(__, p)
  def filter(p: A => Boolean): Traverser[A]               = new NonStrictTraverserOps.Filter(__, p)
  def flatMap[B](f: A => Traverser[B]): Traverser[B]      = new NonStrictTraverserOps.FlatMap(__, f)
  def map[B](f: A => B): Traverser[B]                     = new NonStrictTraverserOps.Map(__, f)
  def slice(lower: Int, upper: Int): Traverser[A]         = new NonStrictTraverserOps.Slice(__, lower, upper)
  def span(p: A => Boolean): (Traverser[A], Traverser[A]) = (takeWhile(p), dropWhile(p))
  def take(upper: Int): Traverser[A]                      = new NonStrictTraverserOps.Take(__, upper)
  def takeWhile(p: A => Boolean): Traverser[A]            = new NonStrictTraverserOps.TakeWhile(__, p)
  def withFilter(p: A => Boolean): Traverser[A]           = new NonStrictTraverserOps.Filter(__, p)
}

private[sequential] object NonStrictTraverserOps {
  import scala.runtime._

  class Collect[-A, +B](
      protected[this] val these: Traverser[A],
      protected[this] val q: PartialFunction[A, B])
    extends Traverser[B] {

    override def traverse(g: B => Unit): Unit =
      these.traverse(new CollectTraverse(q, g))
  }

  final class CollectTraverse[-A, +B](q: PartialFunction[A, B], g: B => Unit) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (q.isDefinedAt(x)) g(q(x))
  }

  class Map[-A, +B](
      protected[this] val these: Traverser[A],
      protected[this] val f: A => B)
    extends Traverser[B] {

    override def traverse(g: B => Unit): Unit =
      these.traverse(f andThen g)
  }

  class FlatMap[-A, +B](
      protected[this] val these: Traverser[A],
      protected[this] val f: A => Traverser[B])
    extends Traverser[B] {

    override def traverse(g: B => Unit): Unit =
      these.traverse(new FlatMapTraverse(f, g))
  }

  final class FlatMapTraverse[-A, +B](f: A => Traverser[B], g: B => Unit) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = f(x).traverse(g)
  }

  class Filter[+A](
      protected[this] val these: Traverser[A],
      protected[this] val p: A => Boolean)
    extends Traverser[A] {

    override def traverse(g: A => Unit): Unit =
      these.traverse(new FilterTraverse(p, g))
  }

  final class FilterTraverse[-A](p: A => Boolean, g: A => Unit) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (p(x)) g(x)
  }

  class DropWhile[+A](
      protected[this] val these: Traverser[A],
      protected[this] val p: A => Boolean)
    extends Traverser[A] {

    override def traverse(g: A => Unit): Unit =
      these.traverse(new DropWhileTraverse(p, g))
  }

  final class DropWhileTraverse[-A](p: A => Boolean, g: A => Unit) extends AbstractFunction1[A, Unit] {
    private[this] var split = false
    override def apply(x: A): Unit = if (split || (!p(x) && { split = true; true })) g(x)
  }

  class TakeWhile[+A](
      protected[this] val these: Traverser[A],
      protected[this] val p: A => Boolean)
    extends Traverser[A] {

    override def traverse(g: A => Unit): Unit =
      begin { these.traverse(new TakeWhileTraverse(p, g)) }
  }

  final class TakeWhileTraverse[-A](p: A => Boolean, g: A => Unit) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (p(x)) g(x) else begin.break()
  }

  class Drop[+A](
      protected[this] val these: Traverser[A],
      protected[this] val lower: Int)
    extends Traverser[A] {

    override def traverse(g: A => Unit): Unit =
      these.traverse(new DropTraverse(lower, g))
  }

  final class DropTraverse[-A](l: Int, g: A => Unit) extends AbstractFunction1[A, Unit] {
    private[this] var i = 0
    override def apply(x: A): Unit = if (i >= l) g(x) else i += 1
  }

  class Take[+A](
      protected[this] val these: Traverser[A],
      protected[this] val upper: Int)
    extends Traverser[A] {

    override def traverse(g: A => Unit): Unit =
      begin { these.traverse(new TakeTraverse(upper, g)) }
  }

  final class TakeTraverse[-A](u: Int, g: A => Unit) extends AbstractFunction1[A, Unit] {
    private[this] var i = 0
    override def apply(x: A): Unit = if (i < u) { g(x); i += 1 } else begin.break()
  }

  class Slice[+A](
      protected[this] val these: Traverser[A],
      protected[this] val lower: Int,
      protected[this] val upper: Int)
    extends Traverser[A] {

    override def traverse(g: A => Unit): Unit = {
      val l = 0 max lower
      val u = l max upper
      if (l < u) begin { these.traverse(new SliceTraverse(l, u, g)) }
    }
  }

  final class SliceTraverse[-A](l: Int, u: Int, g: A => Unit) extends AbstractFunction1[A, Unit] {
    private[this] var i = 0
    override def apply(x: A): Unit = if (i < u) { if (i >= l) g(x); i += 1 } else begin.break()
  }

  class ++[+A](
      protected[this] val these: Traverser[A],
      protected[this] val those: Traverser[A])
    extends Traverser[A] {

    override def traverse(g: A => Unit): Unit = {
      these.traverse(g)
      those.traverse(g)
    }
  }
}
