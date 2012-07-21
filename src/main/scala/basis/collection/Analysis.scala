/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

import scala.runtime.AbstractFunction1

private[basis] object Analysis {
  final class Select[-A, +B](q: PartialFunction[A, B]) extends AbstractFunction1[A, Unit] {
    private[this] var a: Option[B] = None
    override def apply(x: A): Unit = if (q.isDefinedAt(x)) { a = Some(q(x)); throw Break }
    def result: Option[B] = a
  }
  
  final class FoldLeft[-A, +B](z: B)(op: (B, A) => B) extends AbstractFunction1[A, Unit] {
    private[this] var a: B = z
    override def apply(x: A): Unit = a = op(a, x)
    def result: B = a
  }
  
  final class ReduceLeft[-A, +B >: A](op: (B, A) => B) extends AbstractFunction1[A, Unit] {
    private[this] var b: Boolean = false
    private[this] var a: B = _
    override def apply(x: A): Unit = if (!b) { a = x; b = true } else a = op(a, x)
    def isDefined: Boolean = b
    def result: B = a
  }
  
  final class Find[A](p: A => Boolean) extends AbstractFunction1[A, Unit] {
    private[this] var a: Option[A] = None
    override def apply(x: A): Unit = if (p(x)) { a = Some(x); throw Break }
    def result: Option[A] = a
  }
  
  final class Forall[A](p: A => Boolean) extends AbstractFunction1[A, Unit] {
    private[this] var b: Boolean = true
    override def apply(x: A): Unit = if (!p(x)) { b = false; throw Break }
    def result: Boolean = b
  }
  
  final class Exists[A](p: A => Boolean) extends AbstractFunction1[A, Unit] {
    private[this] var b: Boolean = false
    override def apply(x: A): Unit = if (p(x)) { b = true; throw Break }
    def result: Boolean = b
  }
  
  final class Count[A](p: A => Boolean) extends AbstractFunction1[A, Unit] {
    private[this] var total: Int = 0
    override def apply(x: A): Unit = if (p(x)) total += 1
    def result: Int = total
  }
  
  final class AddString[A](s: java.lang.StringBuilder, sep: String) extends AbstractFunction1[A, Unit] {
    private[this] var q = true
    override def apply(x: A): Unit = (if (q) { q = false; s } else s.append(sep)).append(x)
  }
  
  final class FlatMap[-A, +B, +U](f: B => U, g: A => Once[B]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = g(x).foreach(f)
  }
  
  final class Filter[-A, +U](f: A => U, p: A => Boolean) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (p(x)) f(x)
  }
  
  final class Collect[-A, +B, +U](f: B => U, q: PartialFunction[A, B]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (q.isDefinedAt(x)) f(q(x))
  }
  
  final class DropWhile[-A, +U](f: A => U, p: A => Boolean) extends AbstractFunction1[A, Unit] {
    private[this] var taking: Boolean = false
    override def apply(x: A) {
      if (!taking && !p(x)) taking = true
      if (taking) f(x)
    }
  }
  
  final class TakeWhile[-A, +U](f: A => U, p: A => Boolean) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (p(x)) f(x) else throw Break
  }
  
  final class Drop[-A, +U](f: A => U, lower: Int) extends AbstractFunction1[A, Unit] {
    private[this] var i = 0
    override def apply(x: A): Unit = if (i >= lower) f(x) else i += 1
  }
  
  final class Take[-A, +U](f: A => U, upper: Int) extends AbstractFunction1[A, Unit] {
    private[this] var i = 0
    override def apply(x: A): Unit = if (i < upper) { f(x); i += 1 } else throw Break
  }
  
  final class Slice[-A, +U](f: A => U, lower: Int, upper: Int) extends AbstractFunction1[A, Unit] {
    private[this] var i = 0
    override def apply(x: A): Unit = if (i < upper) { if (i >= lower) f(x); i += 1 } else throw Break
  }
  
  final class Maker[-A](make: Make[_, A]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = make += x
  }
  
  final class MakeMap[-A, +B](f: A => B, make: Make[_, B]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = make += f(x)
  }
  
  final class MakeFlatMap[-A, +B](f: A => Once[B], make: Make[_, B]) extends AbstractFunction1[A, Unit] {
    private[this] val maker = new Maker(make)
    override def apply(x: A): Unit = f(x).foreach(maker)
  }
  
  final class MakeFilter[-A](p: A => Boolean, make: Make[_, A]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (p(x)) make += x
  }
  
  final class MakeCollect[-A, B](q: PartialFunction[A, B], make: Make[_, B]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (q.isDefinedAt(x)) make += q(x)
  }
}
