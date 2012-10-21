/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

private[basis] object Traversers {
  import scala.runtime.AbstractFunction1
  import Enumerator.traverse
  
  final class FoldLeft[-A, +B](z: B)(op: (B, A) => B) extends AbstractFunction1[A, Unit] {
    private[this] var r: B = z
    override def apply(x: A): Unit = r = op(r, x)
    def state: B = r
  }
  
  final class ReduceLeft[-A, +B >: A](op: (B, A) => B) extends AbstractFunction1[A, Unit] {
    private[this] var e: Boolean = false
    private[this] var r: B = _
    override def apply(x: A): Unit = if (!e) { r = x; e = true } else r = op(r, x)
    def isDefined: Boolean = e
    def state: B = r
  }
  
  final class Find[A](p: A => Boolean) extends AbstractFunction1[A, Unit] {
    private[this] var r: Option[A] = None
    override def apply(x: A): Unit = if (p(x)) { r = Some(x); throw Break }
    def state: Option[A] = r
  }
  
  final class Forall[A](p: A => Boolean) extends AbstractFunction1[A, Unit] {
    private[this] var r: Boolean = true
    override def apply(x: A): Unit = if (!p(x)) { r = false; throw Break }
    def state: Boolean = r
  }
  
  final class Exists[A](p: A => Boolean) extends AbstractFunction1[A, Unit] {
    private[this] var r: Boolean = false
    override def apply(x: A): Unit = if (p(x)) { r = true; throw Break }
    def state: Boolean = r
  }
  
  final class Count[A](p: A => Boolean) extends AbstractFunction1[A, Unit] {
    private[this] var t: Int = 0
    override def apply(x: A): Unit = if (p(x)) t += 1
    def state: Int = t
  }
  
  final class Select[-A, +B](q: scala.PartialFunction[A, B]) extends AbstractFunction1[A, Unit] {
    private[this] var r: Option[B] = None
    override def apply(x: A): Unit = if (q.isDefinedAt(x)) { r = Some(q(x)); throw Break }
    def state: Option[B] = r
  }
  
  final class Collect[-A, +B, +U](q: scala.PartialFunction[A, B], f: B => U) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (q.isDefinedAt(x)) f(q(x))
  }
  
  final class FlatMap[-A, +B, +U](g: A => Enumerator[B], f: B => U) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = traverse(g(x))(f)
  }
  
  final class Filter[-A, +U](p: A => Boolean, f: A => U) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (p(x)) f(x)
  }
  
  final class DropWhile[-A, +U](p: A => Boolean, f: A => U) extends AbstractFunction1[A, Unit] {
    private[this] var taking: Boolean = false
    override def apply(x: A): Unit = if (taking || (!p(x) && { taking = true; true })) f(x)
  }
  
  final class TakeWhile[-A, +U](p: A => Boolean, f: A => U) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (p(x)) f(x) else throw Break
  }
  
  final class Span[-A, +U1, +U2](p: A => Boolean, f1: A => U1, f2: A => U2) extends AbstractFunction1[A, Unit] {
    private[this] var taking: Boolean = false
    override def apply(x: A): Unit = if (!taking && (p(x) || { taking = true; false })) f1(x) else f2(x)
  }
  
  final class Drop[-A, +U](lower: Int, f: A => U) extends AbstractFunction1[A, Unit] {
    private[this] var i = 0
    override def apply(x: A): Unit = if (i >= lower) f(x) else i += 1
  }
  
  final class Take[-A, +U](upper: Int, f: A => U) extends AbstractFunction1[A, Unit] {
    private[this] var i = 0
    override def apply(x: A): Unit = if (i < upper) { f(x); i += 1 } else throw Break
  }
  
  final class Slice[-A, +U](lower: Int, upper: Int, f: A => U) extends AbstractFunction1[A, Unit] {
    private[this] var l = scala.math.max(0, lower)
    private[this] var u = scala.math.max(l, upper)
    private[this] var i = 0
    override def apply(x: A): Unit = if (i < u) { if (i >= l) f(x); i += 1 } else throw Break
  }
  
  final class CollectInto[-A, B](q: scala.PartialFunction[A, B], buffer: Buffer[_, B]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (q.isDefinedAt(x)) buffer += q(x)
  }
  
  final class MapInto[-A, +B](f: A => B, buffer: Buffer[_, B]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = buffer += f(x)
  }
  
  final class FlatMapInto[-A, +B](f: A => Enumerator[B], buffer: Buffer[_, B]) extends AbstractFunction1[A, Unit] {
    private[this] val add = new Traversers.AddInto(buffer)
    override def apply(x: A): Unit = traverse(f(x))(add)
  }
  
  final class FilterInto[-A](p: A => Boolean, buffer: Buffer[_, A]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (p(x)) buffer += x
  }
  
  final class DropWhileInto[-A](p: A => Boolean, buffer: Buffer[_, A]) extends AbstractFunction1[A, Unit] {
    private[this] var taking: Boolean = false
    override def apply(x: A): Unit = if (taking || (!p(x) && { taking = true; true })) buffer += x
  }
  
  final class TakeWhileInto[-A](p: A => Boolean, buffer: Buffer[_, A]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (p(x)) buffer += x else throw Break
  }
  
  final class SpanInto[-A](p: A => Boolean, bufferA: Buffer[_, A], bufferB: Buffer[_, A]) extends AbstractFunction1[A, Unit] {
    private[this] var taking: Boolean = false
    override def apply(x: A): Unit = if (!taking && (p(x) || { taking = true; false })) bufferA += x else bufferB += x
  }
  
  final class DropInto[-A](lower: Int, buffer: Buffer[_, A]) extends AbstractFunction1[A, Unit] {
    private[this] var i = 0
    override def apply(x: A): Unit = if (i >= lower) buffer += x else i += 1
  }
  
  final class TakeInto[-A](upper: Int, buffer: Buffer[_, A]) extends AbstractFunction1[A, Unit] {
    private[this] var i = 0
    override def apply(x: A): Unit = if (i < upper) { buffer += x; i += 1 } else throw Break
  }
  
  final class SliceInto[-A](lower: Int, upper: Int, buffer: Buffer[_, A]) extends AbstractFunction1[A, Unit] {
    private[this] var l = scala.math.max(0, lower)
    private[this] var u = scala.math.max(l, upper)
    private[this] var i = 0
    override def apply(x: A): Unit = if (i < u) { if (i >= l) buffer += x; i += 1 } else throw Break
  }
  
  final class AddInto[-A](buffer: Buffer[_, A]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = buffer += x
  }
}
