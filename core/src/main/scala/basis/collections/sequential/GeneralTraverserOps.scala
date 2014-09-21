//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis._
import basis.text._
import basis.util._

final class GeneralTraverserOps[+A](val __ : Traverser[A]) extends AnyVal {
  def foreach[U](f: A => U): Unit =
    __.traverse(new GeneralTraverserOps.Foreach(f))

  def fold[B >: A](z: B)(op: (B, B) => B): B = {
    val f = new GeneralTraverserOps.FoldLeft(z)(op)
    __.traverse(f)
    f.state
  }

  def reduce[B >: A](op: (B, B) => B): B = {
    val f = new GeneralTraverserOps.ReduceLeft(op)
    __.traverse(f)
    if (f.isDefined) f.state else throw new UnsupportedOperationException
  }

  def mayReduce[B >: A](op: (B, B) => B): Maybe[B] = {
    val f = new GeneralTraverserOps.ReduceLeft(op)
    __.traverse(f)
    if (f.isDefined) Bind(f.state) else Trap
  }

  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    val f = new GeneralTraverserOps.FoldLeft(z)(op)
    __.traverse(f)
    f.state
  }

  def reduceLeft[B >: A](op: (B, A) => B): B = {
    val f = new GeneralTraverserOps.ReduceLeft(op)
    __.traverse(f)
    if (f.isDefined) f.state else throw new UnsupportedOperationException
  }

  def mayReduceLeft[B >: A](op: (B, A) => B): Maybe[B] = {
    val f = new GeneralTraverserOps.ReduceLeft(op)
    __.traverse(f)
    if (f.isDefined) Bind(f.state) else Trap
  }

  def find(p: A => Boolean): Maybe[A] = {
    val f = new GeneralTraverserOps.Find(p)
    begin { __.traverse(f) }
    f.state
  }

  def forall(p: A => Boolean): Boolean = {
    val f = new GeneralTraverserOps.Forall(p)
    begin { __.traverse(f) }
    f.state
  }

  def exists(p: A => Boolean): Boolean = {
    val f = new GeneralTraverserOps.Exists(p)
    begin { __.traverse(f) }
    f.state
  }

  def count(p: A => Boolean): Int = {
    val f = new GeneralTraverserOps.Count(p)
    __.traverse(f)
    f.state
  }

  def choose[B](q: PartialFunction[A, B]): Maybe[B] = {
    val f = new GeneralTraverserOps.Choose(q)
    begin { __.traverse(f) }
    f.state
  }

  def joinString(open: String, separator: String, close: String)(implicit builder: StringBuilder): builder.State = {
    val f = new GeneralTraverserOps.JoinString(separator)(builder)
    builder.append(open)
    __.traverse(f)
    builder.append(close)
    builder.state
  }

  def joinString(separator: String)(implicit builder: StringBuilder): builder.State = {
    val f = new GeneralTraverserOps.JoinString(separator)(builder)
    __.traverse(f)
    builder.state
  }

  def eagerly: StrictTraverserOps[A, Traverser[_]] =
    new StrictTraverserOps[A, Traverser[_]](__)

  def lazily: NonStrictTraverserOps[A] =
    new NonStrictTraverserOps[A](__)
}

private[sequential] object GeneralTraverserOps {
  import scala.runtime._

  final class Foreach[-A, +U](f: A => U) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = {
      f(x)
      ()
    }
  }

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
    private[this] var r: Maybe[A] = Trap
    override def apply(x: A): Unit = if (p(x)) { r = Bind(x); begin.break() }
    def state: Maybe[A] = r
  }

  final class Forall[A](p: A => Boolean) extends AbstractFunction1[A, Unit] {
    private[this] var r: Boolean = true
    override def apply(x: A): Unit = if (!p(x)) { r = false; begin.break() }
    def state: Boolean = r
  }

  final class Exists[A](p: A => Boolean) extends AbstractFunction1[A, Unit] {
    private[this] var r: Boolean = false
    override def apply(x: A): Unit = if (p(x)) { r = true; begin.break() }
    def state: Boolean = r
  }

  final class Count[A](p: A => Boolean) extends AbstractFunction1[A, Unit] {
    private[this] var t: Int = 0
    override def apply(x: A): Unit = if (p(x)) t += 1
    def state: Int = t
  }

  final class Choose[-A, +B](q: PartialFunction[A, B]) extends AbstractFunction1[A, Unit] {
    private[this] var r: Maybe[B] = Trap
    override def apply(x: A): Unit = if (q.isDefinedAt(x)) { r = Bind(q(x)); begin.break() }
    def state: Maybe[B] = r
  }

  final class JoinString[-A](s: String)(implicit b: StringBuilder) extends AbstractFunction1[A, Unit] {
    private[this] var r: Boolean = false
    override def apply(x: A): Unit = { if (r) b.append(s) else r = true; b.show(x) }
  }
}
