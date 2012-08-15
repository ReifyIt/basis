/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

final class TraversableOps[Kind, A](val __ : Traversable[A]) extends AnyVal {
  import __.foreach
  
  def select[B](q: PartialFunction[A, B]): Option[B] = {
    val f = new TraversableOps.Select(q)
    try foreach(f) catch { case e: Break => () }
    f.result
  }
  
  def fold[B >: A](z: B)(op: (B, B) => B): B = {
    val f = new TraversableOps.FoldLeft(z)(op)
    foreach(f)
    f.result
  }
  
  def reduce[B >: A](op: (B, B) => B): B = {
    val f = new TraversableOps.ReduceLeft(op)
    foreach(f)
    if (f.isDefined) f.result else throw new UnsupportedOperationException
  }
  
  def reduceOption[B >: A](op: (B, B) => B): Option[B] = {
    val f = new TraversableOps.ReduceLeft(op)
    foreach(f)
    if (f.isDefined) Some(f.result) else None
  }
  
  def find(p: A => Boolean): Option[A] = {
    val f = new TraversableOps.Find(p)
    try foreach(f) catch { case e: Break => () }
    f.result
  }
  
  def forall(p: A => Boolean): Boolean = {
    val f = new TraversableOps.Forall(p)
    try foreach(f) catch { case e: Break => () }
    f.result
  }
  
  def exists(p: A => Boolean): Boolean = {
    val f = new TraversableOps.Exists(p)
    try foreach(f) catch { case e: Break => () }
    f.result
  }
  
  def count(p: A => Boolean): Int = {
    val f = new TraversableOps.Count(p)
    foreach(f)
    f.result
  }
  
  def map[B](f: A => B)(implicit builder: Builder[Kind, B]): builder.Result = {
    foreach(new TraversableOps.AppendMap(f, builder))
    builder.result
  }
  
  def flatMap[B](f: A => Traversable[B])(implicit builder: Builder[Kind, B]): builder.Result = {
    foreach(new TraversableOps.AppendFlatMap(f, builder))
    builder.result
  }
  
  def filter(p: A => Boolean)(implicit builder: Builder[Kind, A]): builder.Result = {
    foreach(new TraversableOps.AppendFilter(p, builder))
    builder.result
  }
  
  def withFilter(p: A => Boolean): Traversable[A] = new TraversableOps.WithFilter(__, p)
  
  def collect[B](q: PartialFunction[A, B])(implicit builder: Builder[Kind, B]): builder.Result = {
    foreach(new TraversableOps.AppendCollect(q, builder))
    builder.result
  }
}

private[collection] object TraversableOps {
  import scala.runtime.AbstractFunction1
  
  final class WithFilter[+A](self: Traversable[A], p: A => Boolean) extends Traversable[A] {
    override def foreach[U](f: A => U): Unit = self.foreach(new Filter(f, p))
  }
  
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
  
  final class FlatMap[-A, +B, +U](f: B => U, g: A => Traversable[B]) extends AbstractFunction1[A, Unit] {
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
  
  final class Append[-A](builder: Builder[_, A]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = builder += x
  }
  
  final class AppendMap[-A, +B](f: A => B, builder: Builder[_, B]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = builder += f(x)
  }
  
  final class AppendFlatMap[-A, +B](f: A => Traversable[B], builder: Builder[_, B]) extends AbstractFunction1[A, Unit] {
    private[this] val append = new Append(builder)
    override def apply(x: A): Unit = f(x).foreach(append)
  }
  
  final class AppendFilter[-A](p: A => Boolean, builder: Builder[_, A]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (p(x)) builder += x
  }
  
  final class AppendCollect[-A, B](q: PartialFunction[A, B], builder: Builder[_, B]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (q.isDefinedAt(x)) builder += q(x)
  }
}
