/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

final class IteratorOps[From, A](val __ : Iterator[A]) extends AnyVal {
  import __.{hasNext, next}
  
  @inline def select[B](q: PartialFunction[A, B]): Option[B] = {
    while (hasNext) { val x = next(); if (q.isDefinedAt(x)) return Some(q(x)) }
    None
  }
  
  @inline def fold[B >: A](z: B)(op: (B, B) => B): B = {
    var result = z
    while (hasNext) result = op(result, next())
    result
  }
  
  @inline def reduce[B >: A](op: (B, B) => B): B = {
    if (!hasNext) throw new UnsupportedOperationException
    var result: B = next()
    while (hasNext) result = op(result, next())
    result
  }
  
  @inline def reduceOption[B >: A](op: (B, B) => B): Option[B] = {
    if (!hasNext) return None
    var result: B = next()
    while (hasNext) result = op(result, next())
    Some(result)
  }
  
  @inline def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var result = z
    while (hasNext) result = op(result, next())
    result
  }
  
  @inline def reduceLeft[B >: A](op: (B, A) => B): B = {
    if (!hasNext) throw new UnsupportedOperationException
    var result: B = next()
    while (hasNext) result = op(result, next())
    result
  }
  
  @inline def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = {
    if (!hasNext) return None
    var result: B = next()
    while (hasNext) result = op(result, next())
    Some(result)
  }
  
  @inline def find(p: A => Boolean): Option[A] = {
    while (hasNext) { val x = next(); if (p(x)) return Some(x) }
    None
  }
  
  @inline def forall(p: A => Boolean): Boolean = {
    while (hasNext) if (!p(next())) return false
    true
  }
  
  @inline def exists(p: A => Boolean): Boolean = {
    while (hasNext) if (p(next())) return true
    false
  }
  
  @inline def count(p: A => Boolean): Int = {
    var total = 0
    while (hasNext) if (p(next())) total += 1
    total
  }
  
  def map[B](f: A => B): Iterator[B] = new IteratorOps.Map(__, f)
  
  def flatMap[B](f: A => Iterator[B]): Iterator[B] = new IteratorOps.FlatMap(__, f)
  
  def filter(p: A => Boolean): Iterator[A] = new IteratorOps.Filter(__, p)
  
  def withFilter(p: A => Boolean): Iterator[A] = filter(p)
  
  def collect[B](q: PartialFunction[A, B]): Iterator[B] = new IteratorOps.Collect(__, q)
  
  def zip[B](that: Iterator[B]): Iterator[(A, B)] = new IteratorOps.Zip(__, that)
  
  def dropWhile(p: A => Boolean): Iterator[A] = new IteratorOps.DropWhile(__, p)
  
  def takeWhile(p: A => Boolean): Iterator[A] = new IteratorOps.TakeWhile(__, p)
  
  def drop(lower: Int): Iterator[A] = new IteratorOps.Drop(__, lower)
  
  def take(upper: Int): Iterator[A] = new IteratorOps.Take(__, upper)
  
  def slice(lower: Int, upper: Int): Iterator[A] = new IteratorOps.Slice(__, lower, upper)
}

private[collection] object IteratorOps {
  import scala.annotation.tailrec
  
  final class Map[-A, +B](self: Iterator[A], f: A => B) extends Iterator[B] {
    override def hasNext: Boolean = self.hasNext
    override def next(): B = f(self.next())
  }
  
  final class FlatMap[-A, +B](self: Iterator[A], f: A => Iterator[B]) extends Iterator[B] {
    private[this] var these: Iterator[B] = Iterator.Empty
    @tailrec override def hasNext: Boolean = these.hasNext || self.hasNext && { these = f(self.next()); hasNext }
    override def next(): B = (if (hasNext) these else Iterator.Empty).next()
  }
  
  final class Filter[+A](self: Iterator[A], p: A => Boolean) extends Iterator[A] {
    private[this] var head: A = _
    private[this] var isDefined: Boolean = false
    @tailrec override def hasNext: Boolean = isDefined || self.hasNext && { head = self.next(); isDefined = p(head); hasNext }
    override def next(): A = if (hasNext) { val x = head; head = null.asInstanceOf[A]; isDefined = false; x } else Iterator.Empty.next()
  }
  
  final class Collect[-A, +B](self: Iterator[A], q: PartialFunction[A, B]) extends Iterator[B] {
    private[this] var head: A = _
    private[this] var isDefined: Boolean = false
    @tailrec override def hasNext: Boolean = isDefined || self.hasNext && { head = self.next(); isDefined = q.isDefinedAt(head); hasNext }
    override def next(): B = if (hasNext) { val x = head; head = null.asInstanceOf[A]; isDefined = false; q(x) } else Iterator.Empty.next()
  }
  
  final class Zip[+A, +B](these: Iterator[A], those: Iterator[B]) extends Iterator[(A, B)] {
    override def hasNext: Boolean = these.hasNext && those.hasNext
    override def next(): (A, B) = (these.next(), those.next())
  }
  
  final class DropWhile[+A](self: Iterator[A], p: A => Boolean) extends Iterator[A] {
    private[this] var head: A = _
    private[this] var begun: Boolean = false
    private[this] var isDefined: Boolean = true
    @tailrec override def hasNext: Boolean =
      begun && (isDefined || self.hasNext && { head = self.next(); isDefined = true; true }) ||
      self.hasNext && { head = self.next(); begun = !p(head); hasNext }
    override def next(): A = if (hasNext) { val x = head; head = null.asInstanceOf[A]; isDefined = false; x } else Iterator.Empty.next()
  }
  
  final class TakeWhile[+A](self: Iterator[A], p: A => Boolean) extends Iterator[A] {
    private[this] var head: A = _
    private[this] var done: Boolean = false
    private[this] var isDefined: Boolean = false
    override def hasNext: Boolean =
      !done && (isDefined || self.hasNext && {
        head = self.next(); isDefined = p(head); isDefined || { head = null.asInstanceOf[A]; done = true; false }
      })
    override def next(): A = if (hasNext) { val x = head; head = null.asInstanceOf[A]; isDefined = false; x } else Iterator.Empty.next()
  }
  
  final class Drop[+A](self: Iterator[A], lower: Int) extends Iterator[A] {
    private[this] var index: Int = 0
    @tailrec override def hasNext: Boolean = self.hasNext && (index >= lower || { self.next(); index += 1; hasNext })
    override def next(): A = (if (hasNext) self else Iterator.Empty).next()
  }
  
  final class Take[+A](self: Iterator[A], upper: Int) extends Iterator[A] {
    private[this] var index: Int = 0
    override def hasNext: Boolean = index < upper && self.hasNext
    override def next(): A = (if (hasNext) { index += 1; self } else Iterator.Empty).next()
  }
  
  final class Slice[+A](self: Iterator[A], lower: Int, upper: Int) extends Iterator[A] {
    private[this] val start: Int = math.max(0, lower)
    private[this] val until: Int = math.max(start, upper)
    private[this] var index: Int = 0
    @tailrec override def hasNext: Boolean = self.hasNext && index >= lower && index < upper || { self.next(); index += 1; hasNext }
    override def next(): A = (if (hasNext) { index += 1; self } else Iterator.Empty).next()
  }
}
