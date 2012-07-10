/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Iterator[+A] extends Any with Incremental[A] {
  import Iterator._
  
  def hasNext: Boolean
  
  def next(): A
  
  override def foreach[U](f: A => U): Unit = while (hasNext) f(next())
  
  override def collectFirst[B](q: PartialFunction[A, B]): Option[B] = {
    var result = None: Option[B]
    while (result.isEmpty && hasNext) {
      val head = next()
      if (q.isDefinedAt(head)) result = Some(q(head))
    }
    result
  }
  
  override def fold[B >: A](z: B)(op: (B, B) => B): B = foldLeft(z)(op)
  
  override def reduce[B >: A](op: (B, B) => B): B = reduceLeft(op)
  
  override def reduceOption[B >: A](op: (B, B) => B): Option[B] = reduceLeftOption(op)
  
  override def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var result = z
    while (hasNext) result = op(result, next())
    result
  }
  
  override def reduceLeft[B >: A](op: (B, A) => B): B = {
    var result = next(): B
    while (hasNext) result = op(result, next())
    result
  }
  
  override def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = {
    if (hasNext) {
      var result = next(): B
      while (hasNext) result = op(result, next())
      Some(result)
    }
    else None
  }
  
  override def find(p: A => Boolean): Option[A] = {
    while (hasNext) {
      val x = next()
      if (p(x)) return Some(x)
    }
    None
  }
  
  override def forall(p: A => Boolean): Boolean = {
    while (hasNext) if (!p(next())) return false
    true
  }
  
  override def exists(p: A => Boolean): Boolean = {
    while (hasNext) if (p(next())) return true
    false
  }
  
  override def count(p: A => Boolean): Int = {
    var total = 0
    while (hasNext) if (p(next())) total += 1
    total
  }
  
  def map[B](f: A => B): Iterator[B] = new Map[A, B](this, f)
  
  def flatMap[B](f: A => Iterator[B]): Iterator[B] = new FlatMap[A, B](this, f)
  
  def filter(p: A => Boolean): Iterator[A] = new Filter[A](this, p)
  
  def withFilter(p: A => Boolean): Iterator[A] = filter(p)
  
  def collect[B](q: PartialFunction[A, B]): Iterator[B] = new Collect[A, B](this, q)
  
  def drop(lower: Int): Iterator[A] = new Drop[A](this, lower)
  
  def take(upper: Int): Iterator[A] = new Take[A](this, upper)
  
  def slice(lower: Int, upper: Int): Iterator[A] = new Slice[A](this, lower, upper)
  
  def zip[B](that: Iterator[B]): Iterator[(A, B)] = new Zip[A, B](this, that)
}

object Iterator {
  abstract class Abstract[+A] extends Iterator[A]
  
  object Empty extends Iterator[Nothing] {
    override def hasNext: Boolean = false
    override def next(): Nothing = throw new NoSuchElementException("empty iterator")
  }
  
  final class Map[-A, +B](self: Iterator[A], f: A => B) extends Abstract[B] {
    override def hasNext: Boolean = self.hasNext
    override def next(): B = f(self.next())
  }
  
  final class FlatMap[-A, +B](self: Iterator[A], f: A => Iterator[B]) extends Abstract[B] {
    private[this] var current: Iterator[B] = Empty
    override def hasNext: Boolean = {
      while (!current.hasNext && self.hasNext) current = f(self.next())
      current.hasNext
    }
    override def next(): B = {
      while (!current.hasNext && self.hasNext) current = f(self.next())
      current.next()
    }
  }
  
  final class Filter[+A](self: Iterator[A], p: A => Boolean) extends Abstract[A] {
    private[this] var head: A = _
    private[this] var isDefined: Boolean = false
    override def hasNext: Boolean = {
      while (!isDefined && self.hasNext) {
        head = self.next()
        if (p(head)) isDefined = true
      }
      isDefined
    }
    override def next(): A = {
      if (hasNext) {
        val item = head
        head = null.asInstanceOf[A]
        isDefined = false
        item
      }
      else Empty.next()
    }
  }
  
  final class Collect[-A, +B](self: Iterator[A], q: PartialFunction[A, B]) extends Abstract[B] {
    private[this] var head: A = _
    private[this] var isDefined: Boolean = false
    override def hasNext: Boolean = {
      while (!isDefined && self.hasNext) {
        head = self.next()
        if (q.isDefinedAt(head)) isDefined = true
      }
      isDefined
    }
    override def next(): B = {
      if (hasNext) {
        val item = head
        head = null.asInstanceOf[A]
        isDefined = false
        q(item)
      }
      else Empty.next()
    }
  }
  
  final class Drop[+A](self: Iterator[A], lower: Int) extends Abstract[A] {
    private[this] var index: Int = 0
    override def hasNext: Boolean = {
      while (index < lower && self.hasNext) { index += 1; self.next() }
      self.hasNext
    }
    override def next(): A = (if (hasNext) self else Empty).next()
  }
  
  final class Take[+A](self: Iterator[A], upper: Int) extends Abstract[A] {
    private[this] var index: Int = 0
    override def hasNext: Boolean = index < upper && self.hasNext
    override def next(): A = (if (hasNext) { index += 1; self } else Empty).next()
  }
  
  final class Slice[+A](self: Iterator[A], lower: Int, upper: Int) extends Abstract[A] {
    private[this] val start: Int = math.max(0, lower)
    private[this] val until: Int = math.max(start, upper)
    private[this] var index: Int = 0
    override def hasNext: Boolean = {
      while (index < start && self.hasNext) { index += 1; self.next() }
      index < upper && self.hasNext
    }
    override def next(): A = (if (hasNext) { index += 1; self } else Empty).next()
  }
  
  final class Zip[+A, +B](self: Iterator[A], that: Iterator[B]) extends Abstract[(A, B)] {
    override def hasNext: Boolean = self.hasNext && that.hasNext
    override def next(): (A, B) = (self.next(), that.next())
  }
}
