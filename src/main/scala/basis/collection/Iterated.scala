/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Iterated[+A] extends Any with Incremental[A] { self =>
  def hasNext: Boolean
  
  def next(): A
  
  override def foreach[U](f: A => U): Unit = while (hasNext) f(next())
  
  def map[B](f: A => B): Iterated[B] = new Mapped[B](f)
  
  def flatMap[B](f: A => Iterated[B]): Iterated[B] = new FlatMapped[B](f)
  
  def filter(p: A => Boolean): Iterated[A] = new Filtered(p)
  
  def withFilter(p: A => Boolean): Iterated[A] = filter(p)
  
  def collect[B](q: PartialFunction[A, B]): Iterated[B] = new Collected[B](q)
  
  override def collectFirst[B](q: PartialFunction[A, B]): Option[B] = {
    var result = None: Option[B]
    while (result.isEmpty && hasNext) {
      val x = next()
      if (q.isDefinedAt(x)) result = Some(q(x))
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
    if (!hasNext) throw new UnsupportedOperationException("empty reduceLeft")
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
  
  def drop(lower: Int): Iterated[A] = new Dropped(lower)
  
  def take(upper: Int): Iterated[A] = new Taken(upper)
  
  def slice(lower: Int, upper: Int): Iterated[A] = new Sliced(lower, upper)
  
  def zip[B](that: Iterated[B]): Iterated[(A, B)] = new Zip[B](that)
  
  private final class Mapped[+B](f: A => B) extends AbstractIterated[B] {
    override def hasNext: Boolean = self.hasNext
    override def next(): B = f(self.next())
  }
  
  private final class FlatMapped[+B](f: A => Iterated[B]) extends AbstractIterated[B] {
    private[this] var current: Iterated[B] = null
    
    override def hasNext: Boolean = {
      while ((current == null || !current.hasNext) && self.hasNext) current = f(self.next())
      current.hasNext
    }
    
    override def next(): B = {
      if (!hasNext) throw new NoSuchElementException("empty iterator")
      current.next()
    }
  }
  
  private final class Filtered(p: A => Boolean) extends AbstractIterated[A] {
    private[this] var hasCurrent: Boolean = false
    private[this] var current: A = _
    
    override def hasNext: Boolean = {
      while (!hasCurrent && self.hasNext) {
        current = self.next()
        if (p(current)) hasCurrent = true
      }
      hasCurrent
    }
    
    override def next(): A = {
      if (!hasNext) throw new NoSuchElementException("empty iterator")
      hasCurrent = false
      current
    }
  }
  
  private final class Collected[+B](q: PartialFunction[A, B]) extends AbstractIterated[B] {
    private[this] var hasCurrent: Boolean = false
    private[this] var current: A = _
    
    override def hasNext: Boolean = {
      while (!hasCurrent && self.hasNext) {
        current = self.next()
        if (q.isDefinedAt(current)) hasCurrent = true
      }
      hasCurrent
    }
    
    override def next(): B = {
      if (!hasNext) throw new NoSuchElementException("empty iterator")
      hasCurrent = false
      q(current)
    }
  }
  
  private final class Dropped(lower: Int) extends AbstractIterated[A] {
    private[this] var index: Int = 0
    
    override def hasNext: Boolean = {
      while (index < lower && self.hasNext) { index += 1; self.next() }
      self.hasNext
    }
    
    override def next(): A = {
      if (!hasNext) throw new NoSuchElementException("empty iterator")
      index += 1
      self.next()
    }
  }
  
  private final class Taken(upper: Int) extends AbstractIterated[A] {
    private[this] var index: Int = 0
    
    override def hasNext: Boolean = index < upper && self.hasNext
    
    override def next(): A = {
      if (!hasNext) throw new NoSuchElementException("empty iterator")
      index += 1
      self.next()
    }
  }
  
  private final class Sliced(from: Int, until: Int) extends AbstractIterated[A] {
    private[this] val lower: Int = math.max(0, from)
    private[this] val upper: Int = math.max(lower, until)
    private[this] var index: Int = 0
    
    override def hasNext: Boolean = {
      while (index < lower && self.hasNext) { index += 1; self.next() }
      index < upper && self.hasNext
    }
    
    override def next(): A = {
      if (!hasNext) throw new NoSuchElementException("empty iterator")
      index += 1
      self.next()
    }
  }
  
  private final class Zip[B](that: Iterated[B]) extends AbstractIterated[(A, B)] {
    override def hasNext: Boolean = self.hasNext && that.hasNext
    override def next(): (A, B) = (self.next(), that.next())
  }
}

private[basis] abstract class AbstractIterated[+A] extends Iterated[A]
