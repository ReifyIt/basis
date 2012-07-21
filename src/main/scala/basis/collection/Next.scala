/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Next[+A] extends Any with Once[A] {
  def hasNext: Boolean
  
  def next(): A
  
  @inline final override def foreach[U](f: A => U): Unit = while (hasNext) f(next())
}

object Next {
  import scala.annotation.tailrec
  import scala.language.implicitConversions
  
  object Nada extends Next[Nothing] {
    override def hasNext: Boolean = false
    override def next(): Nothing = throw new NoSuchElementException("next to nada")
  }
  
  @inline implicit def ForNext[A](self: Next[A]): ForNext[self.From, A] =
    new ForNext[self.From, A](self)
  
  final class ForNext[From, A](val __ : Next[A]) extends AnyVal {
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
    
    def map[B](f: A => B): Next[B] = new Map(__, f)
    
    def flatMap[B](f: A => Next[B]): Next[B] = new FlatMap(__, f)
    
    def filter(p: A => Boolean): Next[A] = new Filter(__, p)
    
    def withFilter(p: A => Boolean): Next[A] = filter(p)
    
    def collect[B](q: PartialFunction[A, B]): Next[B] = new Collect(__, q)
    
    def zip[B](that: Next[B]): Next[(A, B)] = new Zip(__, that)
    
    def dropWhile(p: A => Boolean): Next[A] = new DropWhile(__, p)
    
    def takeWhile(p: A => Boolean): Next[A] = new TakeWhile(__, p)
    
    def drop(lower: Int): Next[A] = new Drop(__, lower)
    
    def take(upper: Int): Next[A] = new Take(__, upper)
    
    def slice(lower: Int, upper: Int): Next[A] = new Slice(__, lower, upper)
  }
  
  private[basis] final class Map[-A, +B](self: Next[A], f: A => B) extends Next[B] {
    override def hasNext: Boolean = self.hasNext
    override def next(): B = f(self.next())
  }
  
  private[basis] final class FlatMap[-A, +B](self: Next[A], f: A => Next[B]) extends Next[B] {
    private[this] var these: Next[B] = Nada
    @tailrec override def hasNext: Boolean = these.hasNext || self.hasNext && { these = f(self.next()); hasNext }
    override def next(): B = (if (hasNext) these else Nada).next()
  }
  
  private[basis] final class Filter[+A](self: Next[A], p: A => Boolean) extends Next[A] {
    private[this] var head: A = _
    private[this] var isDefined: Boolean = false
    @tailrec override def hasNext: Boolean = isDefined || self.hasNext && { head = self.next(); isDefined = p(head); hasNext }
    override def next(): A = if (hasNext) { val x = head; head = null.asInstanceOf[A]; isDefined = false; x } else Nada.next()
  }
  
  private[basis] final class Collect[-A, +B](self: Next[A], q: PartialFunction[A, B]) extends Next[B] {
    private[this] var head: A = _
    private[this] var isDefined: Boolean = false
    @tailrec override def hasNext: Boolean = isDefined || self.hasNext && { head = self.next(); isDefined = q.isDefinedAt(head); hasNext }
    override def next(): B = if (hasNext) { val x = head; head = null.asInstanceOf[A]; isDefined = false; q(x) } else Nada.next()
  }
  
  private[basis] final class Zip[+A, +B](these: Next[A], those: Next[B]) extends Next[(A, B)] {
    override def hasNext: Boolean = these.hasNext && those.hasNext
    override def next(): (A, B) = (these.next(), those.next())
  }
  
  private[basis] final class DropWhile[+A](self: Next[A], p: A => Boolean) extends Next[A] {
    private[this] var head: A = _
    private[this] var begun: Boolean = false
    private[this] var isDefined: Boolean = true
    @tailrec override def hasNext: Boolean =
      begun && (isDefined || self.hasNext && { head = self.next(); isDefined = true; true }) ||
      self.hasNext && { head = self.next(); begun = !p(head); hasNext }
    override def next(): A = if (hasNext) { val x = head; head = null.asInstanceOf[A]; isDefined = false; x } else Nada.next()
  }
  
  private[basis] final class TakeWhile[+A](self: Next[A], p: A => Boolean) extends Next[A] {
    private[this] var head: A = _
    private[this] var done: Boolean = false
    private[this] var isDefined: Boolean = false
    override def hasNext: Boolean =
      !done && (isDefined || self.hasNext && {
        head = self.next(); isDefined = p(head); isDefined || { head = null.asInstanceOf[A]; done = true; false }
      })
    override def next(): A = if (hasNext) { val x = head; head = null.asInstanceOf[A]; isDefined = false; x } else Nada.next()
  }
  
  private[basis] final class Drop[+A](self: Next[A], lower: Int) extends Next[A] {
    private[this] var index: Int = 0
    @tailrec override def hasNext: Boolean = self.hasNext && (index >= lower || { self.next(); index += 1; hasNext })
    override def next(): A = (if (hasNext) self else Nada).next()
  }
  
  private[basis] final class Take[+A](self: Next[A], upper: Int) extends Next[A] {
    private[this] var index: Int = 0
    override def hasNext: Boolean = index < upper && self.hasNext
    override def next(): A = (if (hasNext) { index += 1; self } else Nada).next()
  }
  
  private[basis] final class Slice[+A](self: Next[A], lower: Int, upper: Int) extends Next[A] {
    private[this] val start: Int = math.max(0, lower)
    private[this] val until: Int = math.max(start, upper)
    private[this] var index: Int = 0
    @tailrec override def hasNext: Boolean = self.hasNext && index >= lower && index < upper || { self.next(); index += 1; hasNext }
    override def next(): A = (if (hasNext) { index += 1; self } else Nada).next()
  }
}
