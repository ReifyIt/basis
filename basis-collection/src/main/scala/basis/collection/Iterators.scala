/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

import basis._

private[basis] object Iterators {
  import scala.annotation.tailrec
  
  object Empty extends Iterator[Nothing] {
    override def hasNext: Boolean = false
    
    override def next: Nothing = throw new scala.NoSuchElementException("empty iterator")
  }
  
  final class Collect[-A, +B](self: Iterator[A], q: scala.PartialFunction[A, B]) extends Iterator[B] {
    private[this] var head: A = _
    private[this] var isDefined: Boolean = false
    
    @tailrec override def hasNext: Boolean =
      isDefined || (self.hasNext && { head = self.next(); isDefined = q.isDefinedAt(head); hasNext })
    
    @tailrec override def next(): B = {
      if (isDefined) { val x = head; head = null.asInstanceOf[A]; isDefined = false; q(x) }
      else if (self.hasNext) { head = self.next(); isDefined = q.isDefinedAt(head); next() }
      else Empty.next()
    }
  }
  
  final class Map[-A, +B](self: Iterator[A], f: A => B) extends Iterator[B] {
    override def hasNext: Boolean = self.hasNext
    override def next(): B = f(self.next())
  }
  
  final class FlatMap[-A, +B](self: Iterator[A], f: A => Iterator[B]) extends Iterator[B] {
    private[this] var these: Iterator[B] = Empty
    
    @tailrec override def hasNext: Boolean =
      these.hasNext || (self.hasNext && { these = f(self.next()); hasNext })
    
    @tailrec override def next(): B = {
      if (these.hasNext) these.next()
      else if (self.hasNext) { these = f(self.next()); next() }
      else Empty.next()
    }
  }
  
  final class FlatMapContainer[-A, +B](self: Iterator[A], f: A => basis.Container[B]) extends Iterator[B] {
    private[this] var these: Iterator[B] = Empty
    
    @tailrec override def hasNext: Boolean =
      these.hasNext || (self.hasNext && { these = f(self.next()).iterator; hasNext })
    
    @tailrec override def next(): B = {
      if (these.hasNext) these.next()
      else if (self.hasNext) { these = f(self.next()).iterator; next() }
      else Empty.next()
    }
  }
  
  final class Filter[+A](self: Iterator[A], p: A => Boolean) extends Iterator[A] {
    private[this] var head: A = _
    private[this] var isDefined: Boolean = false
    
    @tailrec override def hasNext: Boolean =
      isDefined || (self.hasNext && { head = self.next(); isDefined = p(head); hasNext })
    
    @tailrec override def next(): A = {
      if (isDefined) { val x = head; head = null.asInstanceOf[A]; isDefined = false; x }
      else if (self.hasNext) { head = self.next(); isDefined = p(head); next() }
      else Empty.next()
    }
  }
  
  final class DropWhile[+A](self: Iterator[A], p: A => Boolean) extends Iterator[A] {
    private[this] var head: A = _
    private[this] var state: Int = 0 // 0 = drop; 1 = buffer; 2 = pass
    
    @tailrec override def hasNext: Boolean = state match {
      case 0 => if (self.hasNext) { val x = self.next(); if (p(x)) hasNext else { head = x; state = 1; true } }
                else { state = 2; false }
      case 1 => true
      case 2 => self.hasNext
    }
    
    @tailrec override def next(): A = state match {
      case 0 => if (self.hasNext) { val x = self.next(); if (p(x)) next() else { state = 2; x } }
                else { state = 2; next() }
      case 1 => val x = head; head = null.asInstanceOf[A]; state = 2; x
      case 2 => self.next()
    }
  }
  
  final class TakeWhile[+A](self: Iterator[A], p: A => Boolean) extends Iterator[A] {
    private[this] var head: A = _
    private[this] var state: Int = 0 // 0 = take; 1 = buffer; 2 = done
    
    override def hasNext: Boolean = state match {
      case 0 => (self.hasNext && { val x = self.next(); p(x) && { head = x; state = 1; true } }) || { state = 2; false }
      case 1 => true
      case 2 => false
    }
    
    @tailrec override def next(): A = state match {
      case 0 => if (self.hasNext) { val x = self.next(); if (p(x)) x else { state = 2; next() } }
                else { state = 2; next() }
      case 1 => val x = head; head = null.asInstanceOf[A]; state = 0; x
      case 2 => Empty.next()
    }
  }
  
  final class Drop[+A](self: Iterator[A], lower: Int) extends Iterator[A] {
    private[this] var index: Int = 0
    
    @tailrec override def hasNext: Boolean =
      self.hasNext && (index >= lower || { self.next(); index += 1; hasNext })
    
    @tailrec override def next(): A = {
      if (index < lower) { self.next(); index += 1; next() }
      else self.next()
    }
  }
  
  final class Take[+A](self: Iterator[A], upper: Int) extends Iterator[A] {
    private[this] var index: Int = 0
    
    override def hasNext: Boolean =
      index < upper && self.hasNext
    
    override def next(): A = {
      if (index < upper) { val x = self.next(); index += 1; x }
      else Empty.next()
    }
  }
  
  final class Slice[+A](self: Iterator[A], lower: Int, upper: Int) extends Iterator[A] {
    private[this] val start: Int = scala.math.max(0, lower)
    private[this] val until: Int = scala.math.max(start, upper)
    private[this] var index: Int = 0
    
    @tailrec override def hasNext: Boolean =
      (self.hasNext && index >= start && index < until) || { self.next(); index += 1; hasNext }
    
    @tailrec override def next(): A = {
      if (index < start) { self.next(); index += 1; next() }
      else if (index < until) { val x = self.next(); index += 1; x }
      else Empty.next()
    }
  }
  
  final class Zip[+A, +B](these: Iterator[A], those: Iterator[B]) extends Iterator[(A, B)] {
    override def hasNext: Boolean = these.hasNext && those.hasNext
    override def next(): (A, B) = (these.next(), those.next())
  }
  
  final class ++ [+A](these: Iterator[A], those: Iterator[A]) extends Iterator[A] {
    private[this] var segment: Int = 0
    
    @tailrec override def hasNext: Boolean = segment match {
      case 0 => these.hasNext || { segment = 1; hasNext }
      case 1 => those.hasNext
    }
    
    @tailrec override def next(): A = segment match {
      case 0 => if (these.hasNext) these.next() else { segment = 1; next() }
      case 1 => those.next()
    }
  }
}
